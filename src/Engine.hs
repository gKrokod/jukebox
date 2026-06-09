module Engine where
import Hotkey.Types ( Pause(..) )
import Data.Time ( UTCTime)
import Handlers.Engine (Track(..), Library, updateTrack, formatMMSS)
import System.Process ( createProcess, terminateProcess, proc,
      CreateProcess(std_err, std_in, std_out), StdStream(NoStream) ) 
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import qualified Data.Map.Strict as Map
import Data.Time ( diffUTCTime, getCurrentTime ) 
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )
import Control.Concurrent.STM
    ( atomically, readTVar, retry, writeTVar, STM, TVar )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getLibrary :: TVar Library -> IO (Library)
getLibrary libT = do
  atomically (readTVar libT)

modifyTrack :: TVar Library -> Track -> IO ()
modifyTrack libT track = do
  time <- Data.Time.getCurrentTime
  atomically $ do 
    lib <- readTVar libT
    let newTrack = updateTrack time track
        newLib = Map.insert (newTrack.path) newTrack lib
    writeTVar libT newLib
--
saveDataBaseToFile :: FilePath -> TVar Library -> IO ()
saveDataBaseToFile file libT = do
  lib <- atomically (readTVar libT)
  BL.writeFile file (encode lib)

playTrackSTM :: TVar Pause -> TVar Double -> Track -> IO ()
playTrackSTM pause offset track = do
    atomically $ do
      p <- readTVar pause
      case p of
        On -> retry       -- ждать, пока TVar pause изменится
        _  -> pure ()     -- Off или Next — можно продолжать
  
 -- p <- atomically $ readTVar pause
 -- if p == On then playTrackSTM pause offset track
 -- else do
    offsetStart <- atomically $ readTVar offset
    timeStart <- Data.Time.getCurrentTime
    TIO.putStrLn $ "Debug Duration: " <> formatMMSS track.duration
              -- ("Длительность: " <> formatMMSS t.duration <> ", Интервал: " <> T.pack (show t.interval) <> ", Следует прослушать: " <> T.pack (show t.planPlay))
    putStrLn $ "Debug Offset: " <> show offsetStart
    putStrLn $ "Debug TimeStart: " <> show timeStart
    (_, _, _, ph) <-
      createProcess (proc "ffplay"
        [ "-nodisp"
        , "-autoexit"
        , "-ss", show offsetStart 
        , "-loglevel", "quiet"
        , T.unpack track.path
        ])
        { std_in  = NoStream
        , std_out = NoStream
        , std_err = NoStream
        }
    let timeLeft = max 0 (fromIntegral track.duration - (ceiling $ offsetStart))

    timeout <- race (threadDelay (timeLeft * 1000)) (pressPauseNext pause)
    case timeout of
      Right (Right timePause) -> do 
        terminateProcess ph
        let offset' = (offsetStart + deltaOffset timeStart timePause)
        if offset' >= fromIntegral track.duration then do
          atomically $ writeTVar offset 0
        else do
          atomically $ writeTVar offset offset'
          playTrackSTM pause offset track
      _ -> do 
        terminateProcess ph
        atomically $ writeTVar offset 0 

data Next

deltaOffset :: Data.Time.UTCTime -> Data.Time.UTCTime -> Double
deltaOffset start end = realToFrac $ Data.Time.diffUTCTime end start

pressPauseNext :: TVar Pause -> IO (Either Next Data.Time.UTCTime)
pressPauseNext pause = do
  status <- atomically $ pressPauseOrNext pause
  time <- Data.Time.getCurrentTime
  case status of
    Left _ -> do
               atomically $ writeTVar pause Off  --чтобы после некст начинало играть без паузу 
               pure $ Left $ error "press next" 
    Right _ -> pure $ Right time

pressPauseOrNext :: TVar Pause -> STM (Either Next Data.Time.UTCTime)
pressPauseOrNext pause = do
  statusPause <- readTVar pause
  case statusPause of
    On -> pure $ Right $ error "any value for UTCTime"
    Next -> pure $ Left $ error "press Next"
    Off -> retry

