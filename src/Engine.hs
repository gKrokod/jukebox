module Engine where
import Hotkey.Types ( Pause(..) )
import Data.Time ( UTCTime)
import Handlers.Engine (Track(..), Library)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import qualified Data.Map.Strict as Map
import Data.Time ( diffUTCTime, getCurrentTime ) 
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )
import Control.Concurrent.STM
    ( atomically, readTVar, retry, writeTVar, STM, TVar )
import qualified Data.Text as T
import PlayerState
import System.Process
import Control.Exception (bracket)
import Control.Monad
import Handlers.LogicMemo (updateTrack)

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

newtype OffsetStart = OffsetStart String
acquireFFplay :: OffsetStart -> FilePath -> IO (ProcessHandle)
acquireFFplay (OffsetStart offset) path = do
    (_, _, _, ph) <-
      createProcess (proc "ffplay"
        [ "-nodisp"
        , "-autoexit"
        , "-ss", offset
        , "-loglevel", "quiet"
        , path
        ])
        { std_in  = NoStream
        , std_out = NoStream
        , std_err = NoStream
        }
    pure ph

releaseFFplay :: FFPlay -> ProcessHandle -> IO ()
releaseFFplay state ph = do
  terminateProcess ph
  atomically $ writeTVar state.ph Nothing
  void $ waitForProcess ph
  

data PlayStatus = PlayDone | PlayPause


useFFplay :: TVar Pause -> FFPlay -> Track -> ProcessHandle -> IO (PlayStatus)
useFFplay pause state track ph = do
  offsetStart <- atomically $ do
    writeTVar state.ph (Just ph) -- ph from acuireFFplay
    readTVar state.offset

  timeStart <- Data.Time.getCurrentTime
  let timeLeft = max 0 (fromIntegral track.duration - (ceiling $ offsetStart))

  timeout <- race (threadDelay (timeLeft * 1000)) (pressPauseNext pause)

  case timeout of
    Right (Right timePause) -> do 
      let offset' = (offsetStart + deltaOffset timeStart timePause)
      if offset' >= fromIntegral track.duration then do atomically $ writeTVar state.offset 0 >> pure PlayDone
      else do
        atomically $ writeTVar state.offset offset'
        pure PlayPause
    _ -> do 
      atomically $ writeTVar state.offset 0
      pure PlayDone
      
playTrackSTM :: TVar Pause -> FFPlay -> Track -> IO ()
playTrackSTM pause state track = do
    atomically $ do
      p <- readTVar pause
      case p of
        On -> retry       -- ждать, пока TVar pause изменится
        _  -> pure ()     -- Off или Next — можно продолжать

    offsetStart <- atomically $ readTVar state.offset
    timeStart <- Data.Time.getCurrentTime
    putStrLn $ "Debug Offset: " <> show offsetStart
    putStrLn $ "Debug TimeStart: " <> show timeStart
    playStatus <- bracket
                    (acquireFFplay (OffsetStart . show $ offsetStart) (T.unpack track.path))
                    (releaseFFplay state)
                    (useFFplay pause state track)
    case playStatus of
      PlayDone -> pure ()
      PlayPause -> playTrackSTM pause state track


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

