module Engine where
import Hotkey.Types ( Pause(..) )
import Control.Exception (SomeException, displayException, throwIO, try)
import Data.Time ( UTCTime, addUTCTime )
import Handlers.Engine (Track(..), Library, updateTrack)
import System.Process
    ( createProcess, terminateProcess,
      proc,
      CreateProcess(std_err, std_in, std_out),
      StdStream(NoStream) ) 
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ( (</>), takeExtension )
import System.OsPath (encodeUtf)
import Monatone.Common  (parseMetadata)
import Monatone.Metadata  (Metadata(..), AudioProperties (duration))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, eitherDecode)
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as Map
import Data.Time ( diffUTCTime, getCurrentTime ) 
import Control.Monad (filterM)
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )
import Control.Concurrent.STM
    ( atomically, newTVarIO, readTVar, retry, writeTVar, STM, TVar )
import qualified Data.Text as T

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

initLibrary :: FilePath -> FilePath -> IO (TVar Library)
initLibrary dir file = do
  library <- migration dir file
  cleanUpLibrary <- Map.fromList <$> filterM (doesFileExist . T.unpack . fst) (Map.toList  library)
  newTVarIO cleanUpLibrary

parseTrack :: FilePath -> IO (Track)
parseTrack file = do
  time <- Data.Time.getCurrentTime
  osPath <- encodeUtf file
  metadata <- parseMetadata osPath
  case metadata of
    Left _ -> error "parse error"
    Right md -> pure $ Track { 
      path = T.pack file,
      duration = fromIntegral $ fromMaybe 0 md.audioProperties.duration,
      interval = 0,
      count = 0,
      lastPlay = Nothing,
      planPlay = Just $ addUTCTime 86400 time -- tomorrow for new track
                          } 

-- bank = "/home/m/share/sharedFolder/test"

migration :: FilePath -> FilePath -> IO Library
migration dir file = do
  eFileDB <- loadFromFileDB dir 
  dirDB <- loadFromDir dir
  case eFileDB of
    Left _ -> BL.writeFile file (encode dirDB) >> pure dirDB
    Right fileDB -> do
      pure $ Map.union fileDB dirDB

-- *****
loadFromDir :: FilePath -> IO Library
loadFromDir dir = do
  -- Получаем список всех элементов в текущей директории
  items <- listDirectory dir
  
  -- Превращаем их в полные пути
  let fullPaths = map (dir </>) items
  
  -- Разделяем пути на папки и файлы
  subDirs <- filterM doesDirectoryExist fullPaths
  let files = filter (`notElem` subDirs) fullPaths
  
  -- 1. Обрабатываем MP3 файлы в текущей директории
  let mp3s = filter (\f -> takeExtension f `elem` [".mp3",".flac",".wav",".ogg"]) files
  tracks <- mapM parseTrack mp3s
  let currentMap = Map.fromList (zip (map T.pack mp3s) tracks)
  
  -- 2. Рекурсивно заходим во все подпапки
  subMaps <- mapM loadFromDir subDirs
  
  -- 3. Объединяем карту текущей папки со всеми картами подпапок
  pure (Map.unions (currentMap : subMaps))

-- *****


-- loadFromDirectory :: 
loadFromFile :: FilePath -> IO Library
loadFromFile dir = do
  db <- loadDB dir
  print db
  pure db

loadDB :: FilePath -> IO Library
loadDB dir = do
  db <- loadFromFileDB dir
  case db of
    Left error' -> throwIO $ userError error'
    Right config -> pure config

loadFromFileDB :: FilePath -> IO (Either String Library)
loadFromFileDB path =
  either (Left . displayException) eitherDecode
    <$> try @SomeException (BL.readFile (path <> "/jukebox.json"))
