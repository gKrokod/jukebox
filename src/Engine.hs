{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Engine where
import Hotkey.Types
import Control.Exception (SomeException, displayException, throwIO, try)
import Handlers.Engine (Track(..), Library, updateTrack)
import System.Process
    ( createProcess, terminateProcess,
      proc,
      waitForProcess,
      CreateProcess(std_err, std_in, std_out),
      StdStream(NoStream) ) 
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ( (</>), takeExtension )
import System.OsPath (encodeUtf)
import Monatone.Common  (parseMetadata)
import Monatone.Metadata  (Metadata(..), AudioProperties (duration))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, eitherDecode)
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Data.Time 
import Control.Monad (filterM)
import Data.Time.Calendar
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.Async 
import Data.Time(nominalDiffTimeToSeconds)

getLibrary :: TVar Library -> IO (Library)
getLibrary libT = do
  atomically (readTVar libT)

modifyTrack :: TVar Library -> Track -> IO ()
modifyTrack libT track = do
  time <- getCurrentTime
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

--- ***
playTrack :: Track -> IO ()
playTrack track = do
  (_, _, _, ph) <-
    createProcess (proc "ffplay"
      [ "-nodisp"
      , "-autoexit"
      , "-loglevel", "quiet"
      , track.path
      ])
      { std_in  = NoStream
      , std_out = NoStream
      , std_err = NoStream
      }
  _ <- waitForProcess ph
  pure ()
--- ***

playTrackSTM :: TVar Pause -> TVar Double -> Track -> IO ()
playTrackSTM pause offset track = do
  p <- atomically $ readTVar pause
  if p == On then playTrackSTM pause offset track
  else do
    print "Pause for track"
    print p
    offsetStart <- atomically $ readTVar offset
    print "Offset for track:"
    print offsetStart
    print track.path
    timeStart <- getCurrentTime
    (_, _, _, ph) <-
      createProcess (proc "ffplay"
        [ "-nodisp"
        , "-autoexit"
        , "-ss", show offsetStart 
        , "-loglevel", "quiet"
        , track.path
        ])
        { std_in  = NoStream
        , std_out = NoStream
        , std_err = NoStream
        }
    -- let timeLeft = max 0 1
    let timeLeft = max 0 (fromIntegral track.duration - (ceiling $ offsetStart))

    timeout <- race (threadDelay (timeLeft * 1000)) (pressPause pause)
    case timeout of
      Left _ -> do 
        terminateProcess ph
        atomically $ writeTVar offset 0 
      Right timePause -> do 
        terminateProcess ph
        let offset' = (offsetStart + deltaOffset timeStart timePause)
        if offset' >= fromIntegral track.duration then do
          atomically $ writeTVar offset 0
        else do
          atomically $ writeTVar offset offset'
          playTrackSTM pause offset track

deltaOffset :: UTCTime -> UTCTime -> Double
deltaOffset start end = realToFrac $ diffUTCTime end start

pressPause :: TVar Pause -> IO (UTCTime)
pressPause pause = do
  atomically $ pressPause' pause
  getCurrentTime

pressPause' :: TVar Pause -> STM ()
pressPause' pause = do
  statusPause <- readTVar pause
  case statusPause of
    On -> pure ()
    Off -> retry



initLibrary :: FilePath -> FilePath -> IO (TVar Library)
initLibrary dir file = do
  library <- migration dir file
  newTVarIO library

parseTrack :: FilePath -> IO (Track)
parseTrack file = do
  time <- getCurrentTime
  osPath <- encodeUtf file
  metadata <- parseMetadata osPath
  case metadata of
    Left _ -> error "parse error"
    Right md -> pure $ Track { 
      path = file,
      duration = fromIntegral $ fromMaybe 0 md.audioProperties.duration,
      interval = 0,
      count = 0,
      lastPlay = Nothing,
      planPlay = Just time
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
--my version  
-- loadFromDir :: FilePath -> IO Library
-- loadFromDir dir = do
--   files <- listDirectory dir
--   let mp3s = [ (dir </> f) | f <- files, takeExtension f == (".mp3") ]
--   mp3m <- mapM parseTrack mp3s
--   pure (Map.fromList $ zip mp3s mp3m)
 

-- Предположим, что ваши типы выглядят так:
-- type Library = Map.Map FilePath Track
-- parseTrack :: FilePath -> IO Track
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
  let currentMap = Map.fromList (zip mp3s tracks)
  
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
