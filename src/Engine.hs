{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Engine where

import Control.Exception (SomeException, displayException, throwIO, try)
import Handlers.Engine (Track(..), Library, updateTrack)
import System.Process
    ( createProcess,
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
import Data.Time ( getCurrentTime )
import Control.Monad (filterM)

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
