{-# LANGUAGE OverloadedStrings #-}
module Engine where

import Control.Exception (SomeException, displayException, throwIO, try)
import Handlers.Engine (Track(..), Library)
import Handlers.Logger (Log (Info), logMessage)
import System.Process
import Control.Concurrent
import System.Directory (listDirectory)
import System.FilePath
import System.OsPath (encodeUtf)
-- import System ((</>), takeExtension, unsafeEncodeUtf)
-- import System.Directory.OsPath (listDirectory)
-- import System.OsPath (OsPath(..), (</>), takeExtension, unsafeEncodeUtf)
import Monatone.Common  (parseMetadata)
import Monatone.Metadata  (Metadata(..), AudioProperties (duration))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, eitherDecode)
import Data.Time (getCurrentTime)
import Data.Maybe
import qualified Data.Map.Strict as Map

parseTrack :: FilePath -> IO (Track)
parseTrack file = do
  time <- getCurrentTime
  osPath <- encodeUtf file
  metadata <- parseMetadata osPath
  case metadata of
    Left _ -> error "parse error"
    Right md -> pure $ Track { 
      path = file,
      duration = fromMaybe 0 md.audioProperties.duration,
      interval = 0,
      lastPlay = Nothing,
      planPlay = Just time
                          } 

bank = "/home/m/share/sharedFolder/test"
migration :: FilePath -> IO (Map.Map FilePath Track)
migration dir = do
  eFileDB <- loadFromFileDB dir 
  dirDB <- loadFromDir dir
  case eFileDB of
    Left _ -> BL.writeFile file (encode dirDB) >> pure dirDB
    Right fileDB -> do
      pure $ Map.union fileDB dirDB
  where 
    file = dir <> "/jukebox.json"
  
loadFromDir :: FilePath -> IO (Map.Map FilePath Track)
loadFromDir dir = do
  files <- listDirectory dir
  let mp3s = [ (dir </> f) | f <- files, takeExtension f == (".mp3") ]
  mp3m <- mapM parseTrack mp3s
  pure (Map.fromList $ zip mp3s mp3m)
  
-- loadFromDirectory :: 
loadFromFile :: FilePath -> IO (Map.Map FilePath Track)
loadFromFile dir = do
  db <- loadDB dir
  print db
  pure db

loadDB :: FilePath -> IO (Map.Map FilePath Track)
loadDB dir = do
  db <- loadFromFileDB dir
  case db of
    Left error' -> throwIO $ userError error'
    Right config -> pure config

loadFromFileDB :: FilePath -> IO (Either String (Map.Map FilePath Track))
loadFromFileDB path =
  either (Left . displayException) eitherDecode
    <$> try @SomeException (BL.readFile (path <> "/jukebox.json"))
