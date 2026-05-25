{-# LANGUAGE OverloadedStrings #-}
      

module Main where

import qualified Handlers.Engine
import Handlers.Logger (Log (..), logMessage)
-- import System.Directory (doesFileExist)
import System.Process
import System.OsPath
import qualified Handlers.Logger
import qualified Logger
import qualified Engine
import Control.Concurrent
import Monatone.MP3 (parseMP3)
import Monatone.Common  (parseMetadata)
import Monatone.Metadata  (Metadata(..), AudioProperties (duration))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T 
import Data.Maybe
import Data.Text (Text)

main :: IO ()
main = do
  -- dir <- getCurrentDirectory - for realise 
  let dir ="/home/m/share/sharedFolder/test"
      file = dir <> "/jukebox.json"
  -- let dir ="C:\\sharedFolder" -- windows
  --     file = dir <> "\\jukebox.json"
  tvar <- Engine.initLibrary dir file
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Debug,
            Handlers.Logger.writeLog = Logger.writeLog
          }
      engine =
        Handlers.Engine.Handle
          { Handlers.Engine.logger = logHandle,
            Handlers.Engine.getLibrary = Engine.getLibrary tvar,
            Handlers.Engine.modifyTrack = Engine.modifyTrack tvar,
            Handlers.Engine.saveDataBaseToFile = Engine.saveDataBaseToFile file tvar,
            Handlers.Engine.playTrack = Engine.playTrack
          }
  Handlers.Engine.ghettoBluster engine    
  pure ()


