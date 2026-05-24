{-# LANGUAGE OverloadedStrings #-}
      

module Main where

import qualified Handlers.Engine
import Handlers.Logger (Log (..), logMessage)
-- import System.Directory (doesFileExist)
import System.Process
import System.OsPath
import qualified Handlers.Logger
import qualified Logger
import qualified Handlers.Engine
import qualified Engine
import Control.Concurrent
import Monatone.MP3 (parseMP3)
import Monatone.Common  (parseMetadata)
import Monatone.Metadata  (Metadata(..))
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text as T 

songmp3 = "/home/m/projects/jukebox/app/1.mp3"


getMD :: OsPath -> IO ()
getMD os = do
  result <- parseMetadata os
  case result of
    Left err -> TIO.putStrLn $ "Error: " <> (T.pack $ show err)
    Right metadata -> do
      -- print metadata
      TIO.putStrLn $ ("Title: ") <> maybe ("Unknown" :: Text) id metadata.title 
      TIO.putStrLn ( "Artist: " <> maybe "Unknown" id ( metadata.artist) )
      -- TIO.putStrLn $ (T.pack "Artist: ") <> (maybe (T.pack "Unknown") id (artist metadata) )
      TIO.putStrLn $ "Album: " <> maybe "Unknown" id ( metadata.album)

main :: IO ()
main = do
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Debug,
            Handlers.Logger.writeLog = Logger.writeLog
          }
      engine =
        Handlers.Engine.Handle
          { Handlers.Engine.logger = logHandle,
            -- Handlers.Engine.getListTracks = Engine.getListTracks (unsafeEncodeUtf "/home/m/share/sharedFolder/test"),
            Handlers.Engine.getListTracks = Engine.getListTracks (unsafeEncodeUtf "/home/m/share/sharedFolder"),
            Handlers.Engine.playTrack = undefined
          }
  ts <- Handlers.Engine.getListTracks (engine)
  dTs <- mapM (\x -> decodeFS x.path) ts
  -- h1 <- decodeFS (Handlers.Engine.path (head $ ts))
  print dTs
  putStrLn "Hello, Haskell!"
  mapM (\x -> getMD $ x.path) ts


  mapM_ (\x -> do
    -- threadDelay 9000000
    threadDelay 90
    print x    
    callProcess "xdg-open" [x]) dTs
    
  pure ()


