{-# LANGUAGE OverloadedStrings #-}
module Engine where

import Handlers.Engine (Song(..))
import Handlers.Logger (Log (Info), logMessage)
-- import System.Directory (doesFileExist)
import System.Process
import Control.Concurrent
-- import Text.Read (readMaybe)
import System.Directory.OsPath (listDirectory)
import System.OsPath (OsPath(..), (</>), takeExtension, unsafeEncodeUtf)
-- import Data.List (sort)

songmp3 = "/home/m/projects/jukebox/app/1.mp3"
songmp2 = "/home/m/projects/jukebox/2.mp3"
--

getListTracks :: OsPath ->  IO ([Song])
getListTracks dir = do
  files <- listDirectory dir
  let mp3s = [ dir </> f | f <- files, takeExtension f == (unsafeEncodeUtf ".mp3") ]
  let songs = [ Song i fp 0 0 0 | (i, fp) <- zip [1..] mp3s ]
  pure songs
  -- pure [Song 0 "" 0 0 0]
  -- pure []


-- getDuration :: FilePath -> IO (Maybe Double)
-- getDuration path = do
--   out <- readProcess "ffprobe"
--     ["-v", "error"
--     ,"-show_entries", "format=duration"
--     ,"-of", "default=noprint_wrappers=1:nokey=1"
--     ,path
--     ] ""
--   pure (readMaybe out)

-- engine :: IO ()
-- engine = do
--  putStrLn "Hello, Haskell!"
--  exists <- doesFileExist songmp3 
--  print exists
--  callProcess "xdg-open" [songmp3]
--  print "pause 3 sec"
--  threadDelay 3000000
--  print "after pause"
--  callProcess "xdg-open" [songmp2]
--
--  pure ()



