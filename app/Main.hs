{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where


import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types
import Control.Monad (forever, when)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.Async 
import Hotkey.Ubunta
import Hotkey.Types

import qualified Handlers.Engine
import Handlers.Logger (Log (..))
import qualified Handlers.Logger
import qualified Logger
import qualified Engine


main2 :: IO ()
main2 = do
  pause <- atomically $ newTVar Off
  p <- atomically $ readTVar pause
  print p
  
  withAsync(getKey pause) $ \_ -> do
    print "I am here"
    getLine >>= putStrLn
    p <- atomically $ readTVar pause
    print p


main :: IO ()
main = do
  pause <- atomically $ newTVar Off
  offset <- atomically $ newTVar 0 
  p <- atomically $ readTVar pause
  print p
  -- dir <- getCurrentDirectory - for realise 
  let dir ="/home/m/share/sharedFolder/test"
      file = dir <> "/jukebox.json"
  -- let dir ="C:\\sharedFolder\\test" -- windows
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
            -- Handlers.Engine.playTrack = Engine.playTrack,
            Handlers.Engine.playTrack = Engine.playTrackSTM pause offset
          }
  withAsync(getKey pause) $ \_ -> do
    Handlers.Engine.ghettoBluster engine    
    print "I am here"
    getLine >>= putStrLn
    p <- atomically $ readTVar pause
    print p
