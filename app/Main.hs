module Main (main) where

import Control.Concurrent.STM ( atomically, newTVar )
import Control.Concurrent.Async ( withAsync )
import Hotkey.Types ( Pause(Off) )
import qualified Handlers.Engine
import Handlers.Logger (Log (..))
import qualified Handlers.Logger
import qualified Logger
import qualified Engine
import qualified DataBase
import PlayerState
import Player.Shutdown (killCurrentFFPlay, withShutdownHandler)
import Hotkey.Grab (getKey)
import System.IO (hSetEncoding, stdout, stderr, utf8)
import Control.Exception
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
-- on Windows type cmd /K "chcp 65001 & R:\_JUKEBOX\musicjukebox-exe.exe"
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  (pause, offset, ph) <- atomically $ 
    (,,) <$> newTVar Off
         <*> newTVar 0
         <*> newTVar Nothing

  dir <- getCurrentDirectory

-- #ifdef mingw32_HOST_OS
--   let dir ="C:\\sharedFolder\\test" -- windows
--       -- file = dir <> "\\jukebox.json"
-- #else
--   let dir ="/home/m/share/sharedFolder/test" -- file = dir <> "/jukebox.json"
-- #endif

  let
#ifdef mingw32_HOST_OS
      file = dir <> "\\jukebox.json"
#else
      file = dir <> "/jukebox.json"
#endif
  tvar <- DataBase.initLibrary dir file
  let ffplay = FFPlay offset ph
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Info,
            Handlers.Logger.writeLog = Logger.writeLog
          }
      engine =
        Handlers.Engine.Handle
          { Handlers.Engine.logger = logHandle,
            Handlers.Engine.getLibrary = Engine.getLibrary tvar,
            Handlers.Engine.modifyTrack = Engine.modifyTrack tvar,
            Handlers.Engine.saveDataBaseToFile = Engine.saveDataBaseToFile file tvar,
            Handlers.Engine.playTrack = Engine.playTrackSTM pause ffplay
          }
  withShutdownHandler ffplay $
    withAsync (getKey pause) $ \_ -> do
      Handlers.Engine.ghettoBluster engine
        `finally` (do
          -- Per-track playback already reaps ffplay via bracket; this is a
          -- final safety net for any handle still live at teardown, plus the
          -- database flush. On Windows console-close the native handler in
          -- withShutdownHandler does the reaping before the OS kills us.
          killCurrentFFPlay ffplay
          Engine.saveDataBaseToFile file tvar
        )
      putStrLn "mb Playlist end. Please type anything"
      getLine >>= putStrLn
