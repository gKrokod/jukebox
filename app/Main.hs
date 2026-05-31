module Main (main) where

import Control.Concurrent.STM
import Control.Concurrent.Async 
import Hotkey.Types

import qualified Handlers.Engine
import Handlers.Logger (Log (..))
import qualified Handlers.Logger
import qualified Logger
import qualified Engine
import Hotkey.Grab (getKey)


main :: IO ()
main = do
  pause <- atomically $ newTVar Off
  offset <- atomically $ newTVar 0 
  -- dir <- getCurrentDirectory - for realise 
#ifdef mingw32_HOST_OS
  let dir ="C:\\sharedFolder\\test" -- windows
      file = dir <> "\\jukebox.json"
#else
  let dir ="/home/m/share/sharedFolder/test"
      file = dir <> "/jukebox.json"
#endif
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
            Handlers.Engine.playTrack = Engine.playTrackSTM pause offset
          }
  withAsync(getKey pause) $ \_ -> do
    Handlers.Engine.ghettoBluster engine    
    putStrLn "Playlist end. Please type anything"
    getLine >>= putStrLn
