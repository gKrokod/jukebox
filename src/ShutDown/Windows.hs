module ShutDown.Windows (consoleHandler) where
import PlayerState (FFPlay)
import System.Win32.Types
import System.Win32.Console
import System.Win32.Console.CtrlHandler 
import System.Process ( createProcess, terminateProcess, proc,waitForProcess, ProcessHandle,
      CreateProcess(std_err, std_in, std_out), StdStream(NoStream) ) 

shutdownProcess :: FFPlay -> IO ()
shutdownProcess state = mask_ $ do
  mph <- atomically $ do
    cur <- readTVar state.ph
    writeTVar state.ph Nothing
    pure cur
  mapM_ reap mph
  where
    reap :: ProcessHandle -> IO ()
    reap ph = do
      terminateProcess ph
      void (waitForProcess ph)

consoleHandler :: FFPlay -> IO a -> IO a
consoleHandler ffplay = 
  withConsoleCtrlHandler handler 
  where
    handler :: DWORD -> IO Bool
    handler exception
      | exception > 1 = shutdownProcess ffplay >> pure True
      | otherwise = pure False

--
-- CTRL_C_EVENT = 0
--
-- CTRL_BREAK_EVENT = 1
--
-- CTRL_CLOSE_EVENT = 2
--
-- CTRL_LOGOFF_EVENT = 5
--
-- CTRL_SHUTDOWN_EVENT = 6

