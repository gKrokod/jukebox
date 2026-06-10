{-# LANGUAGE CPP #-}

-- | Centralized, cross-platform shutdown handling for the ffplay child process.
--
-- The single source of truth for "is there a child process and how do we stop
-- it" is the @TVar (Maybe ProcessHandle)@ that lives in 'PlayerState.FFPlay'.
-- This module knows how to drain that handle safely ('killCurrentFFPlay') and
-- how to make sure the drain runs no matter how the program is torn down
-- ('withShutdownHandler').
--
-- On POSIX, ordinary @bracket@/@finally@ plus the RTS Ctrl+C handling is enough:
-- a SIGINT is delivered as a 'UserInterrupt' async exception, the cleanup runs,
-- and the child is killed.
--
-- On Windows, clicking the console close button (the @X@) raises a
-- @CTRL_CLOSE_EVENT@. The RTS does *not* turn that into a Haskell exception, so
-- @finally@ never runs and the program is killed by the OS after a short grace
-- period, leaving @ffplay@ orphaned and still playing. We therefore install a
-- native console control handler with the Win32 package that runs the same
-- cleanup synchronously before returning, which is the only reliable hook for
-- the close button.
module Player.Shutdown
  ( killCurrentFFPlay
  , withShutdownHandler
  ) where

import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (mask_)
import Control.Monad (void)
import System.Process (ProcessHandle, terminateProcess, waitForProcess)
import PlayerState (FFPlay (..))

#ifdef mingw32_HOST_OS
import System.Win32.Console.CtrlHandler (withConsoleCtrlHandler)
#endif

-- | Terminate the ffplay process that is currently tracked in the shared state
-- (if any) and block until it has actually exited, then clear the handle so we
-- never try to reap the same process twice.
--
-- Run under 'mask_' so an async exception cannot leave us with a half-killed
-- child or a stale handle. 'terminateProcess' followed by 'waitForProcess' is
-- what guarantees the OS process is gone, not merely signalled.
killCurrentFFPlay :: FFPlay -> IO ()
killCurrentFFPlay state = mask_ $ do
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

-- | Run an action with shutdown handling installed for the given player state.
--
-- The ffplay child is always reaped on the way out, whether the action returns
-- normally or dies from an exception. On Windows we additionally arm a native
-- console control handler so the same reaping happens when the console close
-- button is pressed, which the RTS would otherwise not surface as an exception.
withShutdownHandler :: FFPlay -> IO a -> IO a
#ifdef mingw32_HOST_OS
withShutdownHandler state action =
  withConsoleCtrlHandler handler action
  where
    -- CTRL_CLOSE_EVENT == 2, CTRL_LOGOFF_EVENT == 5, CTRL_SHUTDOWN_EVENT == 6.
    -- Ctrl+C (0) and Ctrl+Break (1) are left to the RTS so its normal
    -- UserInterrupt handling keeps working. Returning True tells Windows the
    -- event was handled. We reap synchronously because the process may be
    -- terminated by the OS as soon as the handler returns.
    handler ev
      | ev `elem` [2, 5, 6] = killCurrentFFPlay state >> pure True
      | otherwise           = pure False
#else
withShutdownHandler _ action = action
#endif
