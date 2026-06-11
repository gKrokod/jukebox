module ShutDown.Ubuntu where
import PlayerState (FFPlay)

consoleHandler :: FFPlay -> IO a -> IO a
consoleHandler = flip const
