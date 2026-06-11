module ShutDown.ConsoleHandler (consoleHandler)  where

#ifdef mingw32_HOST_OS
import ShutDown.Windows (consoleHandler)
#else
import ShutDown.Ubuntu (consoleHandler)
#endif
