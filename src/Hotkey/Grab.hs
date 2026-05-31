module Hotkey.Grab (getKey) where

#ifdef mingw32_HOST_OS
import Hotkey.Windows (getKey)
#else
import Hotkey.Ubuntu (getKey)
#endif
