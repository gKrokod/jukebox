{-# LANGUAGE CPP #-}
module Hotkey.Grab (getKey) where

#ifdef mingw32_HOST_OS
import Hotkey.Windows (getKey)
#else
import Hotkey.Ubunta (getKey)
#endif
