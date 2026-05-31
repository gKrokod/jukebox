module Hotkey.Ubuntu (getKey)
   where

import Control.Concurrent.STM (atomically, writeTVar, TVar)
import Control.Monad (forever, when)
import Data.Bits ((.&.))
import Hotkey.Types (Pause(..))
import Graphics.X11.Xlib
    ( anyModifier
    , controlMask
    , grabModeAsync
    , keyPress
    , keyPressMask
    , xK_F7
    , xK_F8
    , xK_F9
    , defaultScreen
    , openDisplay
    , rootWindow
    , allocaXEvent
    , get_EventType
    , get_KeyEvent
    , nextEvent
    , selectInput
    , sync
    , grabKey
    , keysymToKeycode
    )

getKey :: TVar Pause -> IO ()
getKey pause = do
    dpy <- openDisplay ""

    let scr = defaultScreen dpy
    root <- rootWindow dpy scr

    f7Code <- keysymToKeycode dpy xK_F7
    f8Code <- keysymToKeycode dpy xK_F8
    f9Code <- keysymToKeycode dpy xK_F9

    putStrLn "Ctrl+F7 - pause\tCtrl+F8 - resume\tCtrl+F9 - next"

    -- Глобально захватываем F7/F8/F9 на root-окне.
    -- Используем anyModifier, а нужный Ctrl проверяем уже по state события.
    -- Это помогает не ломаться из-за NumLock/CapsLock и прочих модификаторов. [web:186][web:196]
    grabKey dpy f7Code anyModifier root True grabModeAsync grabModeAsync
    grabKey dpy f8Code anyModifier root True grabModeAsync grabModeAsync
    grabKey dpy f9Code anyModifier root True grabModeAsync grabModeAsync

    selectInput dpy root keyPressMask
    sync dpy False

    allocaXEvent $ \ev -> forever $ do
        nextEvent dpy ev
        t <- get_EventType ev
        when (t == keyPress) $ do
            (_, _, _, _, _, _, _, mods, keycode, _) <- get_KeyEvent ev

            let ctrlPressed = (mods .&. controlMask) /= 0

            when (ctrlPressed && keycode == f7Code) $ do
                atomically $ writeTVar pause On
                putStrLn "Нажата Ctrl+F7 (перехвачено глобально)"

            when (ctrlPressed && keycode == f8Code) $ do
                atomically $ writeTVar pause Off
                putStrLn "Нажата Ctrl+F8 (перехвачено глобально)"

            when (ctrlPressed && keycode == f9Code) $ do
                atomically $ writeTVar pause Next
                putStrLn "Нажата Ctrl+F9 (перехвачено глобально)"
