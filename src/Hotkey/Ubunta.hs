{-# LANGUAGE RecordWildCards #-}

module Hotkey.Ubunta
   where
import Control.Concurrent.STM
import Control.Monad (forever, when)
import Control.Concurrent
import Control.Concurrent.Async 
import Hotkey.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types


getKey :: TVar Pause -> IO ()
getKey pause = do
    -- Подключаемся к X-серверу
    dpy <- openDisplay ""

    let scr  = defaultScreen dpy
    root <- rootWindow dpy scr

    -- Получаем keycode для F7 (должен совпасть с твоим 73)
    -- f7Code <- keysymToKeycode dpy xK_A
    f7Code <- keysymToKeycode dpy xK_F7
    f8Code <- keysymToKeycode dpy xK_F8
    putStrLn $ "F7 keycode = " ++ show f7Code

    -- Глобально захватываем F7 на root-окне
    -- AnyModifier, чтобы не ломалось из-за NumLock/CapsLock
    grabKey dpy f7Code anyModifier root True grabModeAsync grabModeAsync
    grabKey dpy f8Code anyModifier root True grabModeAsync grabModeAsync

    -- Просим у root события нажатия клавиш
    selectInput dpy root keyPressMask
    sync dpy False

    putStrLn "Глобально слушаю F7 (X11). Нажимай F7, Ctrl+C для выхода."

    allocaXEvent $ \ev -> forever $ do
    -- allocaXEvent $ \ev -> do
        nextEvent dpy ev
        t <- get_EventType ev
        when (t == keyPress) $ do
            (_, _, _, _, _, _, _, _mods, keycode, _) <- get_KeyEvent ev
            when (keycode == f7Code) $ do
                atomically $ writeTVar pause On 
                putStrLn "Нажата F7 (перехвачено глобально)"
            when (keycode == f8Code) $ do
                atomically $ writeTVar pause Off 
                putStrLn "Нажата F8 (перехвачено глобально)"
