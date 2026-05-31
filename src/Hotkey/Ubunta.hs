{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Hotkey.Ubunta (getKey)
   where
import Control.Concurrent.STM ( atomically, writeTVar, TVar )
import Control.Monad (forever, when)
import Hotkey.Types ( Pause(..) )
import Graphics.X11.Xlib
    ( anyModifier,
      grabModeAsync,
      keyPress,
      keyPressMask,
      xK_F7,
      xK_F8,
      xK_F9,
      defaultScreen,
      openDisplay,
      rootWindow,
      allocaXEvent,
      get_EventType,
      get_KeyEvent,
      nextEvent,
      selectInput,
      sync,
      grabKey,
      keysymToKeycode )

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
    f9Code <- keysymToKeycode dpy xK_F9
    putStrLn $ "F7 - pause \t F8 - resume \t F9 - next"

    -- Глобально захватываем F7 на root-окне
    -- AnyModifier, чтобы не ломалось из-за NumLock/CapsLock
    grabKey dpy f7Code anyModifier root True grabModeAsync grabModeAsync
    grabKey dpy f8Code anyModifier root True grabModeAsync grabModeAsync
    grabKey dpy f9Code anyModifier root True grabModeAsync grabModeAsync

    -- Просим у root события нажатия клавиш
    selectInput dpy root keyPressMask
    sync dpy False

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
            when (keycode == f9Code) $ do
                atomically $ writeTVar pause Next 
                putStrLn "Нажата F9 (перехвачено глобально)"
