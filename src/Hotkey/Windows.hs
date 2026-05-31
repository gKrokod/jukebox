{-# LANGUAGE CPP #-}
module Hotkey.Windows (getKey)
   where

import Control.Concurrent.STM (atomically, writeTVar, TVar)
import Hotkey.Types (Pause(..))
import Control.Concurrent (threadDelay)
import qualified Data.Bits as B
import Graphics.Win32.Key

-- Виртуальные коды клавиш
vkCtrl, vkF7, vkF8, vkF9 :: Int
vkCtrl = 0x11   -- VK_CONTROL
vkF7   = 0x76   -- VK_F7
vkF8   = 0x77   -- VK_F8
vkF9   = 0x78   -- VK_F9

-- Проверка: клавиша сейчас нажата?
-- GetAsyncKeyState: старший бит (0x8000) означает "клавиша сейчас зажата" [web:179][web:181]
isDown :: Int -> IO Bool
isDown vk = do
  state <- getAsyncKeyState vk
  let s :: Int
      s = fromIntegral state
  pure ((s B..&. 0x8000) /= 0)

-- Нажаты ли одновременно Ctrl + нужная клавиша
isCtrlComboDown :: Int -> IO Bool
isCtrlComboDown vk = do
  ctrl <- isDown vkCtrl
  key  <- isDown vk
  pure (ctrl && key)

-- Ждём, пока будут отпущены и Ctrl, и сама клавиша
waitReleaseCombo :: Int -> IO ()
waitReleaseCombo vk = do
  ctrl <- isDown vkCtrl
  key  <- isDown vk
  if ctrl || key
    then threadDelay 50000 >> waitReleaseCombo vk
    else pure ()

-- Глобальная обработка:
-- Ctrl+F7 -> On
-- Ctrl+F8 -> Off
-- Ctrl+F9 -> Next
getKey :: TVar Pause -> IO ()
getKey pauseVar = loop
  where
    loop = do
      d7 <- isCtrlComboDown vkF7
      d8 <- isCtrlComboDown vkF8
      d9 <- isCtrlComboDown vkF9

      if d7
        then do
          atomically $ writeTVar pauseVar On
          waitReleaseCombo vkF7
          loop
        else if d8
          then do
            atomically $ writeTVar pauseVar Off
            waitReleaseCombo vkF8
            loop
          else if d9
            then do
              atomically $ writeTVar pauseVar Next
              waitReleaseCombo vkF9
              loop
            else do
              threadDelay 50000
              loop