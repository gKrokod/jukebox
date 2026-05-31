{-# LANGUAGE CPP #-}
module Hotkey.Windows
   where
import Control.Concurrent.STM ( atomically, writeTVar, TVar )
import Control.Monad (forever, when)
import Hotkey.Types ( Pause(..) )

import Control.Concurrent (threadDelay)
import qualified Data.Bits as B ((.&.))
#ifdef mingw32_HOST_OS
import Graphics.Win32.Key
#endif

import qualified Data.Bits as B
#ifdef mingw32_HOST_OS
import Graphics.Win32.Key
#endif

-- Какие клавиши нас интересуют
data Key = KeyF7 | KeyF8 | KeyF9
  deriving (Eq, Show)

-- Виртуальные коды F7/F8/F9 (VK_F7/VK_F8/VK_F9)
vkF7, vkF8, vkF9 :: Int
vkF7 = 0x76
vkF8 = 0x77
vkF9 = 0x78

-- Проверка: клавиша сейчас нажата?
-- getAsyncKeyState возвращает 16-битное значение (WORD),
-- старший бит (0x8000) == клавиша зажата [web:43][web:84][web:91]
isDown :: Int -> IO Bool
isDown vk = do
#ifdef mingw32_HOST_OS
  state <- getAsyncKeyState vk    -- :: WORD
#else
  let state = 0 :: Int            -- заглушка для не-Windows, если нужно компилировать
#endif
  let s :: Int
      s = fromIntegral state      -- приводим к Int, чтобы .&. нормально типизировался
  return ((s B..&. 0x8000) /= 0)

-- Ждём отпускания конкретной клавиши, чтобы не ловить автоповтор
waitRelease :: Int -> IO ()
waitRelease vk = do
  d <- isDown vk
  if d
    then threadDelay 50000 >> waitRelease vk
    else return ()

-- Блокирующая функция: ждём, пока пользователь нажмёт F7/F8/F9
getKey :: IO Key
getKey = loop
  where
    loop = do
      d7 <- isDown vkF7
      d8 <- isDown vkF8
      d9 <- isDown vkF9
      case () of
        _ | d7 -> waitRelease vkF7 >> return KeyF7
          | d8 -> waitRelease vkF8 >> return KeyF8
          | d9 -> waitRelease vkF9 >> return KeyF9
          | otherwise -> do
              threadDelay 50000
              loop
