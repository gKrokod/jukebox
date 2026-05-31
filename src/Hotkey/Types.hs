module Hotkey.Types where

data Pause where
  On :: Pause
  Off :: Pause
  Next :: Pause -- yes, I know
  deriving (Show, Eq)

