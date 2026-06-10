module PlayerState where

import System.Process
import Control.Concurrent.STM 

data FFPlay = FFPlay {
  offset :: TVar Double,
  ph :: TVar (Maybe ProcessHandle) }
