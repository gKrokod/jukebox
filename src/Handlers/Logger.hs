module Handlers.Logger (Handle (..), Log (..), logMessage) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Text as T
import GHC.Generics (Generic)

data Log = Info | Debug | Warning | Error | Fatal
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Handle m = Handle
  { levelLogger :: Log,
    writeLog :: T.Text -> m ()
  }

logMessage :: (Monad m) => Handle m -> Log -> T.Text -> m ()
logMessage h lvl msg
  | lvl >= levelLogger h = writeLog h (mconcat ["[", T.pack $ show lvl, "] ", msg])
  | otherwise = pure ()
