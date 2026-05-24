{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Handlers.Engine (Track(..), Handle(..), Library) where
import qualified Handlers.Logger
import System.OsPath (OsPath(..))
import Data.Time
import Data.List
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Map.Strict as Map

type Key = Text -- Path
type Library = Map.Map Key Track


data Track = Track
  { 
    -- path :: Text, -- unique
    path :: FilePath, -- unique
    duration :: Int, -- ms
    interval :: Int, -- через сколько day ставить
    lastPlay :: Maybe UTCTime,
    planPlay :: Maybe UTCTime
    -- factor :: Int -- newInterbal = CurrentInterval * Factor. Assess 5 4 3 2 1 
  }  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
   
-- data TrackJson = TrackJson
--   { 
--     path :: Text, -- unique
--     -- path :: OsPath, -- unique
--     duration :: Int, -- ms
--     interval :: Int, -- через сколько day ставить
--     lastPlay :: Maybe UTCTime,
--     planPla :: Maybe UTCTime
--     -- factor :: Int -- newInterbal = CurrentInterval * Factor. Assess 5 4 3 2 1 
--   }  
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
-- У нас создается база данных в джсон файле и загружается мапка в память.
-- Нужно теперь из этой мапки (отсортировать ее) получить список песен по полю когда слушать
-- дальше запускать песни, изменять их параметры в базе данных.лоЖц
--
data Handle m = Handle 
  { logger :: Handlers.Logger.Handle m,
    migration :: m (Map.Map FilePath Track), --add error potom
    -- saveLibrary :: Library -> m (),
    -- loadLibrary :: m (Either Text Library),
    modifyTrack :: Track -> m (),
    playTrack :: Track -> m ()
  }  


ghettoBluster :: forall m. Monad m => Handle m -> m ()
ghettoBluster Handle{..} = do
  db <- migration
  songs <- sortOn (planPlay) <$> getListTracks 
  mapM_ startPlay songs
    where startPlay :: Monad m => Track -> m ()
          startPlay t = do
            modifyTrack t
            playTrack t

-- getListTracks :: Map.Map FilePath Track -> [Track],
