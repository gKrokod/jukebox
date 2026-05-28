{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Handlers.Engine (Library, ghettoBluster, Track(..), Handle(..), updateTrack) where
import qualified Handlers.Logger
import Data.Time ( UTCTime, addUTCTime )
import Data.List ( sortOn )
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text as T (pack)
import qualified Data.Map.Strict as Map

type Library = Map.Map FilePath Track

newtype PlayList = SortedTracks { sortedTracks :: [Track]} -- SorteList

mapToPlayList :: Library -> PlayList
mapToPlayList = SortedTracks 
                . map snd 
                . sortOn (planPlay . snd) 
                . Map.toList

data Track = Track
  { 
    -- path :: Text, -- unique
    path :: FilePath, -- unique
    duration :: Word, -- ms
    interval :: Word, -- через сколько day ставить
    count :: Word, 
    lastPlay :: Maybe UTCTime,
    planPlay :: Maybe UTCTime
    -- factor :: Int -- newInterbal = CurrentInterval * Factor. Assess 5 4 3 2 1 
  }  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
   
data Handle m = Handle 
  { logger :: Handlers.Logger.Handle m,
    getLibrary :: m (Library),
    modifyTrack :: Track -> m (),
    saveDataBaseToFile :: m (),
    playTrack :: Track -> m ()
  }  

updateTrack :: UTCTime -> Track -> Track
updateTrack time track = track {
  lastPlay = Just time,
  planPlay = Just $ addUTCTime (fromIntegral track.interval * 86400) time,
  count = succ track.count,
  interval = updateInterval track.count track.interval
                               } 
  where updateInterval :: Word -> Word -> Word
        -- 1 = 1
        -- 2 = 6
        -- n = interval * 1.7
        updateInterval 2 _ = 6
        updateInterval _ i  = max 1 (ceiling $ fromIntegral i * baseEaseFactor)
          where baseEaseFactor = 1.7 :: Double

getPlayList :: (Monad m) => Handle m -> m (PlayList)
getPlayList = (mapToPlayList <$>) . getLibrary

ghettoBluster :: forall m. Monad m => Handle m -> m ()
ghettoBluster h@Handle{..} = do
  playList <- sortedTracks <$> getPlayList h
  Handlers.Logger.logMessage logger Handlers.Logger.Debug ("Playlist size = " <> T.pack ( show $ length playList))
  mapM (\x -> Handlers.Logger.logMessage logger Handlers.Logger.Debug (T.pack $ show x) ) playList
  mapM_ (\x -> infoTrack x >> startPlay x) playList
  Handlers.Logger.logMessage logger Handlers.Logger.Debug ("Playlist end")
    where startPlay :: Monad m => Track -> m ()
          startPlay t = do
            playTrack t
            modifyTrack t
            saveDataBaseToFile 
          infoTrack :: Monad m => Track -> m ()
          infoTrack t = do
            Handlers.Logger.logMessage logger Handlers.Logger.Debug "Играет трек"
            Handlers.Logger.logMessage logger Handlers.Logger.Debug (T.pack $ show t)
