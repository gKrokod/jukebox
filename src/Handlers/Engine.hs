{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Handlers.Engine (ghettoBluster, Track(..), Handle(..), Library(..), updateTrack) where
import qualified Handlers.Logger
import System.OsPath (OsPath(..))
import Data.Time
import Data.List
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import GHC.Generics (Generic)
import Data.Text (Text)
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
  interval = updateInterval track.interval
                               } 
  where updateInterval :: Word -> Word
        updateInterval i | i >= 60 = i + div i 10
                         | i == 0 = succ i
                         | otherwise = i * 2
--
getPlayList :: (Monad m) => Handle m -> m (PlayList)
getPlayList = (mapToPlayList <$>) . getLibrary

ghettoBluster :: forall m. Monad m => Handle m -> m ()
ghettoBluster h@Handle{..} = do
  playList <- sortedTracks <$> getPlayList h
  mapM (\x -> Handlers.Logger.logMessage logger Handlers.Logger.Debug (T.pack $ show x) ) playList
  mapM_ (\x -> infoTrack x >> startPlay x) playList
    where startPlay :: Monad m => Track -> m ()
          startPlay t = do
            playTrack t
            modifyTrack t
            saveDataBaseToFile 
          infoTrack :: Monad m => Track -> m ()
          infoTrack t = do
            Handlers.Logger.logMessage logger Handlers.Logger.Debug "Играет трек"
            Handlers.Logger.logMessage logger Handlers.Logger.Debug (T.pack $ show t)


  -- let logHandle =
  --       Handlers.Logger.Handle
  --         { Handlers.Logger.levelLogger = Debug,
  --           Handlers.Logger.writeLog = Logger.writeLog
  --         }
  --     engine =
  --       Handlers.Engine.Handle
  --         { Handlers.Engine.logger = logHandle,
