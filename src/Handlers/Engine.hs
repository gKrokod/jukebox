{-# LANGUAGE RecordWildCards #-}
module Handlers.Engine (Library, ghettoBluster, Track(..), Handle(..), updateTrack, formatMMSS) where
import qualified Handlers.Logger
import Data.Time ( UTCTime, addUTCTime )
import Data.List ( sortOn )
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text as T (pack)
import Data.Text (Text)
import qualified Data.Map.Strict as Map

ghettoBluster :: forall m. Monad m => Handle m -> m ()
ghettoBluster h@Handle{..} = do
  playList <- sortedTracks <$> getPlayList h
  Handlers.Logger.logMessage logger Handlers.Logger.Info ("Размер плейлиста = " <> T.pack ( show $ length playList))
  mapM_ (\x -> infoTrack x >> startPlay x) playList
  Handlers.Logger.logMessage logger Handlers.Logger.Info ("Плейлист прослушан")
    where startPlay :: Monad m => Track -> m ()
          startPlay t = do
            modifyTrack t
            playTrack t
            saveDataBaseToFile 
          infoTrack :: Monad m => Track -> m ()
          infoTrack t = do
            Handlers.Logger.logMessage logger Handlers.Logger.Info ("Играет трек: " <> t.path)
            Handlers.Logger.logMessage logger Handlers.Logger.Info $ 
              ("Длительность: " <> formatMMSS t.duration <> ", Интервал: " <> T.pack (show t.interval) <> ", Следует прослушать: " <> T.pack (show t.planPlay))

formatMMSS :: Word -> Text
formatMMSS ms =
  let (m,s) = ms `divMod` 60000
  in T.pack $ mconcat[show m, ":",take 2 $ show s]

type Library = Map.Map Text Track
-- type Library = Map.Map FilePath Track

newtype PlayList = SortedTracks { sortedTracks :: [Track]} -- SorteList

mapToPlayList :: Library -> PlayList
mapToPlayList = SortedTracks 
                . map snd 
                . sortOn (planPlay . snd) 
                . Map.toList

data Track = Track
  { 
    path :: Text, -- unique
    duration :: Word, -- ms
    interval :: Word, -- через сколько day ставить
    count :: Word, 
    lastPlay :: Maybe UTCTime,
    planPlay :: Maybe UTCTime
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

