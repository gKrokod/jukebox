{-# LANGUAGE RecordWildCards #-}
module Handlers.Engine (Library, ghettoBluster, Track(..), Handle(..), formatMMSS) where
import qualified Handlers.Logger
import Data.Time ( UTCTime )
import Data.List ( sortOn )
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text as T (pack)
import Data.Text (Text)
import qualified Data.Map.Strict as Map

ghettoBluster :: forall m. Monad m => Handle m -> m ()
ghettoBluster h@Handle{..} = do
  playList <- shuffle 1 . sortedTracks <$> getPlayList h
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

getPlayList :: (Monad m) => Handle m -> m (PlayList)
getPlayList = (mapToPlayList <$>) . getLibrary


--fro little random in start playlist
shuffle :: Int -> [a] -> [a]
shuffle 0 xs = xs
shuffle n xs = shuffle (pred n) z
  where l = length xs
        part = div l 3
        (x1, x2) = splitAt part xs
        z = myZip x1 x2

myZip :: [a] -> [a] -> [a]
myZip (x : xs) (y : ys) = x : y : myZip xs ys
myZip [] ys = ys 
myZip xs [] = xs 

