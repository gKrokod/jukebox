module Handlers.Engine (Song(..), Handle(..)) where
import qualified Handlers.Logger
import System.OsPath (OsPath(..))

data Song = Song
  { id :: Int, 
    path :: OsPath, -- unique
    duration :: Double, --s
    interval :: Double, -- через сколько ставить
    count :: Int -- skolko raz igralol
  } 
   

data Handle m = Handle 
  { logger :: Handlers.Logger.Handle m,
    getListTracks :: m ([Song]),
    -- loadTracks :: undefined ,
    playTrack :: Song -> m ()
  }  
