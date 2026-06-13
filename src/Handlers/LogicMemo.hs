module Handlers.LogicMemo where
import Handlers.Engine
    ( Track(interval, lastPlay, planPlay, count) )
import Data.Time ( UTCTime, addUTCTime )
  

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
