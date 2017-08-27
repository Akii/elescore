module Elescore.Remote.StationCache
  ( StationCache
  , loadStations
  , getStation
  , getStations
  , putStation
  ) where

import           ClassyPrelude              hiding (fromList)

import           Elescore.Disruptions.Types
import           Elescore.Repository

type StationCache = Repository Stations

loadStations :: FilePath -> IO StationCache
loadStations fp = do
  r <- loadRepository fp
  periodicallySaveRepository 3600 r
  return r

  where
    periodicallySaveRepository delay r =
      void . async $ forever (waitSeconds delay >> saveRepository r)

    waitSeconds = threadDelay . (* 1000000)

getStation :: StationCache -> StationId -> IO (Maybe Station)
getStation sc sid = getsEntities sc (lookup sid)

getStations :: StationCache -> IO Stations
getStations = getEntities

putStation :: StationCache -> Station -> IO ()
putStation sc s = modifyEntities sc (insertWith ((<>)) (sId s) s)
