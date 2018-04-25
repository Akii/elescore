module Elescore.Disruptions.StationCache
  ( Stations
  , StationCache
  , loadStations
  , getStation
  , getStations
  , putStation
  ) where

import           ClassyPrelude         hiding (fromList)

import           Elescore.Common.Types
import           Elescore.Common.EventLog

type Stations = Map StationId Station

data StationCache = StationCache
  { scFilePath :: FilePath
  , scStations :: IORef Stations
  }

loadStations :: FilePath -> IO StationCache
loadStations fp = do
  smap <- newIORef =<< foldl' (flip insertStation) mempty <$> loadLog fp
  return (StationCache fp smap)

getStation :: StationCache -> StationId -> IO (Maybe Station)
getStation sc sid = lookup sid <$> getStations sc

getStations :: StationCache -> IO Stations
getStations = readIORef . scStations

putStation :: StationCache -> Station -> IO ()
putStation sc s = do
  modifyIORef' (scStations sc) (insertStation s)
  appendLog (scFilePath sc) s

insertStation :: Station -> Stations -> Stations
insertStation s = insertMap (sId s) s
