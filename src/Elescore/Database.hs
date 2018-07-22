{-# LANGUAGE QuasiQuotes #-}

module Elescore.Database
  ( mkConnection
  ) where

import           ClassyPrelude
import           Database.SQLite.Simple
import           Text.RawString.QQ

mkConnection :: FilePath -> IO Connection
mkConnection dbFile = do
  conn <- open dbFile

  createDisruptionLogTable conn
  createStationTable conn
  createFacilityTable conn

  return conn

createDisruptionLogTable :: MonadIO m => Connection -> m ()
createDisruptionLogTable conn = liftIO $ execute_ conn [r|
    CREATE TABLE IF NOT EXISTS disruption_log (
      disruption_id VARCHAR PRIMARY KEY,
      station_id INTEGER NOT NULL,
      facility_id INTEGER NOT NULL,
      facility_state VARCHAR NOT NULL,
      reason VARCHAR,
      occurred_on VARCHAR NOT NULL,
      change_type VARCHAR NOT NULL
    );
  |]

createStationTable :: MonadIO m => Connection -> m ()
createStationTable conn = liftIO $ execute_ conn [r|
    CREATE TABLE IF NOT EXISTS station (
      station_id INTEGER PRIMARY KEY,
      name VARCHAR NOT NULL
    );
  |]

createFacilityTable :: MonadIO m => Connection -> m ()
createFacilityTable conn = liftIO $ execute_ conn [r|
    CREATE TABLE IF NOT EXISTS facility (
      facility_id INTEGER PRIMARY KEY,
      station_id INTEGER NOT NULL,
      facility_type VARCHAR NOT NULL,
      description VARCHAR,
      geo_coord_x REAL,
      geo_coord_y REAL
    );
  |]
