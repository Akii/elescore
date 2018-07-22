{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Elescore.Domain.Station
  ( StationRepo(..)
  , mkStationRepo
  ) where

import           ClassyPrelude
import           Data.Map               (elems)
import           Database.SQLite.Simple hiding (fold)

import           Elescore.Domain.Types

data StationRepo = StationRepo
  { findAll  :: forall m. MonadIO m => m [Station]
  , findById :: forall m. MonadIO m => StationId -> m (Maybe Station)
  , save     :: forall m. MonadIO m => Station -> m ()
  }

mkStationRepo :: Connection -> StationRepo
mkStationRepo conn = StationRepo {..}
  where
    findAll :: MonadIO m => m [Station]
    findAll = liftIO $ do
      ss <- foldr (liftA2 insertMap sId id) mempty <$> query_ conn "SELECT * FROM station"
      fs <- query_ conn "SELECT * FROM facility"
      return . elems $ foldr insertFacility ss fs

    -- this screams lens
    insertFacility :: Facility -> Map StationId Station -> Map StationId Station
    insertFacility f = adjustWithKey (\_ s -> s { sFacilities = insertMap (fId f) f (sFacilities s) }) (fStationId f)

    findById :: MonadIO m => StationId -> m (Maybe Station)
    findById sid = liftIO $ do
      ms <- headMay <$> query conn "SELECT * FROM station WHERE station_id = ?" (Only sid)
      for ms $ \s -> do
        fs <- query conn "SELECT * FROM facility WHERE station_id = ?" (Only sid)
        return s { sFacilities = foldr (liftA2 insertMap fId id) mempty fs }

    save :: MonadIO m => Station -> m ()
    save s = liftIO $ do
      execute conn "INSERT OR REPLACE INTO station VALUES (?,?)" s
      forM_ (sFacilities s) (execute conn "INSERT OR REPLACE INTO facility VALUES (?,?,?,?,?,?)")
