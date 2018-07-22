{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Elescore.Domain.DisruptionLog
  ( DisruptionRepo(..)
  , mkDisruptionRepo
  ) where

import           ClassyPrelude          hiding (getCurrentTime)
import           Database.SQLite.Simple

import           Elescore.Domain.Types

data DisruptionRepo = DisruptionRepo
  { appendEvent :: forall m. MonadIO m => DisruptionEvent -> m ()
  , findAll     :: forall m. MonadIO m => m [DisruptionEvent]
  }

mkDisruptionRepo :: Connection -> DisruptionRepo
mkDisruptionRepo conn = DisruptionRepo {..}
  where
    appendEvent :: MonadIO m => DisruptionEvent -> m ()
    appendEvent dev = liftIO $ execute conn "INSERT INTO disruption_log VALUES (?,?,?,?,?,?,?)" dev

    findAll :: MonadIO m => m [DisruptionEvent]
    findAll = liftIO $ query_ conn "SELECT * FROM disruption_log ORDER BY rowid ASC"
