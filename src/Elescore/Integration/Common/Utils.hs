{-# LANGUAGE ScopedTypeVariables #-}

module Elescore.Integration.Common.Utils where

import           ClassyPrelude                          hiding (for, forM)
import           Pipes

import           Database.SimpleEventStore

-- | Yields given `bs` before awaiting data from upstream
eachBefore :: Monad m => [a] -> Pipe a a m ()
eachBefore bs = do
  forM_ bs yield
  for cat yield

eventStoreP :: (MonadIO m, HasStream a, PersistableEvent a) => Store -> Pipe [a] [PersistedEvent a] m ()
eventStoreP store = for cat $ \ev -> do
  pev <- liftIO . try $ appendMany store ev
  either (\(e :: SQLError) -> print e) yield pev
