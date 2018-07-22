module Elescore.Remote
  ( module Elescore.Remote.Client
  , module Elescore.Remote.Types
  , module Elescore.Remote.Mapping
  , runAPI
  ) where

import           ClassyPrelude

import           Elescore.Remote.Client
import           Elescore.Remote.Mapping
import           Elescore.Remote.Types
import           Elescore.Types

runAPI :: API a -> Elescore a
runAPI a = do
  mgr <- reqManager
  key <- apiKey
  host <- config cfgHost
  liftIO $ runReaderT a (key, mgr, host)
