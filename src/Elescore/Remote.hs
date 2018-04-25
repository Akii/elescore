module Elescore.Remote
  ( module Elescore.Remote.Client
  , module Elescore.Remote.Monitoring
  , module Elescore.Remote.Types
  , runAPI
  ) where

import ClassyPrelude

import Elescore.Remote.Client
import Elescore.Remote.Monitoring
import Elescore.Remote.Types
import Elescore.Types

runAPI :: API a -> Elescore a
runAPI a = do
  mgr <- reqManager
  key <- apiKey
  host <- opts optHost
  liftIO $ runReaderT a (key, mgr, host)
