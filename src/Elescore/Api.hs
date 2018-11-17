{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api
  ( eleapi
  , module Elescore.Projection.Disruption
  ) where

import           ClassyPrelude
import           Control.Concurrent             (forkIO, threadDelay)
import qualified Data.IntMap                    as IM
import qualified Data.Map                       as M
import           Data.Proxy
import           Network.Wai.Handler.Warp       (run)
import           Servant.API
import           Servant.Server

import           Elescore.Api.Data
import           Elescore.Api.Types
import           Elescore.Projection
import           Elescore.Projection.Disruption
import           Elescore.Types

type API = "api" :> DataApi

eleapi :: Elescore ()
eleapi = do
  port <- config cfgPort
  diss <- disruptions
  dt <- downtimes
  objR <- objects
  fcR <- facilities

  markerRef <- newIORef mempty
  stationsRef <- newIORef mempty

  runFrontendCache markerRef stationsRef diss dt objR fcR

  liftIO . run port $ serve api (dataServer markerRef stationsRef)

  where
    api :: Proxy API
    api = Proxy

runFrontendCache
  :: MonadIO m
  => IORef [DisruptionMarker]
  -> IORef [UIStation]
  -> IORef DisruptionProjection
  -> IORef SumOfDowntimes
  -> IORef Objects
  -> IORef Facilities
  -> m ()
runFrontendCache markerRef stationsRef disProjRef sodRef objRef facRef = void . liftIO . forkIO . forever $ do
  sodt <- readIORef sodRef
  objs <- readIORef objRef
  fs <- readIORef facRef
  diss <- liftIO $ filter (isNothing . dResolvedOn) . IM.elems . dpDisruptions <$> readIORef disProjRef

  let marker = mapMaybe (fromDisruption objs fs) diss
      stations = fmap (fromStation sodt fs) (M.elems objs)

  void $ concurrently
    (marker `deepseq` writeIORef markerRef marker)
    (stations `deepseq` writeIORef stationsRef stations)

  threadDelay (5 * 1000000)
