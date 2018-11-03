{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api.Data
  ( DataApi
  , dataServer
  ) where

import           ClassyPrelude       hiding (Handler)
import qualified Data.IntMap         as IM
import qualified Data.Map            as M
import           Servant

import           Elescore.Api.Types
import           Elescore.Projection

type DataApi =
       "disruptions" :> DisruptionApi
  :<|> "stations" :> Get '[JSON] [UIStation]

type DisruptionApi =
       "marker" :> Get '[JSON] [DisruptionMarker]

dataServer :: IORef DisruptionProjection -> IORef SumOfDowntimes -> IORef Objects -> IORef Facilities -> Server DataApi
dataServer dis dt objs fs =
  disruptionMarkerHandler dis objs fs
  :<|> listStationsHandler dt objs fs

disruptionMarkerHandler :: IORef DisruptionProjection -> IORef Objects -> IORef Facilities -> Handler [DisruptionMarker]
disruptionMarkerHandler dis objsR fsR = do
  objs <- liftIO (readIORef objsR)
  fs <- liftIO (readIORef fsR)
  diss <- liftIO $ filter (isNothing . dResolvedOn) . IM.elems . dpDisruptions <$> readIORef dis
  return . catMaybes $ map (fromDisruption objs fs) diss

listStationsHandler :: IORef SumOfDowntimes -> IORef Objects -> IORef Facilities -> Handler [UIStation]
listStationsHandler dt objsR fsR = do
  sodt <- liftIO (readIORef dt)
  objs <- liftIO (readIORef objsR)
  fs <- liftIO (readIORef fsR)
  return $ fmap (fromStation sodt fs) (M.elems objs)
