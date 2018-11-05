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

dataServer :: TVar DisruptionProjection -> IORef SumOfDowntimes -> TVar Objects -> TVar Facilities -> Server DataApi
dataServer dis dt objs fs =
  disruptionMarkerHandler dis objs fs
  :<|> listStationsHandler dt objs fs

disruptionMarkerHandler :: TVar DisruptionProjection -> TVar Objects -> TVar Facilities -> Handler [DisruptionMarker]
disruptionMarkerHandler dis objsR fsR = do
  objs <- liftIO (readTVarIO objsR)
  fs <- liftIO (readTVarIO fsR)
  diss <- liftIO $ filter (isNothing . dResolvedOn) . IM.elems . dpDisruptions <$> readTVarIO dis
  return . catMaybes $ map (fromDisruption objs fs) diss

listStationsHandler :: IORef SumOfDowntimes -> TVar Objects -> TVar Facilities -> Handler [UIStation]
listStationsHandler dt objsR fsR = do
  sodt <- liftIO (readIORef dt)
  objs <- liftIO (readTVarIO objsR)
  fs <- liftIO (readTVarIO fsR)
  return $ fmap (fromStation sodt fs) (M.elems objs)
