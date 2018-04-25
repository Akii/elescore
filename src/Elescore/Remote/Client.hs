{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Remote.Client
  ( API
  , fetchFacilities
  , fetchFacility
  , fetchDisruptions
  , fetchStation
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Network.HTTP.Client     (Manager)
import           Servant.API
import           Servant.Client

import           Elescore.Common.Types
import           Elescore.Remote.Types

type Host = String
type API a = ReaderT (ApiKey, Manager, Host) IO a

type FaStaAPI = "fasta" :> "v2" :> (FacilitiesAPI :<|> StationAPI)

type FacilitiesAPI = "facilities" :> Header "Authorization" ApiKey :> Get '[JSON] [Remote Facility]
                :<|> "facilities" :> Header "Authorization" ApiKey :> QueryParam "state" [Remote FacilityState] :> Get '[JSON] [Remote Disruption]
                :<|> "facilities" :> Header "Authorization" ApiKey :> Capture "facility id" Int :> Get '[JSON] (Remote Facility)

type StationAPI = "stations" :> Header "Authorization" ApiKey :> Capture "station id" Int :> Get '[JSON] (Remote Station)

fetchFacilities :: API (Either ServantError [Facility])
fetchFacilities = (fmap . fmap . fmap) unRemote (makeRequest facilities)

fetchDisruptions :: API (Either ServantError [Disruption])
fetchDisruptions = (fmap . fmap . fmap) unRemote . makeRequest . flip disruptions . Just $ [Remote Inactive, Remote Unknown]

fetchFacility :: Int -> API (Either ServantError Facility)
fetchFacility fid = fmap unRemote <$> makeRequest (`facility` fid)

fetchStation :: Int -> API (Either ServantError Station)
fetchStation sid = fmap unRemote <$> makeRequest (`station` sid)

makeRequest :: (Maybe ApiKey -> ClientM a) -> API (Either ServantError a)
makeRequest c = do
  (key, mgr, h) <- ask
  liftIO $ runClientM (c $ Just key) (ClientEnv mgr (BaseUrl Https h 443 "") Nothing)

facilities :: Maybe ApiKey -> ClientM [Remote Facility]
disruptions :: Maybe ApiKey -> Maybe [Remote FacilityState] -> ClientM [Remote Disruption]
facility :: Maybe ApiKey -> Int -> ClientM (Remote Facility)
station :: Maybe ApiKey -> Int -> ClientM (Remote Station)
((facilities :<|> disruptions :<|> facility) :<|> station) = client api

api :: Proxy FaStaAPI
api = Proxy
