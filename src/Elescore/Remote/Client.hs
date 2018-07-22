{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Remote.Client
  ( API
  , fetchDisruptedFacilities
  , fetchFacilities
  , fetchFacility
  , fetchStation
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Network.HTTP.Client     (Manager)
import           Servant.API
import           Servant.Client

import           Elescore.Remote.Types

type Host = String
type API a = ReaderT (ApiKey, Manager, Host) IO a

type FaStaAPI = "fasta" :> "v2" :> (FacilitiesAPI :<|> StationAPI)

type FacilitiesAPI = "facilities" :> Header "Authorization" ApiKey :> QueryParam "state" [FacilityState] :> Get '[JSON] [Facility]
                :<|> "facilities" :> Header "Authorization" ApiKey :> Capture "facility id" Integer :> Get '[JSON] Facility

type StationAPI = "stations" :> Header "Authorization" ApiKey :> Capture "station id" Integer :> Get '[JSON] Station

fetchDisruptedFacilities :: API (Either ServantError [Facility])
fetchDisruptedFacilities = fetchFacilities (Just [Inactive, Unknown])

fetchFacilities :: Maybe [FacilityState] -> API (Either ServantError [Facility])
fetchFacilities = makeRequest . flip facilities

fetchFacility :: Integer -> API (Either ServantError Facility)
fetchFacility fid = makeRequest (`facility` fid)

fetchStation :: Integer -> API (Either ServantError Station)
fetchStation sid = makeRequest (`station` sid)

makeRequest :: (Maybe ApiKey -> ClientM a) -> API (Either ServantError a)
makeRequest c = do
  (key, mgr, h) <- ask
  liftIO $ runClientM (c $ Just key) (ClientEnv mgr (BaseUrl Https h 443 "") Nothing)

facilities :: Maybe ApiKey -> Maybe [FacilityState] -> ClientM [Facility]
facility :: Maybe ApiKey -> Integer -> ClientM Facility
station :: Maybe ApiKey -> Integer -> ClientM Station
((facilities :<|> facility) :<|> station) = client api

api :: Proxy FaStaAPI
api = Proxy
