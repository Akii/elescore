{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Integration.DB.Client
  ( API
  , Host
  , fetchDisruptedFacilities
  , fetchFacilities
  , fetchFacility
  , fetchStation
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Network.HTTP.Client           (Manager)
import           Servant.API
import           Servant.Client

import           Elescore.Integration.DB.Types

type Host = String
type API a = ReaderT (ApiKey, Manager, Host) IO (Either ServantError a)

type FaStaAPI = "fasta" :> "v2" :> (FacilitiesAPI :<|> StationAPI)

type FacilitiesAPI = "facilities" :> Header "Authorization" ApiKey :> QueryParam "state" [FacilityState] :> Get '[JSON] [Facility]
                :<|> "facilities" :> Header "Authorization" ApiKey :> Capture "facility id" Integer :> Get '[JSON] Facility

type StationAPI = "stations" :> Header "Authorization" ApiKey :> Capture "station id" Text :> Get '[JSON] Station

fetchDisruptedFacilities :: API [Facility]
fetchDisruptedFacilities = fetchFacilities (Just [Inactive, Unknown])

fetchFacilities :: Maybe [FacilityState] -> API [Facility]
fetchFacilities = makeRequest . flip facilities

fetchFacility :: Integer -> API Facility
fetchFacility fid = makeRequest (`facility` fid)

fetchStation :: Text -> API Station
fetchStation sid = makeRequest (`station` sid)

makeRequest :: (Maybe ApiKey -> ClientM a) -> API a
makeRequest c = do
  (key, mgr, h) <- ask
  liftIO $ runClientM (c $ Just key) (ClientEnv mgr (BaseUrl Https h 443 "") Nothing)

facilities :: Maybe ApiKey -> Maybe [FacilityState] -> ClientM [Facility]
facility :: Maybe ApiKey -> Integer -> ClientM Facility
station :: Maybe ApiKey -> Text -> ClientM Station
((facilities :<|> facility) :<|> station) = client api

api :: Proxy FaStaAPI
api = Proxy
