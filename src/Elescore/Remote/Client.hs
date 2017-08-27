{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Elescore.Remote.Client
  ( fetchDisruptions
  , fetchDisruption
  , fetchFacilities
  , fetchFacility
  , fetchStation
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Elescore.Remote.Types
import           Elescore.Types

type FaStaAPI = "fasta" :> "v1" :> (DisruptionsAPI :<|> FacilitiesAPI :<|> StationAPI)

type DisruptionsAPI = "disruptions" :> Header "Authorization" ApiKey :> Get '[JSON] [RemoteDisruption]
                 :<|> "disruptions" :> Header "Authorization" ApiKey :> Capture "disruption id" Int :> Get '[JSON] RemoteDisruption

type FacilitiesAPI = "facilities" :> Header "Authorization" ApiKey :> Get '[JSON] [RemoteFacility]
                :<|> "facilities" :> Header "Authorization" ApiKey :> Capture "facility id" Int :> Get '[JSON] RemoteFacility

type StationAPI = "stations" :> Header "Authorization" ApiKey :> Capture "station id" Int :> Get '[JSON] RemoteStation

fetchDisruptions :: Elescore (Either ServantError [DisruptionData])
fetchDisruptions = (fmap . fmap . fmap) unRemoteDisruption (makeRequest disruptions)

fetchDisruption :: Int -> Elescore (Either ServantError DisruptionData)
fetchDisruption did = (fmap . fmap) unRemoteDisruption $ makeRequest (`disruption` did)

fetchFacilities :: Elescore (Either ServantError [FacilityData])
fetchFacilities = (fmap . fmap . fmap) unRemoteFacility (makeRequest facilities)

fetchFacility :: Int -> Elescore (Either ServantError FacilityData)
fetchFacility fid = (fmap . fmap) unRemoteFacility $ makeRequest (`facility` fid)

fetchStation :: Int -> Elescore (Either ServantError (StationData FacilityData))
fetchStation sid =
  let stationData = (fmap . fmap) unRemoteStation $ makeRequest (`station` sid)
  in (fmap . fmap . fmap) unRemoteFacility stationData

makeRequest :: (Maybe ApiKey -> ClientM a) -> Elescore (Either ServantError a)
makeRequest c = do
  key <- apiKey
  mgr <- reqManager
  host <- opts optHost

  liftIO $ runClientM (c $ Just key) (ClientEnv mgr (BaseUrl Https host 443 ""))

disruptions :: Maybe ApiKey -> ClientM [RemoteDisruption]
disruption :: Maybe ApiKey -> Int -> ClientM RemoteDisruption
facilities :: Maybe ApiKey -> ClientM [RemoteFacility]
facility :: Maybe ApiKey -> Int -> ClientM RemoteFacility
station :: Maybe ApiKey -> Int -> ClientM RemoteStation
((disruptions :<|> disruption) :<|> (facilities :<|> facility) :<|> station) = client api

api :: Proxy FaStaAPI
api = Proxy
