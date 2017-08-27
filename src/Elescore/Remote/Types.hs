{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elescore.Remote.Types where

import           ClassyPrelude              hiding (fromList)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.DateTime
import           Data.Map                   (fromList)
import           Servant

import           Elescore.Disruptions.Types

newtype ApiKey =
  ApiKey Text
  deriving (Eq)

instance IsString ApiKey where
  fromString = ApiKey . pack

instance ToHttpApiData ApiKey where
  toHeader (ApiKey key) = encodeUtf8 ("Bearer " <> key)
  toQueryParam (ApiKey key) = key

type Disruptions = Map DisruptionId DisruptionData

newtype RemoteDisruption = RemoteDisruption
  { unRemoteDisruption :: DisruptionData
  }

newtype RemoteStation = RemoteStation
  { unRemoteStation :: StationData RemoteFacility
  }

newtype RemoteFacility = RemoteFacility
  { unRemoteFacility :: FacilityData
  }

-- kept fields so that no migration of the disruption log is necessary
data DisruptionData = DisruptionData
  { disId                 :: !DisruptionId
  , disEquipmentId        :: !FacilityId
  , disStationId          :: !StationId
  , disType               :: !FacilityType
  , disState              :: !(Maybe FacilityState)
  , disDescription        :: !(Maybe Text)
  , disReason             :: !(Maybe Text)
  , disFurtherDescription :: !(Maybe Text)
  , disGeoCoordinates     :: !(Maybe Point)
  , disPlannedCompletion  :: !(Maybe DateTime)
  , disOutOfServiceOn     :: !(Maybe DateTime)
  , disOutOfServiceTo     :: !(Maybe DateTime)
  , disLastUpdate         :: !(Maybe DateTime)
  } deriving (Eq)

data StationData a = StationData
  { sdStationId  :: !StationId
  , sdName       :: !Text
  , sdFacilities :: ![a]
  } deriving (Functor)

data FacilityData = FacilityData
  { fdFacilityId     :: !FacilityId
  , fdStationId      :: !StationId
  , fdType           :: !FacilityType
  , fdState          :: !FacilityState
  , fdDescription    :: !(Maybe Text)
  , fdGeoCoordinates :: !(Maybe Point)
  }

toDisruption :: DisruptionData -> Disruption
toDisruption DisruptionData {..} =
  let disFacilityId = disEquipmentId
      disFacilityState = disState
      disFacilityType = disType
  in Disruption {..}

toStation :: StationData FacilityData -> Station
toStation StationData {..} =
  let sId = sdStationId
      sName = Just sdName
      sFacilities = fromList $ (liftA2 (,) fdFacilityId toFacility <$> sdFacilities)
  in Station {..}

toFacility :: FacilityData -> Facility
toFacility FacilityData {..} =
  let fId = fdFacilityId
      fStationId = fdStationId
      fType = fdType
      fGeoCoordinates = fdGeoCoordinates
      fDescription = fdDescription
  in Facility {..}

instance FromJSON RemoteDisruption where
  parseJSON = withObject "the disruption" $ \o -> do
    disId <- o .: "disruptionnumber"
    disEquipmentId <- o .: "equipmentnumber"
    disStationId <- o .: "stationnumber"
    disType <- remoteFacilityType =<< o .: "type"
    disState <- toMaybeParser remoteFacilityState =<< o .:? "state"
    disDescription <- o .:? "description"
    disReason <- o .:? "outOfServiceReason"
    disFurtherDescription <- o .:? "furtherDescription"
    disGeoCoordinates <- toMaybeParser remoteGeoCoordinate =<< o .:? "geographicCoordinates"
    disPlannedCompletion <- o .:? "plannedCompletion"
    disOutOfServiceOn <- o .:? "outOfServiceOn"
    disOutOfServiceTo <- o .:? "outOfServiceTo"
    disLastUpdate <- o .:? "lastUpdate"

    return $ RemoteDisruption  DisruptionData {..}

    where
      remoteGeoCoordinate = withObject "the point" $ \o -> do
        [lat,lng] <- o .: "coordinates"
        return (Point lat lng)

instance FromJSON RemoteStation where
  parseJSON = withObject "the station" $ \o -> do
    sdStationId <- o .: "stationnumber"
    sdName <- o .: "name"
    sdFacilities <- o .: "facilities"

    return $ RemoteStation StationData {..}

instance FromJSON RemoteFacility where
  parseJSON = withObject "the facility" $ \o -> do
    fdFacilityId <- o .: "equipmentnumber"
    fdStationId <- o .: "stationnumber"
    fdType <- remoteFacilityType =<< o .: "type"
    fdState <- remoteFacilityState =<< o .: "state"
    fdDescription <- o .:? "description"
    fdGeoCoordinates <- remoteGeoCoordinate o

    return $ RemoteFacility FacilityData {..}

    where
      remoteGeoCoordinate :: Object -> Parser (Maybe Point)
      remoteGeoCoordinate o = do
        latitude <- o .:? "geocoordX"
        longitude <- o .:? "geocoordY"
        return $ liftA2 Point latitude longitude

remoteFacilityType :: Value -> Parser FacilityType
remoteFacilityType = withText "the type" $ \s ->
  return $ if s == "ELEVATOR"
           then Elevator
           else Escalator

remoteFacilityState :: Value -> Parser FacilityState
remoteFacilityState = withText "the state" $ \s ->
  return $ if s == "ACTIVE"
           then Active
           else Inactive

toMaybeParser :: (a -> Parser b) -> Maybe a -> Parser (Maybe b)
toMaybeParser _ Nothing  = return Nothing
toMaybeParser p (Just v) = return $ parseMaybe p v

concat <$> mapM
  (deriveJSON defaultOptions)
  [''DisruptionData, ''StationData, ''FacilityData]
