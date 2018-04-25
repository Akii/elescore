{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Elescore.Remote.Types where

import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types      (Parser)
import           Servant

import           Elescore.Common.Types

type Disruptions = Map FacilityId Disruption

data Change
  = New
  | Updated
  | Deleted
  deriving (Show, Eq)

data DisruptionEvent = DisruptionEvent
  { devDisruption :: !Disruption
  , devChange     :: !Change
  }

data Disruption = Disruption
  { disFacilityId    :: FacilityId
  , disStationId     :: StationId
  , disFacilityState :: FacilityState
  , disReason        :: Maybe Text
  } deriving (Eq, Show)

newtype Remote a = Remote
  { unRemote :: a
  }

instance FromJSON (Remote Station) where
  parseJSON = withObject "the station" $ \o -> do
    sId <- StationId <$> (o .: "stationnumber")
    sName <- o .: "name"
    sFacilities <- foldr (liftA2 insertMap fId id . unRemote) mempty <$> (o .: "facilities" :: Parser [Remote Facility])

    return . Remote $ Station {..}

instance FromJSON (Remote Facility) where
  parseJSON = withObject "the facility" $ \o -> do
    fId <- FacilityId <$> (o .: "equipmentnumber")
    fStationId <- StationId <$> (o .: "stationnumber")
    fType <- remoteFacilityType =<< o .: "type"
    fDescription <- o .:? "description"
    fGeoCoordinates <- remoteGeoCoordinate o

    return . Remote $ Facility {..}

    where
      remoteGeoCoordinate :: Object -> Parser (Maybe Point)
      remoteGeoCoordinate o = do
        latitude <- o .:? "geocoordX"
        longitude <- o .:? "geocoordY"
        return $ liftA2 Point latitude longitude

instance FromJSON (Remote Disruption) where
  parseJSON = withObject "the disruption" $ \o -> do
    disFacilityId <- FacilityId <$> (o .: "equipmentnumber")
    disStationId <- StationId <$> (o .: "stationnumber")
    disFacilityState <- remoteFacilityState =<< o .: "state"
    disReason <- o .:? "stateExplanation"

    return . Remote $ Disruption {..}

remoteFacilityType :: Value -> Parser FacilityType
remoteFacilityType = withText "the type" $ \s ->
  return $ if s == "ELEVATOR"
           then Elevator
           else Escalator

remoteFacilityState :: Value -> Parser FacilityState
remoteFacilityState = withText "the state" $ \s ->
  return $ case s of
    "ACTIVE"   -> Active
    "INACTIVE" -> Inactive
    _          -> Unknown

instance ToHttpApiData (Remote FacilityState) where
  toQueryParam (Remote Active)   = "ACTIVE"
  toQueryParam (Remote Inactive) = "INACTIVE"
  toQueryParam (Remote Unknown)  = "UNKNOWN"

instance ToHttpApiData [Remote FacilityState] where
  toQueryParam = intercalate "," . fmap toQueryParam

newtype ApiKey =
  ApiKey Text
  deriving (Eq)

instance IsString ApiKey where
  fromString = ApiKey . pack

instance ToHttpApiData ApiKey where
  toHeader (ApiKey key) = encodeUtf8 ("Bearer " <> key)
  toQueryParam (ApiKey key) = key

deriveJSON defaultOptions ''Disruption
deriveJSON defaultOptions ''DisruptionEvent
deriveJSON defaultOptions ''Change
