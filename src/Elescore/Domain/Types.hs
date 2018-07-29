{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elescore.Domain.Types where

import           ClassyPrelude                    hiding (fromString,
                                                   getCurrentTime, toLower)
import           Control.Lens.TH
import           Data.Aeson.Encoding
import           Data.Aeson.TH
import           Data.Aeson.Types                 (FromJSONKey (..),
                                                   ToJSONKey (..),
                                                   ToJSONKeyFunction (ToJSONKeyText))
import           Data.Char                        (toLower)
import           Data.DateTime
import           Data.UUID
import           Data.UUID.V4                     (nextRandom)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField

newtype DisruptionId = DisruptionId
  { getDisruptionId :: UUID
  } deriving (Ord, Eq, Show)

newtype StationId = StationId
  { getStationId :: Integer
  } deriving (Ord, Eq, Show)

newtype FacilityId = FacilityId
  { getFacilityId :: Integer
  } deriving (Ord, Eq, Show)

data Disruption = Disruption
  { disStationId     :: StationId
  , disFacilityId    :: FacilityId
  , disFacilityState :: FacilityState
  , disReason        :: Maybe Text
  } deriving (Eq, Show)

data DisruptionEvent = DisruptionEvent
  { devId            :: DisruptionId
  , devStationId     :: StationId
  , devFacilityId    :: FacilityId
  , devFacilityState :: FacilityState
  , devReason        :: Maybe Text
  , devOccurredOn    :: DateTime
  , devChange        :: Change
  } deriving (Eq, Show)

data Station = Station
  { sId         :: StationId
  , sName       :: Text
  , sFacilities :: Map FacilityId Facility
  } deriving (Eq, Show)

data Facility = Facility
  { fId             :: FacilityId
  , fStationId      :: StationId
  , fType           :: FacilityType
  , fDescription    :: Maybe Text
  , fGeoCoordinates :: Maybe Point
  } deriving (Eq, Show)

data Change
  = New
  | Updated
  | Resolved
  deriving (Eq, Read, Show)

data FacilityType
  = Elevator
  | Escalator
  deriving (Eq, Read, Show)

data FacilityState
  = Active
  | Inactive
  | Unknown
  deriving (Eq, Read, Show)

data Point = Point
  { x :: Double
  , y :: Double
  } deriving (Eq, Show)

mkDisruptionEvent :: MonadIO m => Disruption -> Change -> m DisruptionEvent
mkDisruptionEvent Disruption {..} devChange = do
  devId <- nextDisruptionId
  devOccurredOn <- liftIO getCurrentTime

  let devStationId = disStationId
      devFacilityId = disFacilityId
      devFacilityState = disFacilityState
      devReason = disReason

  return DisruptionEvent {..}

toDisruption :: DisruptionEvent -> Disruption
toDisruption DisruptionEvent {..} =
  Disruption devStationId devFacilityId devFacilityState devReason

nextDisruptionId :: MonadIO m => m DisruptionId
nextDisruptionId = DisruptionId <$> liftIO nextRandom

-- DB instances

instance FromField UUID where
  fromField a = do
    muuid <- fromString <$> fromField a
    maybe (fail "Unable to map UUID from row") return muuid

instance ToField UUID where
  toField = toField . toString

instance FromField DisruptionId where
  fromField a = DisruptionId <$> fromField a

instance ToField DisruptionId where
  toField = toField . getDisruptionId

instance FromField FacilityId where
  fromField a = FacilityId <$> fromField a

instance ToField FacilityId where
  toField = toField . getFacilityId

instance FromField StationId where
  fromField a = StationId <$> fromField a

instance ToField StationId where
  toField = toField . getStationId

instance FromField Change where
  fromField a = do
    mc <- (readMay :: String -> Maybe Change) <$> fromField a
    case mc of
      Nothing -> fail "Unable to map Change from row"
      Just c  -> return c

instance ToField Change where
  toField = toField . show

instance FromField FacilityType where
  fromField a = do
    mft <- (readMay :: String -> Maybe FacilityType) <$> fromField a
    maybe (fail "Unable to map FacilityType from row") return mft

instance ToField FacilityType where
  toField = toField . show

instance FromField FacilityState where
  fromField a = do
    mfs <- (readMay :: String -> Maybe FacilityState) <$> fromField a
    maybe (fail "Unable to map FacilityState from row") return mfs

instance ToField FacilityState where
  toField = toField . show

instance FromRow DisruptionEvent where
  fromRow =
    DisruptionEvent <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DisruptionEvent where
  toRow DisruptionEvent {..} =
    toRow
      ( devId
      , devStationId
      , devFacilityId
      , devFacilityState
      , devReason
      , devOccurredOn
      , devChange)

instance FromRow Station where
  fromRow = Station <$> field <*> field <*> pure mempty

instance ToRow Station where
  toRow Station {..} = toRow (sId, sName)

instance FromRow Facility where
  fromRow = do
    fId <- field
    fStationId <- field
    fType <- field
    fDescription <- field
    geoX <- field
    geoY <- field
    let fGeoCoordinates = liftA2 Point geoX geoY
    return Facility {..}

instance ToRow Facility where
  toRow Facility {..} =
    toRow (fId, fStationId, fType, fDescription, geoX, geoY)

    where
      (geoX, geoY) = maybe
        (Nothing, Nothing)
        (liftA2 (,) (Just . x) (Just . y))
        fGeoCoordinates

-- JSON instances

concat <$> mapM
  (deriveJSON defaultOptions {unwrapUnaryRecords = True})
  [''StationId, ''FacilityId, ''DisruptionId, ''FacilityType, ''FacilityState, ''Point]

instance FromJSONKey StationId where
  fromJSONKey = StationId <$> fromJSONKey

instance ToJSONKey StationId where
  toJSONKey = ToJSONKeyText f g
    where
      f (StationId i) = tshow i
      g = text . f

instance FromJSONKey FacilityId where
  fromJSONKey = FacilityId <$> fromJSONKey

instance ToJSONKey FacilityId where
  toJSONKey = ToJSONKeyText f g
    where
      f (FacilityId i) = tshow i
      g = text . f

makeLensesFor
  [("sId", "_sId"), ("sName", "_sName"), ("sFacilities", "_sFacilities")]
  ''Station
makeLensesFor
  [ ("fId", "_fId")
  , ("fStationId", "_fStationId")
  , ("fType", "_fType")
  , ("fDescription", "_fDescription")
  , ("fGeoCoordinates", "_fGeoCoordinates")
  ]
  ''Facility
