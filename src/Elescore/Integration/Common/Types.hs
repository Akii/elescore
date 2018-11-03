{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Elescore.Integration.Common.Types where

import           ClassyPrelude             hiding (toLower)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                 (toLower)
import           Data.Data
import           Pipes

import           Database.SimpleEventStore (HasNamespace (..),
                                            PersistableEvent (..))
import           Elescore.IdTypes

data Source m a = Source
  { disruptionEvents :: Producer (DisruptionEvent a) m ()
  , facilityEvents   :: Producer (FacilityEvent a) m ()
  , objectEvents     :: Producer (ObjectEvent a) m ()
  }

data DisruptionEvent a
  = FacilityDisrupted { deFacilityId :: FacilityId a, deReason :: Reason }
  | DisruptionReasonUpdated { deFacilityId :: FacilityId a, deReason :: Reason }
  | FacilityRestored { deFacilityId :: FacilityId a }
  deriving (Show, Generic, Data)

data FacilityEvent a
  = FacilityIdentified { feFacilityId :: FacilityId a, feFacilityType :: FacilityType, feDescription :: Text }
  | FacilityTypeChanged { feFacilityId :: FacilityId a, feFacilityType :: FacilityType }
  | FacilityAssignedToObject { feFacilityId :: FacilityId a, feObjectId :: ObjectId a }
  | FacilityDescriptionUpdated { feFacilityId :: FacilityId a, feDescription :: Text }
  | FacilityLocated { feFacilityId :: FacilityId a, feGeoLocation :: GeoLocation }
  | FacilityAddressUpdated { feFacilityId :: FacilityId a, feAddress :: Address }
  | FacilityDeleted { feFacilityId :: FacilityId a }
  deriving (Show, Generic, Data)

data ObjectEvent a
  = ObjectIdentified { oeObjectId :: ObjectId a, oeDescription :: Text }
  | ObjectDescriptionUpdated { oeObjectId :: ObjectId a, oeDescription :: Text }
  | ObjectLocated { oeObjectId :: ObjectId a, oeGeoLocation :: GeoLocation }
  | ObjectAddressUpdated { oeObjectId :: ObjectId a, oeAddress :: Address }
  | ObjectDeleted { oeObjectId :: ObjectId a }
  deriving (Show, Generic, Data)

data Reason
  = UnderConstruction
  | UnderMaintenance
  | MonitoringDisrupted
  | NotAvailable
  | MonitoringNotAvailable
  | NoneGiven
  | Unknown Text
  deriving (Eq, Data, Show)

data FacilityType
  = Elevator
  | Escalator
  deriving (Eq, Data, Show)

data Address = Address
  { addrStreet      :: Maybe Text
  , addrHouseNumber :: Maybe Text
  , addrZipCode     :: Maybe Text
  , addrCity        :: Maybe Text
  } deriving (Eq, Data, Show)

data GeoLocation = GeoLocation
  { lat :: Double
  , lng :: Double
  } deriving (Eq, Data, Show)

----------------------
-- JSON instances
----------------------

instance HasNamespace (DisruptionEvent a) where
  getNamespace = "de.elescore.integration.v1"

instance HasNamespace (FacilityEvent a) where
  getNamespace = "de.elescore.integration.v1"

instance HasNamespace (ObjectEvent a) where
  getNamespace = "de.elescore.integration.v1"

instance Data a => PersistableEvent (DisruptionEvent a)
instance Data a => PersistableEvent (FacilityEvent a)
instance Data a => PersistableEvent (ObjectEvent a)

instance ToJSON (DisruptionEvent a) where
  toJSON = genericToJSON options

instance FromJSON (DisruptionEvent a) where
  parseJSON = genericParseJSON options

instance ToJSON (FacilityEvent a) where
  toJSON = genericToJSON options

instance FromJSON (FacilityEvent a) where
  parseJSON = genericParseJSON options

instance ToJSON (ObjectEvent a) where
  toJSON = genericToJSON options

instance FromJSON (ObjectEvent a) where
  parseJSON = genericParseJSON options

options :: Options
options =
  defaultOptions
  { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 2
  , constructorTagModifier = ("de.elescore.integration.v1." <>)
  }

instance ToJSON Reason where
  toJSON a = case a of
    UnderConstruction      -> "UnderConstruction"
    UnderMaintenance       -> "UnderMaintenance"
    MonitoringDisrupted    -> "MonitoringDisrupted"
    NotAvailable           -> "NotAvailable"
    MonitoringNotAvailable -> "MonitoringNotAvailable"
    NoneGiven              -> "NoneGiven"
    Unknown r              -> String r

instance FromJSON Reason where
  parseJSON = withText "the reason" $ \s ->
    return $ case s of
      "UnderConstruction"      -> UnderConstruction
      "UnderMaintenance"       -> UnderMaintenance
      "MonitoringDisrupted"    -> MonitoringDisrupted
      "NotAvailable"           -> NotAvailable
      "MonitoringNotAvailable" -> MonitoringNotAvailable
      "NoneGiven"              -> NoneGiven
      a                        -> Unknown a

concat <$> mapM
  (deriveJSON defaultOptions {unwrapUnaryRecords = True})
  [''FacilityType, ''GeoLocation]

deriveJSON (defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 4 }) ''Address
