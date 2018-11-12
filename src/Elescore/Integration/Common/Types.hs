{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module Elescore.Integration.Common.Types where

import           ClassyPrelude             hiding (toLower)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                 (toLower)
import           Data.Data
import           Pipes.Concurrent          (Input)

import           Database.SimpleEventStore (HasNamespace (..), HasStream (..),
                                            PersistableEvent (..),
                                            PersistedEvent)
import           Elescore.IdTypes

data Integration
  = DB
  | Bogestra
  | All

instance HasStream (DisruptionEvent 'DB) where
  getStream = "Disruptions.DB"

instance HasStream (FacilityEvent 'DB) where
  getStream = "Facilities.DB"

instance HasStream (ObjectEvent 'DB) where
  getStream = "Objects.DB"

instance HasStream (DisruptionEvent 'Bogestra) where
  getStream = "Disruptions.Bogestra"

instance HasStream (FacilityEvent 'Bogestra) where
  getStream = "Facilities.Bogestra"

instance HasStream (ObjectEvent 'Bogestra) where
  getStream = "Objects.Bogestra"

data Source (a :: Integration) = Source
  { disruptionEvents :: Input (PersistedEvent (DisruptionEvent a))
  , facilityEvents   :: Input (PersistedEvent (FacilityEvent a))
  , objectEvents     :: Input (PersistedEvent (ObjectEvent a))
  }

instance Semigroup (Source a) where
  s1 <> s2 =
    Source
    { disruptionEvents = disruptionEvents s1 <> disruptionEvents s2
    , facilityEvents = facilityEvents s1 <> facilityEvents s2
    , objectEvents = objectEvents s1 <> objectEvents s2
    }

instance Monoid (Source a) where
  mempty = Source mempty mempty mempty

data DisruptionEvent (a :: Integration)
  = FacilityDisrupted { deFacilityId :: FacilityId, deReason :: Reason }
  | DisruptionReasonUpdated { deFacilityId :: FacilityId, deReason :: Reason }
  | FacilityRestored { deFacilityId :: FacilityId }
  deriving (Eq, Show, Generic, Data)

data FacilityEvent (a :: Integration)
  = FacilityIdentified { feFacilityId :: FacilityId, feFacilityType :: FacilityType, feDescription :: Text }
  | FacilityTypeChanged { feFacilityId :: FacilityId, feFacilityType :: FacilityType }
  | FacilityAssignedToObject { feFacilityId :: FacilityId, feObjectId :: ObjectId }
  | FacilityDescriptionUpdated { feFacilityId :: FacilityId, feDescription :: Text }
  | FacilityLocated { feFacilityId :: FacilityId, feGeoLocation :: GeoLocation }
  | FacilityAddressUpdated { feFacilityId :: FacilityId, feAddress :: Address }
  | FacilityDeleted { feFacilityId :: FacilityId }
  deriving (Eq, Show, Generic, Data)

data ObjectEvent (a :: Integration)
  = ObjectIdentified { oeObjectId :: ObjectId, oeDescription :: Text }
  | ObjectDescriptionUpdated { oeObjectId :: ObjectId, oeDescription :: Text }
  | ObjectLocated { oeObjectId :: ObjectId, oeGeoLocation :: GeoLocation }
  | ObjectAddressUpdated { oeObjectId :: ObjectId, oeAddress :: Address }
  | ObjectDeleted { oeObjectId :: ObjectId }
  deriving (Eq, Show, Generic, Data)

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

instance Typeable a => PersistableEvent (DisruptionEvent a)
instance Typeable a => PersistableEvent (FacilityEvent a)
instance Typeable a => PersistableEvent (ObjectEvent a)

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
