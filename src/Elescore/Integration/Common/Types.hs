{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module Elescore.Integration.Common.Types where

import           ClassyPrelude             hiding (toLower)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                 (toLower)
import           Data.Data
import           Pipes
import qualified Pipes.Prelude             as P

import           Database.SimpleEventStore (HasNamespace (..), HasStream (..),
                                            PersistableEvent (..),
                                            PersistedEvent)
import           Elescore.IdTypes

data All = All deriving Data

instance HasStream (DisruptionEvent All) where
  getStream = "Disruptions"

instance HasStream (FacilityEvent All) where
  getStream = "Facilities"

instance HasStream (ObjectEvent All) where
  getStream = "Objects"

data Source m a = Source
  { disruptionEvents :: Producer (PersistedEvent (DisruptionEvent a)) m ()
  , facilityEvents   :: Producer (PersistedEvent (FacilityEvent a)) m ()
  , objectEvents     :: Producer (PersistedEvent (ObjectEvent a)) m ()
  }

instance Monad m => Functor (Source m) where
  fmap f Source {..} =
    Source
    { disruptionEvents = disruptionEvents >-> P.map (fmap (fmap f))
    , facilityEvents = facilityEvents >-> P.map (fmap (fmap f))
    , objectEvents = objectEvents >-> P.map (fmap (fmap f))
    }

data DisruptionEvent a
  = FacilityDisrupted { deFacilityId :: FacilityId a, deReason :: Reason }
  | DisruptionReasonUpdated { deFacilityId :: FacilityId a, deReason :: Reason }
  | FacilityRestored { deFacilityId :: FacilityId a }
  deriving (Show, Generic, Data, Functor)

data FacilityEvent a
  = FacilityIdentified { feFacilityId :: FacilityId a, feFacilityType :: FacilityType, feDescription :: Text }
  | FacilityTypeChanged { feFacilityId :: FacilityId a, feFacilityType :: FacilityType }
  | FacilityAssignedToObject { feFacilityId :: FacilityId a, feObjectId :: ObjectId a }
  | FacilityDescriptionUpdated { feFacilityId :: FacilityId a, feDescription :: Text }
  | FacilityLocated { feFacilityId :: FacilityId a, feGeoLocation :: GeoLocation }
  | FacilityAddressUpdated { feFacilityId :: FacilityId a, feAddress :: Address }
  | FacilityDeleted { feFacilityId :: FacilityId a }
  deriving (Show, Generic, Data, Functor)

data ObjectEvent a
  = ObjectIdentified { oeObjectId :: ObjectId a, oeDescription :: Text }
  | ObjectDescriptionUpdated { oeObjectId :: ObjectId a, oeDescription :: Text }
  | ObjectLocated { oeObjectId :: ObjectId a, oeGeoLocation :: GeoLocation }
  | ObjectAddressUpdated { oeObjectId :: ObjectId a, oeAddress :: Address }
  | ObjectDeleted { oeObjectId :: ObjectId a }
  deriving (Show, Generic, Data, Functor)

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
