{-# LANGUAGE TemplateHaskell #-}

module Elescore.Projection.Objects
  ( Objects
  , Facilities
  , Object(..)
  , Facility(..)
  , applyObjectEvent
  , applyFacilityEvent
  ) where

import           ClassyPrelude                     hiding (toLower)
import           Control.Lens

import           Elescore.IdTypes
import           Elescore.Integration.Common.Types

type Objects = Map SomeObjectId Object
type Facilities = Map SomeFacilityId Facility

data Object = Object
  { oId          :: SomeObjectId
  , oDescription :: Text
  } deriving (Eq, Show)

data Facility = Facility
  { fId             :: SomeFacilityId
  , fObjectId       :: Maybe SomeObjectId
  , fType           :: FacilityType
  , fDescription    :: Text
  , fGeoCoordinates :: Maybe GeoLocation
  } deriving (Eq, Show)

makeLensesFor
  [ ("fObjectId", "_fObjectId")
  , ("fType", "_fType")
  , ("fDescription", "_fDescription")
  , ("fGeoCoordinates", "_fGeoCoordinates")
  ]
  ''Facility

applyObjectEvent :: ObjectEvent a -> Objects -> Objects
applyObjectEvent ev = case ev of
  (ObjectIdentified oid desc)           -> at (toSomeObjectId oid) ?~ Object (toSomeObjectId oid) desc
  (ObjectDescriptionUpdated oid desc)   -> at (toSomeObjectId oid) ?~ Object (toSomeObjectId oid) desc
  ObjectLocated {}                      -> id
  ObjectAddressUpdated {}               -> id
  ObjectDeleted {}                      -> id

applyFacilityEvent :: FacilityEvent a -> Facilities -> Facilities
applyFacilityEvent ev = case ev of
  (FacilityIdentified fid ft desc)      -> at (toSomeFacilityId fid) ?~ newFacility fid ft desc
  (FacilityTypeChanged fid ft)          -> at (toSomeFacilityId fid) . _Just . _fType .~ ft
  (FacilityAssignedToObject fid oid)    -> at (toSomeFacilityId fid) . _Just . _fObjectId ?~ toSomeObjectId oid
  (FacilityDescriptionUpdated fid desc) -> at (toSomeFacilityId fid) . _Just . _fDescription .~ desc
  (FacilityLocated fid geo)             -> at (toSomeFacilityId fid) . _Just . _fGeoCoordinates ?~ geo
  FacilityAddressUpdated {}             -> id
  FacilityDeleted {}                    -> id

newFacility :: FacilityId a -> FacilityType -> Text -> Facility
newFacility fid ft desc = Facility (toSomeFacilityId fid) Nothing ft desc Nothing

