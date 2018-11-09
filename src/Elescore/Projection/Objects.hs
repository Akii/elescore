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

type Objects = Map ObjectId Object
type Facilities = Map FacilityId Facility

data Object = Object
  { oId          :: ObjectId
  , oDescription :: Text
  } deriving (Eq, Show)

data Facility = Facility
  { fId             :: FacilityId
  , fObjectId       :: Maybe ObjectId
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
  (ObjectIdentified oid desc)           -> at oid ?~ Object oid desc
  (ObjectDescriptionUpdated oid desc)   -> at oid ?~ Object oid desc
  ObjectLocated {}                      -> id
  ObjectAddressUpdated {}               -> id
  ObjectDeleted {}                      -> id

applyFacilityEvent :: FacilityEvent a -> Facilities -> Facilities
applyFacilityEvent ev = case ev of
  (FacilityIdentified fid ft desc)      -> at fid ?~ newFacility fid ft desc
  (FacilityTypeChanged fid ft)          -> at fid . _Just . _fType .~ ft
  (FacilityAssignedToObject fid oid)    -> at fid . _Just . _fObjectId ?~ oid
  (FacilityDescriptionUpdated fid desc) -> at fid . _Just . _fDescription .~ desc
  (FacilityLocated fid geo)             -> at fid . _Just . _fGeoCoordinates ?~ geo
  FacilityAddressUpdated {}             -> id
  FacilityDeleted {}                    -> id

newFacility :: FacilityId -> FacilityType -> Text -> Facility
newFacility fid ft desc = Facility fid Nothing ft desc Nothing

