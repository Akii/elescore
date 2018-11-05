{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Elescore.IdTypes where

import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.Encoding
import           Data.Aeson.TH
import           Data.Aeson.Types                  (FromJSONKey (..),
                                                    ToJSONKey (..),
                                                    ToJSONKeyFunction (ToJSONKeyText))
import           Data.Data

newtype SomeFacilityId = SomeFacilityId
  { getSomeFacilityId :: Text
  } deriving (Eq, Ord, Show)

newtype SomeObjectId = SomeObjectId
  { getSomeObjectId :: Text
  } deriving (Eq, Ord, Show)

newtype ObjectId a = ObjectId
  { getObjectId :: Text
  } deriving (Eq, Ord, Data, Show, Functor)

newtype FacilityId a = FacilityId
  { getFacilityId :: Text
  } deriving (Eq, Ord, Data, Show, Functor)

toSomeFacilityId :: FacilityId a -> SomeFacilityId
toSomeFacilityId = SomeFacilityId . getFacilityId

toSomeObjectId :: ObjectId a -> SomeObjectId
toSomeObjectId = SomeObjectId . getObjectId

instance ToJSON (ObjectId a) where
  toJSON = toJSON . getObjectId

instance FromJSON (ObjectId a) where
  parseJSON = withText "the objectId" (pure . ObjectId)

instance ToJSON (FacilityId a) where
  toJSON = toJSON . getFacilityId

instance FromJSON (FacilityId a) where
  parseJSON = withText "the objectId" (pure . FacilityId)

instance FromJSONKey SomeObjectId where
  fromJSONKey = SomeObjectId <$> fromJSONKey

instance ToJSONKey SomeObjectId where
  toJSONKey = ToJSONKeyText f g
    where
      f (SomeObjectId i) = i
      g = text . f

instance FromJSONKey SomeFacilityId where
  fromJSONKey = SomeFacilityId <$> fromJSONKey

instance ToJSONKey SomeFacilityId where
  toJSONKey = ToJSONKeyText f g
    where
      f (SomeFacilityId i) = i
      g = text . f

concat <$> mapM
  (deriveJSON defaultOptions {unwrapUnaryRecords = True})
  [''SomeObjectId, ''SomeFacilityId]
