{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Elescore.IdTypes where

import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.Encoding
import           Data.Aeson.TH
import           Data.Aeson.Types    (FromJSONKey (..), ToJSONKey (..),
                                      ToJSONKeyFunction (ToJSONKeyText))
import           Data.Data
import Servant

newtype ObjectId = ObjectId
  { getObjectId :: Text
  } deriving (Generic, NFData, Eq, Ord, Data, Show)

newtype FacilityId = FacilityId
  { getFacilityId :: Text
  } deriving (Generic, NFData, Eq, Ord, Data, Show)

instance FromJSONKey ObjectId where
  fromJSONKey = ObjectId <$> fromJSONKey

instance ToJSONKey ObjectId where
  toJSONKey = ToJSONKeyText f g
    where
      f (ObjectId i) = i
      g = text . f

instance FromJSONKey FacilityId where
  fromJSONKey = FacilityId <$> fromJSONKey

instance ToJSONKey FacilityId where
  toJSONKey = ToJSONKeyText f g
    where
      f (FacilityId i) = i
      g = text . f

instance FromHttpApiData FacilityId where
  parseUrlPiece = pure . FacilityId

instance ToHttpApiData FacilityId where
  toUrlPiece = getFacilityId

concat <$> mapM
  (deriveJSON defaultOptions {unwrapUnaryRecords = True})
  [''ObjectId, ''FacilityId]
