{-# LANGUAGE RecordWildCards #-}
module Elescore.Projection.Disruption
  ( DisruptionProjection(..)
  , Disruption(..)
  , emptyDisruptionProjection
  , applyDisruptionEvent
  , mkDisruption
  ) where

import           ClassyPrelude
import           Data.DateTime
import qualified Data.IntMap               as IM

import           Database.SimpleEventStore
import           Elescore.IdTypes
import           Elescore.Integration

data DisruptionProjection = DP
  { dpActiveDisruptions :: Map FacilityId Int
  , dpDisruptions       :: IntMap Disruption
  } deriving (Show)

data Disruption = Disruption
  { dId         :: Int
  , dFacilityId :: FacilityId
  , dOccurredOn :: DateTime
  , dUpdatedOn  :: Maybe DateTime
  , dResolvedOn :: Maybe DateTime
  , dReason     :: Maybe Reason
  } deriving (Show)

emptyDisruptionProjection :: DisruptionProjection
emptyDisruptionProjection = DP mempty mempty

applyDisruptionEvent :: PersistedEvent (DisruptionEvent a) -> DisruptionProjection -> DisruptionProjection
applyDisruptionEvent PersistedEvent {..} DP {..} =
  let someFacilityId = deFacilityId evPayload
      disruptionId =
        fromMaybe
          (length dpDisruptions + 1)
          (lookup someFacilityId dpActiveDisruptions)
      disruption = mkDisruption disruptionId evOccurredOn evPayload
      activeDisruptions' =
        if isJust (dResolvedOn disruption)
          then deleteMap someFacilityId dpActiveDisruptions
          else insertMap someFacilityId disruptionId dpActiveDisruptions
      disruptions' = IM.insertWith mergeDisruption disruptionId disruption dpDisruptions
  in DP activeDisruptions' disruptions'

mkDisruption :: Int -> DateTime -> DisruptionEvent a -> Disruption
mkDisruption i dt ev = case ev of
  FacilityDisrupted fid r       -> Disruption i fid dt Nothing Nothing (Just r)
  DisruptionReasonUpdated fid r -> Disruption i fid dt (Just dt) Nothing (Just r)
  FacilityRestored fid          -> Disruption i fid dt Nothing (Just dt) Nothing

mergeDisruption :: Disruption -> Disruption -> Disruption
mergeDisruption new old = Disruption
      (dId new)
      (dFacilityId new)
      (dOccurredOn old)
      (dUpdatedOn new <|> dUpdatedOn old)
      (dResolvedOn new <|> dResolvedOn old)
      (dReason new <|> dReason old)
