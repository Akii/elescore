{-# LANGUAGE TemplateHaskell #-}

module Elescore.Disruptions.History
  ( History
  , Progress(..)
  , ProgressEvent(..)
  , loadHistory
  , applyEventToHistory
  ) where

import           ClassyPrelude
import           Data.Aeson.TH
import           Data.DateTime

import           Elescore.Disruptions.Types hiding (disId, disStationId)
import           Elescore.Remote.Monitoring (DisruptionEvent (..))
import qualified Elescore.Remote.Monitoring as RM
import           Elescore.Remote.Types      (DisruptionData (..))

type History = Map DisruptionId Progress

data Progress = Progress
  { dpDisruptionId  :: !DisruptionId
  , dpStationId     :: !StationId
  , dpFacilityId    :: !FacilityId
  , dpReportedOn    :: !DateTime
  , dpLastUpdatedOn :: !DateTime
  , dpResolvedOn    :: !(Maybe DateTime)
  , dpCurrentState  :: !ProgressEvent
  , dpEvents        :: ![(DateTime, ProgressEvent)]
  }

data ProgressEvent
  = Reported
  | Updated
  | Resolved
  | Reopened
  deriving (Eq)

loadHistory :: [DisruptionEvent] -> History
loadHistory = foldl' applyEventToHistory mempty

applyEventToHistory :: History -> DisruptionEvent -> History
applyEventToHistory h ev = insertWith (flip combineProgress) (disId . devDisruption $ ev) (mkDisruptionProgress h ev) h

mkDisruptionProgress :: History -> DisruptionEvent -> Progress
mkDisruptionProgress h ev =
  let disruption = devDisruption ev
      disruptionId = disId disruption
      occurredOn = devOccurredOn ev
      disruptionExists = member disruptionId h
      progressEvent =
        case devChangeType ev of
          RM.New ->
            if disruptionExists
              then Reopened
              else Reported
          RM.Updated -> Updated
          RM.Resolved -> Resolved
      resolvedOn =
        case progressEvent of
          Resolved -> Just occurredOn
          _        -> Nothing
  in Progress
       disruptionId
       (disStationId disruption)
       (disEquipmentId disruption)
       occurredOn
       occurredOn
       resolvedOn
       progressEvent
       [(occurredOn, progressEvent)]

combineProgress :: Progress -> Progress -> Progress
combineProgress dp1 dp2 =
  dp1
  { dpLastUpdatedOn = dpLastUpdatedOn dp2
  , dpResolvedOn = dpResolvedOn dp2 <|> dpResolvedOn dp1
  , dpCurrentState = dpCurrentState dp2
  , dpEvents = dpEvents dp1 <> dpEvents dp2
  }

deriveToJSON defaultOptions ''Progress
deriveToJSON defaultOptions ''ProgressEvent
