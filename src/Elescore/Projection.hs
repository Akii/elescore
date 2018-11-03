module Elescore.Projection
  ( DisruptionProjection(..)
  , Disruption(..)
  , emptyDisruptionProjection
  , applyDisruptionEvent

  , SumOfDowntimes
  , computeDowntimes

  , Objects
  , Facilities
  , Object(..)
  , Facility(..)
  , applyObjectEvent
  , applyFacilityEvent
  ) where

import           Elescore.Projection.Disruption
import           Elescore.Projection.Downtime
import           Elescore.Projection.Objects
