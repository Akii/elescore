{-# LANGUAGE RecordWildCards #-}

module Elescore.Projection.DisruptionsPerDay
  ( DisruptionsPerDay
  , applyDisruptionPerDayEvent
  ) where

import           ClassyPrelude
import           Data.DateTime                     hiding (toGregorian)

import           Database.SimpleEventStore
import           Elescore.IdTypes
import           Elescore.Integration.Common.Types
import           Statistics.IQR

type Date = Text
type DisruptionsPerDay = (Set FacilityId, Map Date Sample)

applyDisruptionPerDayEvent :: PersistedEvent (DisruptionEvent a) -> DisruptionsPerDay -> DisruptionsPerDay
applyDisruptionPerDayEvent PersistedEvent {..} =
  case evPayload of
    FacilityDisrupted fid _ -> addSample (insertSet fid) (toDate evOccurredOn)
    FacilityRestored fid    -> addSample (deleteSet fid) (toDate evOccurredOn)
    _                       -> id

addSample :: (Set FacilityId -> Set FacilityId) -> Date -> DisruptionsPerDay -> DisruptionsPerDay
addSample f date (set, xs) =
  let newSet = f set
      numberOfDisruptions = fromIntegral (length newSet)
      xs' = insertWith (<>) date (singletonSample numberOfDisruptions) xs
  in (newSet, xs')

toDate :: DateTime -> Date
toDate dt =
  let (year, month, day) = toGregorian (utctDay dt)
  in mconcat [tshow year, "-", prependZero month, "-", prependZero day]

prependZero :: Int -> Text
prependZero a
  | a < 10 = "0" <> tshow a
  | otherwise = tshow a
