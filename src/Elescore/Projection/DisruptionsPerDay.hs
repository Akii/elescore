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

type Date = Text
type DisruptionsPerDay = (Set FacilityId, Map Date Double)

applyDisruptionPerDayEvent :: PersistedEvent (DisruptionEvent a) -> DisruptionsPerDay -> DisruptionsPerDay
applyDisruptionPerDayEvent PersistedEvent {..} =
  case evPayload of
    FacilityDisrupted fid _ -> addDisruption fid (toDate evOccurredOn)
    FacilityRestored fid    -> removeDisruption fid (toDate evOccurredOn)
    _                       -> id

addDisruption :: FacilityId -> Date -> DisruptionsPerDay -> DisruptionsPerDay
addDisruption fid date (set, xs) =
  let newSet = insertSet fid set
      setLength = fromIntegral (length newSet)
      oldNum = findWithDefault setLength date xs
  in (newSet, insertMap date ((oldNum + setLength) / 2) xs)

removeDisruption :: FacilityId -> Date -> DisruptionsPerDay -> DisruptionsPerDay
removeDisruption fid date (set, xs) =
  let newSet = deleteSet fid set
      setLength = fromIntegral (length newSet)
      oldNum = findWithDefault setLength date xs
  in (newSet, insertMap date ((oldNum + setLength) / 2) xs)

toDate :: DateTime -> Date
toDate dt =
  let (year, month, day) = toGregorian (utctDay dt)
  in mconcat [tshow year, "-", prependZero month, "-", prependZero day]

prependZero :: Int -> Text
prependZero a
  | a < 10 = "0" <> tshow a
  | otherwise = tshow a
