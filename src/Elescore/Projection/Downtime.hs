module Elescore.Projection.Downtime
  ( Downtime
  , AggregatedDowntime
  , Downtimes
  , SumOfDowntimes
  , Day
  , Month
  , Year
  , computeDowntimes
  , extractRange
  , sumOfDowntimes
  ) where

import           ClassyPrelude                  hiding (Day)
import           Control.Lens
import           Data.DateTime                  hiding (toGregorian)
import           Data.Map                       (filterWithKey, elems)
import           Data.Time.Clock

import           Elescore.IdTypes                (SomeFacilityId)
import           Elescore.Projection.Disruption

type Downtime = Integer
type AggregatedDowntime a = Map (PointInTime a) Downtime
type Downtimes a = Map SomeFacilityId (AggregatedDowntime a)
type SumOfDowntimes = Map SomeFacilityId Downtime

computeDowntimes :: (Ord a, Granularity a) => DateTime -> IntMap Disruption -> Downtimes a
computeDowntimes currT = foldl' (flip apply) mempty
  where
    apply :: (Ord a, Granularity a) => Disruption -> Downtimes a -> Downtimes a
    apply d dt =
      let disDuration = diffSeconds (fromMaybe currT $ dResolvedOn d) (dOccurredOn d)
          pits = distributeDuration (dOccurredOn d) disDuration
      in foldl' (\b (pit,dur) -> over (at (dFacilityId d) . non mempty) (insertWith (+) pit dur) b) dt pits

    distributeDuration :: (Ord a, Granularity a) => DateTime -> Integer -> [(PointInTime a, Integer)]
    distributeDuration dt n
      | n <= 0 = []
      | n <= secondsUntilNextDay = [(toPointInTime dt, n `div` 60)]
      | otherwise = (toPointInTime dt, secondsUntilNextDay `div` 60) : distributeDuration (addSeconds secondsUntilNextDay dt) (n - secondsUntilNextDay)

      where
        secondsUntilNextDay :: Integer
        secondsUntilNextDay = 86400 - floor (fromIntegral (diffTimeToPicoseconds (utctDayTime dt)) * 1e-12 :: Double)

extractRange :: (Ord a, Granularity a) => DateTime -> DateTime -> Downtimes a -> Downtimes a
extractRange start end = fmap (filterWithKey (\k _ -> inRange k pitStart pitEnd))
  where
    pitStart = toPointInTime start
    pitEnd = toPointInTime end

    inRange :: Ord a => PointInTime a -> PointInTime a -> PointInTime a -> Bool
    inRange a s e = a >= s && a <= e

sumOfDowntimes :: Downtimes a -> SumOfDowntimes
sumOfDowntimes = fmap (sum . elems)

-- granularities
data Day = Day deriving (Eq, Ord, Show)
data Month = Month deriving (Eq, Ord, Show)
data Year = Year deriving (Eq, Ord, Show)

class Granularity a where
  toPointInTime :: DateTime -> PointInTime a

instance Granularity Day where
  toPointInTime = toDay

instance Granularity Month where
  toPointInTime = toMonth

instance Granularity Year where
  toPointInTime = toYear

newtype PointInTime a = PIT Text
  deriving (Show)

instance Eq a => Eq (PointInTime a) where
  PIT a == PIT b = a == b

instance Eq a => Ord (PointInTime a) where
  PIT a <= PIT b = a <= b

toDay :: DateTime -> PointInTime Day
toDay dt =
  let (year, month, day) = toGregorian (utctDay dt)
      formatted = mconcat [tshow year, "-", prependZero month, "-", prependZero day]
  in PIT formatted

toMonth :: DateTime -> PointInTime Month
toMonth dt =
  let (year, month, _) = toGregorian (utctDay dt)
      formatted = mconcat [tshow year, "-", prependZero month]
  in PIT formatted

toYear :: DateTime -> PointInTime Year
toYear dt =
  let (year, _, _) = toGregorian (utctDay dt)
  in PIT (tshow year)

prependZero :: Int -> Text
prependZero a
  | a < 10 = "0" <> tshow a
  | otherwise = tshow a
