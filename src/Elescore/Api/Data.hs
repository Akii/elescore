{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Elescore.Api.Data
  ( DataApi
  , dataServer
  ) where

import           ClassyPrelude                         hiding (Handler)
import           Data.DateTime                         (DateTime)
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Servant
import           Servant.Pagination

import           Elescore.Api.Types
import           Elescore.IdTypes
import           Elescore.Projection.DisruptionsPerDay (DisruptionsPerDay)
import           Statistics.IQR                        (average)

type AppliedRanges ranges a = Ranges ranges a
type PagReqHeaders ranges a = Header "Range" (AppliedRanges ranges a)
type PagRespHeaders ranges a = Header "Total-Count" Int ': PageHeaders ranges a

type DisruptionFields = '["id", "occurredOn", "updatedOn", "resolvedOn", "duration"]
type DisruptionRanges = AppliedRanges DisruptionFields UIDisruption
type DisruptionPagReqHeaders = PagReqHeaders DisruptionFields UIDisruption
type DisruptionPagRespHeaders = PagRespHeaders DisruptionFields UIDisruption

type DataApi =
       "stats" :> StatsApi
  :<|> "disruptions" :> DisruptionApi
  :<|> "objects" :> ObjectsApi
  :<|> "facilities" :> FacilitiesApi

type StatsApi =
       Get '[JSON] UIOverallStats
  :<|> "averageDisruptionsPerDay" :> Get '[JSON] [UIDisruptionPerDay]

type DisruptionApi =
       DisruptionPagReqHeaders :> GetPartialContent '[JSON] (Headers DisruptionPagRespHeaders [UIDisruption])
  :<|> "markers" :> Get '[JSON] [UIMapMarker]
  :<|> "active" :> DisruptionPagReqHeaders :> GetPartialContent '[JSON] (Headers DisruptionPagRespHeaders [UIDisruption])

type ObjectsApi = QueryParam "search" Text :> Get '[JSON] [UIObjectSearchResult]

type FacilitiesApi =
       Capture "facilityId" FacilityId :> Get '[JSON] UIFacilityDetails
  :<|> DisruptionPagReqHeaders :> Capture "facilityId" FacilityId :> "disruptions" :>  GetPartialContent '[JSON] (Headers DisruptionPagRespHeaders [UIDisruption])

dataServer ::
     IORef UIOverallStats
  -> IORef DisruptionsPerDay
  -> IORef [UIDisruption]
  -> IORef [UIMapMarker]
  -> IORef [UIObjectSearchResult]
  -> IORef (Map FacilityId UIFacilityDetails)
  -> Server DataApi
dataServer stats disruptionsPerDay disruptions marker searchResults facilities =
  statsApi stats disruptionsPerDay :<|> disruptionsApi disruptions marker :<|> objectsApi searchResults :<|> facilitiesApi facilities disruptions

statsApi :: IORef UIOverallStats -> IORef DisruptionsPerDay -> Server StatsApi
statsApi statsRef disPerDayRef =
  readIORef statsRef :<|> disPerDayHandler
  where
    disPerDayHandler = reverse . fmap mkUIDisruptionPerDay . take 30 . drop 1 . reverse . M.toList . fmap average .  snd <$> readIORef disPerDayRef

disruptionsApi :: IORef [UIDisruption] -> IORef [UIMapMarker] -> Server DisruptionApi
disruptionsApi disRef markerRef =
  allDisruptionsHandler :<|> readIORef markerRef :<|> activeDisruptionsHandler
  where
    allDisruptionsHandler ranges = readIORef disRef >>= disruptionsHandler ranges
    activeDisruptionsHandler ranges = readIORef disRef >>= disruptionsHandler ranges . filter (isNothing . uidResolvedOn)

objectsApi :: IORef [UIObjectSearchResult] -> Server ObjectsApi
objectsApi ref maybeSearch =
  if length search < 3
  then return []
  else filter (nameMatches . uioName . uiosrObject) <$> liftIO (readIORef ref)

  where
    search = T.toLower $ fromMaybe "" maybeSearch
    nameMatches = T.isInfixOf search . T.toLower

facilitiesApi :: IORef (Map FacilityId UIFacilityDetails) -> IORef [UIDisruption] -> Server FacilitiesApi
facilitiesApi facilities disruptions =
  facilityDetailsHandler :<|> facilityDisruptionsHandler
  where
    facilityDetailsHandler facilityId =
      maybe (throwError err404) return . lookup facilityId =<< readIORef facilities
    facilityDisruptionsHandler ranges facilityId =
      readIORef disruptions >>= disruptionsHandler ranges . filter ((==) facilityId . uidFacilityId)

disruptionsHandler :: Maybe DisruptionRanges -> [UIDisruption] -> Handler (Headers DisruptionPagRespHeaders [UIDisruption])
disruptionsHandler r ds =
  addHeader (length ds) <$>
    fromMaybe (returnIdRange ds defaultRange)
        (fmap (returnIdRange ds) (r >>= extractRange)
    <|> fmap (returnOccurredOnRange ds) (r >>= extractRange)
    <|> fmap (returnUpdatedOnRange ds) (r >>= extractRange)
    <|> fmap (returnResolvedOnRange ds) (r >>= extractRange)
    <|> fmap (returnDurationRange ds) (r >>= extractRange))

  where
    returnIdRange xs (range :: Range "id" Int) =
      returnRange range (applyRange range xs)

    returnOccurredOnRange xs (range :: Range "occurredOn" DateTime) =
      returnRange range (applyRange range xs)

    returnUpdatedOnRange xs (range :: Range "updatedOn" (Maybe DateTime)) =
      returnRange range (applyRange range xs)

    returnResolvedOnRange xs (range :: Range "resolvedOn" (Maybe DateTime)) =
      returnRange range (applyRange range xs)

    returnDurationRange xs (range :: Range "duration" Integer) =
      returnRange range (applyRange range xs)

defaultRange :: Range "id" Int
defaultRange = getDefaultRange (Proxy @UIDisruption)
