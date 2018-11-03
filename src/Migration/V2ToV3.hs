{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Migration.V2ToV3 where

import ClassyPrelude
import Database.SQLite.Simple
import Data.DateTime

import Database.SimpleEventStore
import Elescore.Integration
import Elescore.IdTypes

{-

    CREATE TABLE IF NOT EXISTS disruption_log (
      disruption_id VARCHAR PRIMARY KEY,
      station_id INTEGER NOT NULL,
      facility_id INTEGER NOT NULL,
      facility_state VARCHAR NOT NULL,
      reason VARCHAR,
      occurred_on VARCHAR NOT NULL,
      change_type VARCHAR NOT NULL New | Updated | Resolved
    );

    CREATE TABLE IF NOT EXISTS event_store (
      id VARCHAR NOT NULL,
      sequence INTEGER PRIMARY KEY,
      type VARCHAR NOT NULL,
      stream VARCHAR NOT NULL,
      occurred_on VARCHAR NOT NULL,
      payload VARCHAR NOT NULL
    );

data DisruptionEvent a
  = FacilityDisrupted { deFacilityId :: FacilityId a, deReason :: Reason }
  | DisruptionReasonUpdated { deFacilityId :: FacilityId a, deReason :: Reason }
  | FacilityRestored { deFacilityId :: FacilityId a }
  deriving (Show, Generic, Data)

-}

type SId = Integer
type FId = Integer
type ReasonOld = Maybe Text
type Change = Text

type DisruptionLog = (EventId, FId, ReasonOld, DateTime, Change)

migrate :: FilePath -> IO ()
migrate db = do
  conn <- mkConnection db

  log <- query_ conn "SELECT disruption_id, facility_id, reason, occurred_on, change_type FROM disruption_log" :: IO [DisruptionLog]
  mapM_ (writeStream conn) (fmap mapDisruptionToEvent log)

mapDisruptionToEvent :: DisruptionLog -> PersistedEvent (DisruptionEvent DB)
mapDisruptionToEvent (evId, fId, rOld, evOccurredOn, c) =
  let reason = maybe (error "no reason") translateReason rOld
      evPayload = case c of
        "New" -> FacilityDisrupted (mkFacilityId fId) reason
        "Updated" -> DisruptionReasonUpdated (mkFacilityId fId) reason
        "Resolved" -> FacilityRestored (mkFacilityId fId)
        _ -> error "whoops"
      evType = eventType evPayload
      evStream = getStream @(DisruptionEvent DB)
  in PersistedEvent {..}

mkFacilityId :: Integer -> FacilityId DB
mkFacilityId i = FacilityId ("DB-" <> tshow i)

translateReason :: Text -> Reason
translateReason t =
  case t of
    "under construction"       -> UnderConstruction
    "monitoring disrupted"     -> MonitoringDisrupted
    "not available"            -> NotAvailable
    "monitoring not available" -> MonitoringNotAvailable
    "under maintenance"        -> UnderMaintenance
    a                          -> Unknown a

writeStream :: PersistableEvent a => Connection -> PersistedEvent a -> IO ()
writeStream conn = execute conn "INSERT INTO event_store (id, type, stream, occurred_on, payload) VALUES (?,?,?,?,?)"
