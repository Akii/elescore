{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Database.SimpleEventStore
  ( Connection
  , PersistedEvent(..)
  , EventId(..)
  , EventType(..)
  , StreamName(..)
  , HasStream(..)
  , HasNamespace(..)
  , PersistableEvent(..)
  , mkConnection
  , append
  , appendMany
  , readStream
  , module Data.Aeson
  ) where

import           ClassyPrelude                    hiding (getCurrentTime)
import           Data.Aeson
import           Data.Data
import           Data.DateTime
import qualified Data.HashMap.Strict              as HM
import           Data.UUID                        hiding (fromString)
import qualified Data.UUID                        as UUID
import           Data.UUID.V4                     (nextRandom)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Text.RawString.QQ

data PersistedEvent a = PersistedEvent
  { evId         :: EventId
  , evType       :: EventType
  , evStream     :: StreamName
  , evOccurredOn :: DateTime
  , evPayload    :: a
  } deriving (Functor, Show)

newtype EventId = EventId
  { getEventId :: UUID
  } deriving (Show)

newtype EventType = EventType
  { getEventType :: Text
  } deriving (Show)

instance IsString EventType where
  fromString = EventType . pack

newtype StreamName = StreamName
  { getStreamName :: Text
  } deriving (Show)

instance Semigroup StreamName where
  s1 <> s2 = StreamName (getStreamName s1 <> getStreamName s2)

instance IsString StreamName where
  fromString = StreamName . pack

class HasStream a where
  getStream :: StreamName

class HasNamespace a where
  getNamespace :: Text

class PersistableEvent a where
  encodeEvent :: a -> Value
  decodeEvent :: EventType -> Value -> Either Text a
  eventType   :: a -> EventType
  eventTypes  :: [EventType]

  default encodeEvent :: ToJSON a => a -> Value
  encodeEvent = deleteEventTypeTag . toJSON

  default decodeEvent :: FromJSON a => EventType -> Value -> Either Text a
  decodeEvent et v =
    let x = fromJSON (injectEventTypeTag et v)
    in case x of
      Success a -> Right a
      Error err -> Left (pack err)

  default eventType :: (HasNamespace a, Data a) => a -> EventType
  eventType = prependNamespace (getNamespace @a) . toConstr

  default eventTypes :: (HasNamespace a, Data a) => [EventType]
  eventTypes = fmap (prependNamespace (getNamespace @a)) (dataTypeConstrs $ dataTypeOf (error "just need a value here" :: a))

prependNamespace :: Text -> Constr -> EventType
prependNamespace ns ctor = EventType $ ns <> "." <> fromString (showConstr ctor)

injectEventTypeTag :: EventType -> Value -> Value
injectEventTypeTag (EventType et) (Object o) = Object (HM.insert "tag" (String et) o)
injectEventTypeTag _ a = a

deleteEventTypeTag :: Value -> Value
deleteEventTypeTag (Object o) = Object (HM.delete "tag" o)
deleteEventTypeTag a          = a

mkConnection :: FilePath -> IO Connection
mkConnection dbFile = do
  conn <- open dbFile
  createEventStore conn
  return conn

createEventStore :: MonadIO m => Connection -> m ()
createEventStore conn = liftIO $ execute_ conn [r|
    CREATE TABLE IF NOT EXISTS event_store (
      id VARCHAR NOT NULL,
      sequence INTEGER PRIMARY KEY,
      type VARCHAR NOT NULL,
      stream VARCHAR NOT NULL,
      occurred_on VARCHAR NOT NULL,
      payload VARCHAR NOT NULL
    );
  |]

append :: (HasStream a, PersistableEvent a) => Connection -> a -> IO (PersistedEvent a)
append conn a = do
  persistedEvent <- mkPersistedEvent a
  writeStream conn persistedEvent
  return persistedEvent

appendMany :: (HasStream a, PersistableEvent a) => Connection -> [a] -> IO [PersistedEvent a]
appendMany conn as = do
  evs <- mapM mkPersistedEvent as
  writeStreamM conn evs
  return evs

-- todo: use fold for streaming results
-- todo: if sequence needed use (.:)
readStream :: forall a. (HasStream a, PersistableEvent a) => Connection -> IO [PersistedEvent a]
readStream conn =
  let types = eventTypes @a
      stream = getStream @a
      params = (":stream" := (stream <> "%")) : zipWith (\i t -> (":" <> tshow i) := t) [0 .. length types] types
      queryBinds = intercalate "," ((":" <>) . show <$> [0 .. length types - 1])
      q = fromString ("SELECT id, type, stream, occurred_on, payload FROM event_store WHERE stream like :stream AND type IN (" <> queryBinds <> ") ORDER BY sequence ASC")

  in queryNamed conn q params

writeStream :: PersistableEvent a => Connection -> PersistedEvent a -> IO ()
writeStream conn = execute conn "INSERT INTO event_store (id, type, stream, occurred_on, payload) VALUES (?,?,?,?,?)"

writeStreamM :: PersistableEvent a => Connection -> [PersistedEvent a] -> IO ()
writeStreamM conn = executeMany conn "INSERT INTO event_store (id, type, stream, occurred_on, payload) VALUES (?,?,?,?,?)"

mkPersistedEvent :: forall a. (HasStream a, PersistableEvent a) => a -> IO (PersistedEvent a)
mkPersistedEvent a = do
  let evStream = getStream @a
      evType = eventType a
      evPayload = a
  evId <- EventId <$> nextRandom
  evOccurredOn <- getCurrentTime

  return PersistedEvent {..}

---------------------------
-- DB Instances
---------------------------

instance FromField EventId where
  fromField a = do
    muuid <- UUID.fromString <$> fromField a
    maybe (fail "Unable to map UUID from row") (return . EventId) muuid

instance ToField EventId where
  toField = toField . toString . getEventId

instance FromField EventType where
  fromField = fmap EventType . fromField

instance ToField EventType where
  toField = toField . getEventType

instance FromField StreamName where
  fromField = fmap StreamName . fromField

instance ToField StreamName where
  toField = toField . getStreamName

instance PersistableEvent a => ToRow (PersistedEvent a) where
  toRow PersistedEvent {..} =
    toRow (evId, evType, evStream, evOccurredOn, encode (encodeEvent evPayload))

instance PersistableEvent a => FromRow (PersistedEvent a) where
  fromRow = do
    evId <- field
    evType <- field
    evStream <- field
    evOccurredOn <- field
    evValue <- either fail return . eitherDecode =<< field
    evPayload <- either (fail . show) return (decodeEvent @a evType evValue)

    return PersistedEvent {..}
