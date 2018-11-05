module Elescore.Integration.Bogestra
  ( Bogestra
  , mkBogSource
  ) where

import           ClassyPrelude
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude                          as P

import           Database.SimpleEventStore
import           Elescore.Integration.Bogestra.Client
import           Elescore.Integration.Bogestra.Monitor
import           Elescore.Integration.Bogestra.Types
import           Elescore.Integration.Common.Monitoring
import           Elescore.Integration.Common.Types
import           Elescore.Integration.Common.Utils

mkBogSource :: MonadIO m => Store -> m (Source m Bogestra)
mkBogSource store = do
  (out1, in1) <- liftIO (spawn unbounded)
  (out2, in2) <- liftIO (spawn unbounded)

  void . liftIO . forkIO $ runEffect $ elevatorP 900 >-> toOutput (out1 <> out2)

  dP <- disruptions store
  fP <- facilities store
  oEvs <- liftIO (readStream store)

  return Source
     { disruptionEvents = fromInput in1 >-> dP
     , facilityEvents = fromInput in2 >-> fP
     , objectEvents = each oEvs
     }

   where
     elevatorP :: MonadIO m => Int -> Producer [Elevator] m ()
     elevatorP delay = forever $ do
       res <- liftIO (try scrapeAllPages)
       case res of
         Left err         -> print (err :: SomeException)
         Right Nothing    -> print ("Scraping Bogestra failed" :: Text)
         Right (Just evs) -> yield evs
       waitSeconds delay

disruptions :: MonadIO m => Store -> m (Pipe [Elevator] (PersistedEvent (DisruptionEvent Bogestra)) m ())
disruptions store = do
  disEvs <- liftIO (readStream store)

  return $ P.map (fmap $ liftA2 (,) eId eIsDisrupted)
    >-> monitorP bogDisruptionMonitor (replayMkSeed bogDisruptionMonitor . fmap evPayload $ disEvs)
    >-> eventStoreP store
    >-> P.concat
    >-> eachBefore disEvs

facilities :: MonadIO m => Store -> m (Pipe [Elevator] (PersistedEvent (FacilityEvent Bogestra)) m ())
facilities store = do
  fEvs <- liftIO (readStream store)

  return $ monitorP bogFacilityMonitor (replayMkSeed bogFacilityMonitor . fmap evPayload $ fEvs)
    >-> eventStoreP store
    >-> P.concat
    >-> eachBefore fEvs
