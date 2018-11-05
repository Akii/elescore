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

mkBogSource :: MonadIO m => Connection -> IO (Source m Bogestra)
mkBogSource conn = do
  dState <- replayMkSeed bogDisruptionMonitor . fmap evPayload <$> readStream conn
  fState <- replayMkSeed bogFacilityMonitor . fmap evPayload <$> readStream conn

  (out1, in1) <- spawn unbounded
  (out2, in2) <- spawn unbounded

  void . forkIO $ runEffect $ elevatorP 900 >-> toOutput (out1 <> out2)

  return Source
     { disruptionEvents = fromInput in1 >-> P.map (fmap $ liftA2 (,) eId eIsDisrupted) >-> monitorP bogDisruptionMonitor dState >-> P.concat
     , facilityEvents = fromInput in2 >-> monitorP bogFacilityMonitor fState >-> P.concat
     , objectEvents = mempty
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
