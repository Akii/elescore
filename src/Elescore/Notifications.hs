module Elescore.Notifications
  ( sendUserRegistrationMail
  , notifyAboutDisruption
  ) where

import           ClassyPrelude
import qualified Data.Text.Lazy            as LBS
import           Network.Mail.Mime

import           Elescore.Disruptions.Types
import           Elescore.Users.Types

sendUserRegistrationMail :: Text -> Text -> RegistrationToken -> IO ()
sendUserRegistrationMail un em (RegistrationToken t) =
  renderSendMail (simpleMail' to from subject body)

  where
    subject = "Registrierung auf Elescore"
    body = LBS.fromStrict $ "UserName: " <> un <> "\nToken: " <> t <> "\nEmail: " <> em
    from = Address (Just "Elescore") "elescore@akii.de"
    to = Address Nothing "zedd@akii.de"

notifyAboutDisruption :: Disruption -> Maybe Station -> Maybe Facility -> User -> IO ()
notifyAboutDisruption d ms mf u =
  renderSendMail (simpleMail' to from subject body)

  where
    subject = "Neue Störung auf Elescore | " <> disruptionId
    body = LBS.fromStrict $
      "Störung Nr.: " <> disruptionId <>
      "\nStation: " <> stationName <>
      "\nAufzug/Rolltreppe: " <> facilityDescription

    from = Address (Just "Elescore") "elescore@akii.de"
    to = Address (Just . unUserName . uName $ u) (unEmailAddress . uEmail $ u)
    disruptionId = tshow . unDisruptionId . disId $ d
    stationName = fromMaybe ("Unbekannt (" <> (tshow . unStationId . disStationId $ d) <> ")") (join $ fmap sName $ ms)
    facilityDescription = fromMaybe ("Unbekannt (" <> (tshow . unFacilityId . disFacilityId $ d) <> ")") (join $ fmap fDescription $ mf)
