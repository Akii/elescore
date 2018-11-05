{-# LANGUAGE RecordWildCards #-}

module Elescore.Integration.Bogestra.Client where

import ClassyPrelude
import Network.Wreq
import Control.Lens

import Elescore.Integration.Bogestra.Types
import Elescore.Integration.Bogestra.Scraper

urlBochum, urlGelsenkirchen, urlHerne :: String
urlBochum = baseUrl <> "/aufzuginfos/aufzuege-in-bochum.html"
urlGelsenkirchen = baseUrl <> "/aufzuginfos/aufzuege-in-gelsenkirchen.html"
urlHerne = baseUrl <> "/aufzuginfos/aufzuege-in-herne.html"

scrapeAllPages :: IO (Maybe [Elevator])
scrapeAllPages = do
  [b, g, h] <- mapConcurrently scrapePage [urlBochum, urlGelsenkirchen, urlHerne]
  return $
    concat <$>
    sequence
      [ fmap (fmap $ toElevator Bochum) b
      , fmap (fmap $ toElevator Gelsenkirchen) g
      , fmap (fmap $ toElevator Herne) h
      ]
  where
    toElevator :: Object -> TableRow -> Elevator
    toElevator o TR {..} =
      Elevator trFacilityId (Just $ getObjectId o) trStation (isJust trDisruptionLink)

scrapePage :: String -> IO (Maybe Table)
scrapePage url = do
  res <- get url
  let body = res ^. responseBody
      text = toStrict (decodeUtf8 body)
  return (scrape text)
