module Elescore.Integration.Bogestra.Scraper
  ( scrape
  , tableScraper
  ) where

import           ClassyPrelude
import           Data.Text                           (strip)
import           Text.HTML.Scalpel.Core              hiding (scrape)
import           Text.StringLike
import           URI.ByteString

import           Elescore.IdTypes
import           Elescore.Integration.Bogestra.Types

scrape :: Text -> Maybe Table
scrape = flip scrapeStringLike tableScraper

tableScraper :: Scraper Text Table
tableScraper = chroots ("table" // "tr") rowScraper

rowScraper :: Scraper Text TableRow
rowScraper = do
  cols <- fmap strip <$> chroots "td" (innerHTML anySelector)
  case cols of
    [station,facilityId,travelDirection,platform,details,disruption] ->
      TR <$> (fixNewline <$>  embeddedTextScraper station)
         <*> pure (FacilityId ("BOG-" <> facilityId))
         <*> (maybeText . fixNewline <$>  embeddedTextScraper travelDirection)
         <*> (fixNewline <$> embeddedTextScraper platform)
         <*> (details @< relativeURI)
         <*> (disruption @<? relativeURI)
    _ -> mzero

relativeURI :: Scraper Text URI
relativeURI = do
  relUrl <- attr "href" "a"
  case parseURI strictURIParserOptions (encodeUtf8 $ baseUrl <> "/" <> relUrl) of
    Left _  -> mzero
    Right a -> return a

(@<) :: (StringLike a, Ord a) => a -> Scraper a b -> Scraper a b
(@<) a = maybe mzero return . scrapeStringLike a
infixl 5 @<

(@<?) :: (StringLike a, Ord a) => a -> Scraper a b -> Scraper a (Maybe b)
(@<?) a s = return (scrapeStringLike a s)
infixl 5 @<?

embeddedTextScraper :: Text -> Scraper Text Text
embeddedTextScraper t = ("<p>" <> t <> "</p>") @< text anySelector

maybeText :: Text -> Maybe Text
maybeText "" = Nothing
maybeText a  = Just a

fixNewline :: Text -> Text
fixNewline = unwords . filter (not . null) . fmap strip . lines
