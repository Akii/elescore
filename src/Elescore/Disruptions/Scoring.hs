module Elescore.Disruptions.Scoring where

import ClassyPrelude

import Elescore.Disruptions.History -- ugly

{-

KPIs:

for Station or Facility
- # disruptions
- min/max/avg/median down time

-}

data Score
data KPI

calculateKPI :: History -> KPI
calculateKPI = undefined
