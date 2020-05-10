module Cr2d.Ship where

import Cr2d.Point
import Cr2d.Prelude

data Ship = Ship
  { position :: Point
  , rotation :: Double
  , turning :: Turn
  } deriving (Eq, Show)

data Turn
  = Port
  | Straight
  | Starboard
  deriving (Eq, Ord, Show)
