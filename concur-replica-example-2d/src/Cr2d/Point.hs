module Cr2d.Point where

import Cr2d.Prelude

data Point = Point
  { x :: Double
  , y :: Double
  } deriving (Eq, Ord, Show)
