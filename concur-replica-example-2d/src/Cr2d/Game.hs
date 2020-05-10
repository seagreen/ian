module Cr2d.Game where

import Cr2d.Point
import Cr2d.Prelude
import Cr2d.Ship

update :: HashMap Text Ship -> HashMap Text Ship
update =
  fmap f
  where
    f :: Ship -> Ship
    f (Ship (Point x y) rotation t) =
      let
        newRot = case t of
                   Port -> (rotation + 0.1) `mod'` (2 * pi)
                   Straight -> rotation
                   Starboard -> (rotation - 0.1) `mod'` (2 * pi)

        newX = x + cos newRot

        newY = y + sin newRot
      in
        Ship (Point newX newY) newRot t
