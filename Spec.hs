module Main where

import Prelude
import Test.Hspec

import qualified Strong

main :: IO ()
main =
  hspec $
    Strong.spec
