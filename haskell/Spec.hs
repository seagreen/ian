module Main where

import Prelude
import Test.Hspec

import qualified Megaparsec
import qualified Strong

main :: IO ()
main =
  hspec $ do
    Megaparsec.spec
    Strong.spec
