module Main where

import Prelude
import Test.Hspec

import qualified Megaparsec
import qualified MonoidalLexer
import qualified Strong

main :: IO ()
main =
  hspec $ do
    Megaparsec.spec
    MonoidalLexer.spec
    Strong.spec
