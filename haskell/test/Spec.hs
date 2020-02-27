module Main where

import ScratchPrelude
import Test.Hspec

import qualified ApplicativeParser
import qualified Megaparsec
import qualified MonoidLexer
import qualified ProfunctorParser
import qualified Strong

main :: IO ()
main =
  hspec $ do
    ApplicativeParser.spec
    Megaparsec.spec
    MonoidLexer.spec
    ProfunctorParser.spec
    Strong.spec
