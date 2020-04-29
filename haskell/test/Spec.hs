module Main where

import qualified ApplicativeParser
import qualified Megaparsec
import qualified MonoidLexer
import qualified ProfunctorParser
import ScratchPrelude
import qualified Strong
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    ApplicativeParser.spec
    Megaparsec.spec
    MonoidLexer.spec
    ProfunctorParser.spec
    Strong.spec
