module Main where

import qualified Scratch.ApplicativeParser
import qualified Scratch.Megaparsec
import qualified Scratch.MonoidLexer
import Scratch.Prelude
import qualified Scratch.ProfunctorParser
import qualified Scratch.StaticMonad
import qualified Scratch.StaticMonadProblem
import qualified Scratch.Strong
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Scratch.ApplicativeParser.spec
    Scratch.Megaparsec.spec
    Scratch.MonoidLexer.spec
    Scratch.ProfunctorParser.spec
    Scratch.StaticMonad.spec
    Scratch.StaticMonadProblem.spec
    Scratch.Strong.spec
