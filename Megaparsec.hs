-- | Notes on megaparsec.
module Megaparsec where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Text (Text)
import Data.Void
import Prelude
import Test.Hspec
import Test.Main (ProcessResult(prStdout), captureProcessResult)

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text

-- parser-combinators (http://hackage.haskell.org/package/parser-combinators):
--
--     "Lightweight package providing commonly useful parser combinators."
--
-- Control.Monad.Combinators.NonEmpty:
--
--     "The module provides more efficient versions of the combinators from Control.Applicative.Combinators defined in terms of Monad and MonadPlus instead of Applicative and Alternative. When there is no difference in performance we just re-export the combinators from Control.Applicative.Combinators."
import Control.Monad.Combinators.NonEmpty

-- Note that there are four(!) modules that provide different versions
-- of @some@ (six if we count re-exports).
--
-- The Applicative @some@, exported by Control.Applicative and Control.Applicative.Combinators.
--
-- The MonadPlus @some@, exported by Control.Monad.Combinators and Text.Megaparsec.
--
-- The NonEmpty Applicative @some@, exported by Control.Applicative.Combinators.NonEmpty.
--
-- The NonEmpty MonadPlus @some@, exported by Control.Monad.Combinators.NonEmpty.
--
--
-- My strategy is to default to Control.Monad.Combinators.NonEmpty.
-- It's the most efficient and has the most descriptive type signatures.
--
-- (This goes for everyting it exports including @sepBy1@ etc, not just @some@).
--
-- So we import all of Control.Monad.Combinators.NonEmpty
-- and then hide the clashing names from Text.Megaparsec.
import Text.Megaparsec hiding (endBy1, parse, sepBy1, sepEndBy1, some, someTill)

type Parser = Parsec Void Text

-- | megaparsec exports both @parse@ and @runParser@ (which mean the same thing).
-- We hide @parse@ to get the nice name back and then define a new one here
-- for the behavior we want in this file.
parse :: Parser a -> Text -> Either Text a
parse p =
    Bifunctor.first (Text.pack . errorBundlePretty)
  . runParser (p <* eof) "<input>"

-- Notes on redundant definitions
--
-- 'Text.Megaparsec.Char.char' is just a type-constrained version of 'single',
-- so just use that instead.
--
-- 'Text.Megaparsec.Char.string' is just a type-constrained version of 'chunk'.
--
-- 'Control.Monad.Combinators.choice' is just 'asum'.

spec :: Spec
spec =
  describe "megaparsec" $ do
    it "running parsers" $ do
      let
        p :: Parser Char
        p = single 'a'

        shouldReturnStdout :: IO () -> ByteString -> IO ()
        shouldReturnStdout run expected = do
          res <- captureProcessResult run
          prStdout res `shouldBe` expected

      -- When using runParser, parseTest, and parseMaybe
      -- be aware that the last parses an @eof@ after its argument,
      -- but the others don't. So you can't switch freely between them.
      runParser  p "<input>" "ab" `shouldBe` Right 'a'
      parseTest  p           "ab" `shouldReturnStdout` "'a'\n"
      parseMaybe p           "ab" `shouldBe` Nothing

    it "tokens backtracks" $ do
      let
        p :: Parser Text
        p = tokens (==) "aaa" <|> tokens (==) "aab"

      parse p "aab" `shouldBe` Right "aab"

    it "chunk backtracks" $ do
      -- (It's just tokens under the hood).
      let
        p :: Parser Text
        p = chunk "aaa" <|> chunk "aab"

      parse p "aab" `shouldBe` Right "aab"

    it "some satisfy doesn't backtrack" $ do
      let
        takeAs :: Parser Text
        takeAs =
          fmap (Text.pack . toList) (some (satisfy (\c -> c == 'a')))

        takeAsOrBs :: Parser Text
        takeAsOrBs =
          fmap (Text.pack . toList) (some (satisfy (\c -> c == 'a' || c == 'b')))

      parse (takeAs <|> takeAsOrBs) "aab" `shouldBe`
        Left "<input>:1:3:\n  |\n1 | aab\n  |   ^\nunexpected 'b'\nexpecting end of input\n"
