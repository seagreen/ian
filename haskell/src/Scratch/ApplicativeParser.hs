-- | Demonstration of static analysis with an Applicative parser.
--
-- + This is a toy example showing tracking of keywords.
module Scratch.ApplicativeParser where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Scratch.Prelude
import Test.Hspec

data Parser a = Parser
  { keywords :: Set Keyword,
    runParserWithKeywords :: Set Keyword -> Text -> Maybe (Text, a)
  }

newtype Keyword
  = Keyword Text
  deriving (Eq, Ord, Show)

runParser ::
  Parser a ->
  -- | Input
  Text ->
  -- | Unconsumed input and result
  Maybe (Text, a)
runParser Parser {keywords, runParserWithKeywords} =
  runParserWithKeywords keywords

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p@(Parser _ runP) =
    p {runParserWithKeywords = (fmap . fmap . fmap) f . runP}

lift2Parser :: forall a b c. (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2Parser f (Parser k1 p1) (Parser k2 p2) =
  Parser
    { keywords = k1 <> k2,
      runParserWithKeywords = runP
    }
  where
    runP :: Set Keyword -> Text -> Maybe (Text, c)
    runP finalKeywords input = do
      (remaining, a) <- p1 finalKeywords input
      (remaining2, b) <- p2 finalKeywords remaining
      Just (remaining2, f a b)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a =
    Parser
      { keywords = mempty,
        runParserWithKeywords = \_ input -> Just (input, a)
      }

  liftA2 = lift2Parser

-- If we wanted to make this module really useful
-- we'd also write an Alternative instance,
-- which we could do and still keep static analysis (unlike Monad).

parseKeyword :: Text -> Parser ()
parseKeyword keyword =
  Parser
    { keywords = Set.singleton (Keyword keyword),
      runParserWithKeywords = \_ input -> do
        remaining <- Text.stripPrefix keyword input
        Just (Text.dropWhile (== ' ') remaining, ())
    }

-- * Example use

data EqualityEquation
  = EqualityEquation Text Text
  deriving (Eq, Ord, Show)

parseVariable :: Parser Text
parseVariable =
  Parser
    { keywords = mempty,
      runParserWithKeywords = runP
    }
  where
    -- If we want to forbid the keywords of our language
    -- from being used as variables, normally we'd have
    -- to maintain a list of them, and keep it in sync
    -- with the parser code.
    --
    -- But here we pull it out of THIN. AIR.
    runP :: Set Keyword -> Text -> Maybe (Text, Text)
    runP finalKeywords input = do
      let (candidateVar, remaining) = Text.span (/= ' ') input
      guard (not (Text.null candidateVar))
      if Set.member (Keyword candidateVar) finalKeywords
        then Nothing
        else Just (Text.dropWhile (== ' ') remaining, candidateVar)

exampleParser :: Parser EqualityEquation
exampleParser =
  (\() v1 () v2 -> EqualityEquation v1 v2)
    <$> parseKeyword "assert"
    <*> parseVariable
    <*> parseKeyword "=="
    <*> parseVariable

spec :: Spec
spec =
  describe "applicative parser" $ do
    it "fails if a keyword is used as a variable" $ do
      runParser exampleParser "assert assert == b"
        `shouldBe` Nothing
    it "succeeds" $ do
      runParser exampleParser "assert a == b"
        `shouldBe` Just ("", EqualityEquation "a" "b")
