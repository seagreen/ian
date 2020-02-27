-- | Demonstration of static analysis with an Profunctor-based parser.
--
-- + This is a toy example showing tracking of keywords.
module ProfunctorParser where

import Control.Category
import Data.Profunctor
import ScratchPrelude hiding ((.))
import Test.Hspec

import qualified Data.Set as Set
import qualified Data.Text as Text

data Parser a b = Parser
  { keywords :: Set Keyword
  , runParserWithKeywords :: Set Keyword -> Text -> a -> Maybe (Text, b)
  }

newtype Keyword
  = Keyword Text
  deriving (Eq, Ord, Show)

runParser
  :: Parser () b
  -> Text -- ^ Input
  -> Maybe (Text, b) -- ^ Unconsumed input and result
runParser Parser{keywords, runParserWithKeywords} input =
  runParserWithKeywords keywords input ()

instance Functor (Parser a) where
  fmap :: forall b c. (b -> c) -> Parser a b -> Parser a c
  fmap f p@(Parser _ oldRunP) =
    p { runParserWithKeywords = runP }
    where
      runP :: Set Keyword -> Text -> a -> Maybe (Text, c)
      runP finalKeywords input a =
        (fmap.fmap) f (oldRunP finalKeywords input a)

instance Profunctor Parser where
  lmap :: forall x a b. (x -> a) -> Parser a b -> Parser x b
  lmap f p@(Parser _ oldRunP) =
    p { runParserWithKeywords = runP }
    where
      runP :: Set Keyword -> Text -> x -> Maybe (Text, b)
      runP finalKeywords input a =
        oldRunP finalKeywords input (f a)

  rmap :: (b -> c) -> Parser a b -> Parser a c
  rmap = fmap

instance Category Parser where
  id :: Parser a a
  id =
    Parser
      { keywords = mempty
      , runParserWithKeywords = \_ _ _ -> Nothing
      }

  (.) :: forall a b c. Parser b c -> Parser a b -> Parser a c
  Parser k1 p1 . Parser k2 p2 =
    Parser
      { keywords = k1 <> k2
      , runParserWithKeywords = runP
      }
    where
      runP :: Set Keyword -> Text -> a -> Maybe (Text, c)
      runP finalKeywords input a = do
        (remaining, b) <- p2 finalKeywords input a
        p1 finalKeywords remaining b

instance Strong Parser where
  -- | "A profunctor is strong if it can freely pass unknown values through it without modification."
  --
  --     - https://elvishjerricco.github.io/2017/03/10/profunctors-arrows-and-static-analysis.html
  --
  -- Note that not all profunctors are strong. For example:
  --
  --     newtype Ignore a b = Ignore b
  --
  -- (from here: https://blog.functorial.com/posts/2015-12-06-Counterexamples.html).
  first' :: Parser a b -> Parser (a, c) (b, c)
  first' p@(Parser _ runP) =
    p { runParserWithKeywords =
          \finalKeywords input (a, c) ->
            (fmap.fmap) (, c) (runP finalKeywords input a)
      }

parseKeyword :: Text -> Parser () ()
parseKeyword keyword =
  Parser
    { keywords = Set.singleton (Keyword keyword)
    , runParserWithKeywords =
        \_ input () -> do
          remaining <- Text.stripPrefix keyword input
          Just (Text.dropWhile (== ' ') remaining, ())
    }

-- * Example use

parseVariable :: Parser () Text
parseVariable =
  Parser
    { keywords = mempty
    , runParserWithKeywords = runP
    }
  where
    -- If we want to forbid the keywords of our language
    -- from being used as variables, normally we'd have
    -- to maintain a list of them, and keep it in sync
    -- with the parser code.
    --
    -- But here we pull it out of THIN. AIR.
    runP :: Set Keyword -> Text -> () -> Maybe (Text, Text)
    runP finalKeywords input () = do
      let
        (candidateVar, remaining) = Text.span (/= ' ') input
      guard (not (Text.null candidateVar))
      if Set.member (Keyword candidateVar) finalKeywords
        then
          Nothing
        else
          Just (Text.dropWhile (== ' ') remaining, candidateVar)

data Var
  = Var Text
  deriving (Eq, Show)

exampleParser :: Parser () Var
exampleParser =
  parseKeyword "start"
    >>> fmap Var parseVariable
    >>> rmap (\(var, ()) -> var)
          (lmap (\var -> (var, ()))
            (second'
              (parseKeyword "end")))

spec :: Spec
spec =
  describe "profunctor parser" $ do
    it "success" $ do
      runParser exampleParser "start a end"
        `shouldBe`
          Just ("", Var "a")

    it "doesn't allow a keyword as a variable" $ do
      runParser exampleParser "start start end"
        `shouldBe`
          Nothing

    it "simple failure" $ do
      runParser exampleParser "start a asdf"
        `shouldBe`
          Nothing
