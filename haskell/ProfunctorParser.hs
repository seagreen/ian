-- | Demonstration of static analysis with an Profunctor-based parser.
--
-- + This is a toy example showing tracking of keywords.
module ProfunctorParser where

import Control.Category
import Data.Profunctor
import ScratchPrelude hiding ((.))
import Test.Hspec

import qualified Data.Char as Char
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

-- * Simple example

parseNonKeywordToken :: Parser () Text
parseNonKeywordToken =
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

simpleParser :: Parser () Var
simpleParser =
  parseKeyword "start"
    >>> rmap Var parseNonKeywordToken
    >>> passThrough (parseKeyword "end")

-- | Better name?
passThrough :: Strong p => p () x -> p a a
passThrough =
    lmap (\a -> (a, ()))
  . rmap (\(a, _x) -> a)
  . second'

simpleSpec :: Spec
simpleSpec =
  describe "simple profunctor parser" $ do
    it "success" $ do
      runParser simpleParser "start foo end"
        `shouldBe`
          Just ("", Var "foo")

    it "doesn't allow a keyword as a variable" $ do
      runParser simpleParser "start start end"
        `shouldBe`
          Nothing

    it "simple failure" $ do
      runParser simpleParser "start foo asdf"
        `shouldBe`
          Nothing

-- * Full example

-- | Parse @start@,
-- then a text token of letters that must only appear capitalized later
-- then a variable under the previous restriction
-- then @end@
-- then return the variable.
fullParser :: Parser () Var
fullParser = do
  parseKeyword "start"
    >>> parseNonKeywordToken
    >>> capitalizationRestrictedVar
    >>> passThrough (parseKeyword "end")

capitalizationRestrictedVar :: Parser Text Var
capitalizationRestrictedVar =
  lmap (\caps -> (caps, ())) (second' (rmap Var parseNonKeywordToken))
    >>> checkCaps
  where
    checkCaps :: Parser (Text, Var) Var
    checkCaps =
      Parser
        { keywords = mempty -- We're not introducing any new keywords here.
        , runParserWithKeywords = runP
        }
      where
        runP :: Set Keyword -> Text -> (Text, Var) -> Maybe (Text, Var)
        runP _ input (caps, Var varText) =
          if allCapitalized caps varText
            then
              Just (input, Var varText)
            else
              Nothing

        allCapitalized :: Text -> Text -> Bool
        allCapitalized caps =
          Text.all (\c -> Char.isUpper c || not (inCapList c))
          where
            inCapList :: Char -> Bool
            inCapList c =
              Text.singleton (Char.toLower c) `Text.isInfixOf` Text.toLower caps

fullSpec :: Spec
fullSpec =
  describe "full profunctor parser" $ do
    it "success" $ do
      runParser fullParser "start HL HeLLo end"
        `shouldBe`
          Just ("", Var "HeLLo")

    it "failure due to uncapitalized h" $ do
      runParser fullParser "start HL hello end"
        `shouldBe`
          Nothing

    it "failure due to keyword used as variable" $ do
      runParser fullParser "start X end end"
        `shouldBe`
          Nothing

spec :: Spec
spec = do
  simpleSpec
  fullSpec
