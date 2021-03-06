-- | Demonstration of static analysis with a Monoid-equipped lexer.
module Scratch.MonoidLexer where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Scratch.Prelude
import Test.Hspec

data Lexer a = Lexer
  { keywords :: Set Keyword,
    -- | Parse a single token.
    getToken :: Set Keyword -> Text -> Maybe (Text, a)
  }

newtype Keyword
  = Keyword Text
  deriving (Eq, Ord, Show)

-- | Calls 'getToken' repeatedly to build a list of @a@ tokens.
runLexer ::
  Lexer a ->
  -- | Input to lex
  Text ->
  Maybe [a]
runLexer l@Lexer {keywords, getToken} input
  | Text.null input = Just []
  | otherwise = do
    (remainingInput, tok) <- getToken keywords input
    fmap (tok :) (runLexer l remainingInput)

emptyLexer :: Lexer a
emptyLexer =
  Lexer
    { keywords = mempty,
      getToken = \_ _ -> Nothing
    }

-- | Left-biased.
combineLexers :: forall a. Lexer a -> Lexer a -> Lexer a
combineLexers (Lexer k1 getToken1) (Lexer k2 getToken2) =
  Lexer
    { keywords = k1 <> k2,
      getToken = getTok
    }
  where
    getTok :: Set Keyword -> Text -> Maybe (Text, a)
    getTok finalKeywords txt =
      case getToken1 finalKeywords txt of
        Nothing ->
          getToken2 finalKeywords txt
        res ->
          res

instance Semigroup (Lexer a) where
  (<>) = combineLexers

instance Monoid (Lexer a) where
  mempty = emptyLexer

keywordToken :: Text -> a -> Lexer a
keywordToken tokText tok =
  Lexer
    { keywords = Set.singleton (Keyword tokText),
      getToken = \_ input -> do
        (candidateTok, remaining) <- nextToken input
        if candidateTok == tokText
          then Just (remaining, tok)
          else Nothing
    }

-- | Helper function.
--
-- Separates the input on the first space character.
--
-- Returns a tuple whose first @Text@ is what came before the space,
-- and whose second @Text@ is the remaining input after it
-- with leading spaces stripped.
nextToken :: Text -> Maybe (Text, Text)
nextToken input = do
  let (tok, remaining) = Text.span (/= ' ') input
  if Text.null tok
    then Nothing
    else Just (tok, Text.dropWhile (== ' ') remaining)

-- * Example use

data Token
  = Let
  | Equal
  | Var Text
  deriving (Eq, Ord, Show)

variableToken :: Lexer Token
variableToken =
  Lexer
    { keywords = mempty,
      getToken = \finalKeywords input -> do
        (candidateTok, remaining) <- nextToken input
        if Set.member (Keyword candidateTok) finalKeywords
          then Nothing
          else Just (remaining, Var candidateTok)
    }

exampleLexer :: Lexer Token
exampleLexer =
  variableToken
    <> keywordToken "let" Let
    <> keywordToken "=" Equal

spec :: Spec
spec =
  describe "monoid lexer" $ do
    it "doesn't parse 'let' keyword as a variable" $ do
      runLexer exampleLexer "let a = b"
        `shouldBe` Just [Let, Var "a", Equal, Var "b"]
