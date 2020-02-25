module MonoidalLexer where

import Control.Monad
import Data.Set (Set)
import Data.Text (Text)
import Prelude
import Test.Hspec

import qualified Data.Set as Set
import qualified Data.Text as Text

data Lexer a = Lexer
  { keywords :: Set Keyword
  , getToken :: Set Keyword -> Text -> Maybe (Text, a)
  }

newtype Keyword
  = Keyword Text
  deriving (Eq, Ord, Show)

runLexer
  :: Lexer a
  -> Text -- ^ Input to lex
  -> Maybe [a]
runLexer l@Lexer{keywords, getToken} input
  | Text.null input = Just []
  | otherwise       = do
      (remainingInput, tok) <- getToken keywords input
      fmap (tok : ) (runLexer l remainingInput)

emptyLexer :: Lexer a
emptyLexer =
  Lexer
    { keywords = mempty
    , getToken = \_ _ -> Nothing
    }

combineLexers :: forall a. Lexer a -> Lexer a -> Lexer a
combineLexers (Lexer k1 l1) (Lexer k2 l2) =
  Lexer
    { keywords = k1 <> k2
    , getToken = getTok
    }
  where
    getTok :: Set Keyword -> Text -> Maybe (Text, a)
    getTok finalKeywords txt =
      case l1 finalKeywords txt of
        Nothing ->
          l2 finalKeywords txt

        res ->
          res

instance Semigroup (Lexer a) where
  (<>) = combineLexers

instance Monoid (Lexer a) where
  mempty = emptyLexer

keywordToken :: Text -> a -> Lexer a
keywordToken tokText tok =
  Lexer
    { keywords = Set.singleton (Keyword tokText)
    , getToken =
        \_keywords input -> do
          (candidateTok, remaining) <- getNonWhitespace input
          if candidateTok == tokText
            then
              Just (remaining, tok)
            else
              Nothing
    }

-- | Returns a tuple whose first @Text@ is the remaining input,
-- and whose second @Text@ is a potential token.
getNonWhitespace :: Text -> Maybe (Text, Text)
getNonWhitespace input = do
  let
    (txt, remaining) = Text.span (/= ' ') input
  guard (not (Text.null txt))
  Just (txt, Text.dropWhile (== ' ') remaining)

dropWhitespaceOrEnd :: Text -> Maybe Text
dropWhitespaceOrEnd t
  | Text.null t        = Just t
  | Text.head t == ' ' = Just (Text.dropWhile (== ' ') t)
  | otherwise          = Nothing

-- * Example use

data Token
  = If
  | Equals
  | Var Text
  deriving (Eq, Ord, Show)

exampleVar :: Lexer Token
exampleVar =
  Lexer
    { keywords = mempty
    , getToken =
        \finalKeywords input -> do
          (candidateTok, remaining) <- getNonWhitespace input
          if Set.member (Keyword candidateTok) finalKeywords
            then
              Nothing
            else
              Just (remaining, Var candidateTok)
    }

exampleLexer :: Lexer Token
exampleLexer =
  exampleVar <> keywordToken "if" If <> keywordToken "==" Equals

spec :: Spec
spec =
  describe "monoidal lexer" $ do
    it "1" $ do
      runLexer exampleLexer "if a == b"
        `shouldBe`
          Just [If, Var "a", Equals, Var "b"]
