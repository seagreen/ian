module StaticMonadProblem where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import ScratchPrelude hiding (readFile, writeFile)
import Test.Hspec

-- * API

data Script a = InternalScript
  { internalFilesToBeWrittenTo :: Set FilePath,
    internalAction :: IO a
  }
  deriving stock (Functor)

instance Applicative Script where
  pure :: a -> Script a
  pure a =
    InternalScript mempty (pure a)

  (<*>) :: Script (a -> b) -> Script a -> Script b
  InternalScript xs f <*> InternalScript ys a =
    InternalScript (xs <> ys) (f <*> a)

instance Monad Script where
  (>>=) :: Script a -> (a -> Script b) -> Script b
  InternalScript xs ioA >>= f =
    InternalScript
      xs
      ( ioA >>= \a -> case f a of
          InternalScript _ b ->
            b
      )

-- | Files the script will write to.
analyzeScript :: Script a -> Set FilePath
analyzeScript (InternalScript xs _) =
  xs

runScript :: Script a -> IO a
runScript (InternalScript _ io) =
  io

data Result a = InternalResult a

writeFile :: FilePath -> Result ByteString -> Script ()
writeFile path (InternalResult bts) =
  InternalScript (Set.singleton path) (BS.writeFile path bts)

readFile :: FilePath -> Script (Result ByteString)
readFile path =
  InternalScript mempty (InternalResult <$> BS.readFile path)

-- * Example

setupComputer :: Script ()
setupComputer = do
  c1 <- readFile "~/Dropbox/my-config-1.json"
  c2 <- readFile "~/Dropbox/my-config-2.json"
  writeFile "~/config-1.json" c1
  writeFile "~/config-2.json" c2

-- * Test

spec :: Spec
spec = do
  describe "static monad" $ do
    -- it "analyzes correctly" $ do
    --   analyzeScript setupComputer
    --     `shouldBe` Set.fromList ["~/config-1.json", "~/config-2.json"]
    it "misses writes!!" $ do
      analyzeScript setupComputer
        `shouldBe` Set.fromList mempty
