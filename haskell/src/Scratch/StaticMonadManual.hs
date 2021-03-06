module Scratch.StaticMonadManual where

-- import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Scratch.Prelude hiding (readFile, writeFile)
import Test.Hspec

-- * API

data Script a = InternalScript
  { internalFilesToBeWrittenTo :: Set FilePath,
    internalAction :: a
  }
  deriving stock (Functor)

instance Applicative Script where
  pure :: a -> Script a
  pure a =
    InternalScript mempty a

  (<*>) :: Script (a -> b) -> Script a -> Script b
  InternalScript xs f <*> InternalScript ys a =
    InternalScript (xs <> ys) (f a)

instance Monad Script where
  (>>=) :: Script a -> (a -> Script b) -> Script b
  InternalScript xs a >>= f =
    let InternalScript ys b = f a
     in InternalScript (xs <> ys) b

-- | Files the script will write to.
analyzeScript :: Script a -> Set FilePath
analyzeScript (InternalScript xs _) =
  xs

runScript :: Script (IO a) -> IO a
runScript (InternalScript _ io) =
  io

writeFile :: FilePath -> IO ByteString -> Script (IO ())
writeFile path ioBts =
  InternalScript
    (Set.singleton path)
    -- (ioBts >>= BS.writeFile path)
    (ioBts *> putStrLn ("Writing: " <> path))

readFile :: FilePath -> Script (IO ByteString)
readFile path =
  InternalScript
    mempty
    -- (BS.readFile path)
    (putStrLn ("Reading: " <> path) *> pure "abc")

-- * Example

-- setupComputer :: Script (IO ())
-- setupComputer = do
--   read1 <- readFile "~/Dropbox/my-config-1.json"
--   read2 <- readFile "~/Dropbox/my-config-2.json"
--   _ <- writeFile "~/config-1.json" read1 -- TROUBLE!
--   writeFile "~/config-2.json" read2

setupComputer :: Script (IO ())
setupComputer = do
  read1 <- readFile "~/Dropbox/my-config-1.json"
  read2 <- readFile "~/Dropbox/my-config-2.json"
  write1 <- writeFile "~/config-1.json" read1
  write2 <- writeFile "~/config-2.json" read2
  pure (write1 *> write2)

runSetupComputerManual :: IO ()
runSetupComputerManual =
  runScript setupComputer

-- * Test

spec :: Spec
spec = do
  describe "static monad" $ do
    it "analyzes correctly" $ do
      analyzeScript setupComputer
        `shouldBe` Set.fromList ["~/config-1.json", "~/config-2.json"]
