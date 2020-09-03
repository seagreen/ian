module Scratch.StaticMonad where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Scratch.Prelude hiding (readFile, writeFile)
import Test.Hspec

-- * API

data Script a = InternalScript (IO (Set FilePath, IO (), a))
  deriving stock (Functor)

instance Applicative Script where
  pure :: a -> Script a
  pure a =
    InternalScript (pure (mempty, pure (), a))

  (<*>) :: Script (a -> b) -> Script a -> Script b
  InternalScript f <*> InternalScript a =
    InternalScript $ do
      (files1, action1, f1) <- f
      (files2, action2, a2) <- a
      pure (files1 <> files2, action1 *> action2, f1 a2)

instance Monad Script where
  (>>=) :: Script a -> (a -> Script b) -> Script b
  InternalScript a >>= f =
    InternalScript $ do
      (files1, action1, a1) <- a
      let InternalScript io = f a1
      (files2, action2, b) <- io
      pure (files1 <> files2, action1 *> action2, b)

-- | Files the script will write to.
analyzeScript :: Script a -> IO (Set FilePath)
analyzeScript (InternalScript go) = do
  (xs, _, _) <- go
  pure xs

runScript :: Script () -> IO ()
runScript (InternalScript go) = do
  (_, action, ()) <- go
  action

writeFile :: FilePath -> IORef ByteString -> Script ()
writeFile path btsVar =
  InternalScript $ do
    pure
      ( Set.singleton path,
        do
          bts <- readIORef btsVar
          putStrLn ("Writing " <> decodeUtf8 bts <> " to " <> path),
        ()
      )

readFile :: FilePath -> Script (IORef ByteString)
readFile path =
  InternalScript $ do
    btsVar <- newIORef undefined -- Hack
    pure
      ( mempty,
        putStrLn ("Reading: " <> path) *> writeIORef btsVar "abc",
        btsVar
      )

-- * Example

setupComputer :: Script ()
setupComputer = do
  read1 <- readFile "~/Dropbox/my-config-1.json"
  read2 <- readFile "~/Dropbox/my-config-2.json"
  writeFile "~/config-1.json" read1
  writeFile "~/config-2.json" read2

runSetupComputer :: IO ()
runSetupComputer =
  runScript setupComputer

-- * Expanded example

ifEmpty :: IORef ByteString -> Script () -> Script () -> Script ()
ifEmpty predicate (InternalScript go1) (InternalScript go2) =
  InternalScript $ do
    (files1, action1, ()) <- go1
    (files2, action2, ()) <- go2
    pure
      ( files1 <> files2,
        do
          res <- readIORef predicate
          if BS.null res
            then action1
            else action2,
        ()
      )

setupComputerBranching :: Script ()
setupComputerBranching = do
  read1 <- readFile "~/Dropbox/my-config-1.json"
  ifEmpty
    read1
    (writeFile "~/was-empty.json" read1)
    (writeFile "~/not-empty.json" read1)

runSetupComputerBranching :: IO ()
runSetupComputerBranching =
  runScript setupComputerBranching

-- * Test

spec :: Spec
spec = do
  describe "static monad 3" $ do
    it "analyzes correctly" $ do
      analyzeScript setupComputer
        `shouldReturn` Set.fromList ["~/config-1.json", "~/config-2.json"]
    it "branching example" $ do
      analyzeScript setupComputerBranching
        `shouldReturn` Set.fromList ["~/not-empty.json", "~/was-empty.json"]
