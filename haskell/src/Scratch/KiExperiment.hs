-- | Use:
--
-- > stack eval KiExperiment.test
module Scratch.KiExperiment where

import Control.Concurrent (forkIO, threadDelay)
import qualified Ki.Lite as Ki
import Relude

test :: IO ()
test = do
  forkKiPrint
  forkForkIOPrint
  forever (threadDelay maxBound)

forkKiPrint :: IO ()
forkKiPrint = do
  Ki.scoped $ \scope -> do
    _ <- Ki.fork scope (printForever "Ki says hello")
    threadDelay 1_000_000
    putTextLn "Ki exit ---------------------------"

forkForkIOPrint :: IO ()
forkForkIOPrint = do
  _ <- forkIO (printForever "forkIO says hello")
  threadDelay 1_000_000
  putTextLn "forkIO exit -----------------------"

printForever :: Text -> IO ()
printForever t =
  forever $ do
    putTextLn t
    threadDelay 500_000
