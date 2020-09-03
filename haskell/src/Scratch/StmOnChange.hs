module Scratch.StmOnChange where

import Control.Concurrent
import Control.Concurrent.STM (retry)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Relude

onChange :: Eq a => a -> TVar a -> STM a
onChange current v =
  onChangeSTM current (readTVar v)

onChangeSTM :: Eq a => a -> STM a -> STM a
onChangeSTM current s = do
  new <- s
  if current == new
    then retry
    else pure new

-- | Blocks while the @TVar@ is read,
-- then forks a thread that does an action when it changes.
onChangeThread :: forall a. Eq a => TVar a -> (a -> IO ()) -> IO ()
onChangeThread v action = do
  a <- atomically (readTVar v)
  void (forkIO (f a))
  where
    f :: a -> IO ()
    f a = do
      b <- atomically (onChange a v)
      action b
      f b

-- | Blocks while the @TVar@ is read,
-- then forks a thread that serializes it to a file on change.
writeJsonThread :: forall a. (Eq a, ToJSON a) => FilePath -> TVar a -> IO ()
writeJsonThread path v =
  onChangeThread v (LBS.writeFile path . encode)
