module Scratch.Prelude
  ( module Scratch.Prelude,
    module X,
  )
where

{- ORMOLU_DISABLE -}

-- Re-exports

import Relude as X hiding (error, id, undefined)

import Control.Concurrent as X (forkIO, threadDelay)
import Data.Aeson as X (FromJSON(..), ToJSON(..))
import Data.Time as X
import Data.Traversable as X (for)
import Prelude as X (undefined) -- Warningless version for experiment code
import System.FilePath as X ((</>))
import System.IO.Unsafe as X

{- ORMOLU_ENABLE -}

-- For this module

import qualified Prelude

-- * Exception

panic :: HasCallStack => Text -> a
panic e =
  Prelude.error (toString e)

error :: Text -> a
error =
  Prelude.error . toString
{-# WARNING error "'error' remains in code" #-}

-- * General

toInt :: Integral a => a -> Int
toInt =
  fromIntegral

hush :: Either a b -> Maybe b
hush =
  rightToMaybe
