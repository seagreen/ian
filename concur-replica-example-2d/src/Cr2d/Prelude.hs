{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}

-- Normally we use hlint to enforce importing Data.Text as Text,
-- but here we want to import it as X:
{-# HLINT ignore "Avoid restricted qualification" #-}

-- | Tweak the @Prelude@. Includes no domain logic.
module Cr2d.Prelude
  ( module Cr2d.Prelude,
    module X,
  )
where

{- ORMOLU_DISABLE -}

-- Re-exports:

import Prelude as X hiding (error, foldl, id, lookup)

import Control.Applicative as X
import Control.Concurrent as X
import Control.Concurrent.STM as X
import Control.Monad as X
import Control.Monad.IO.Class as X
import Data.Either as X
import Data.Fixed as X
import Data.Foldable as X
import Data.Maybe as X
import Data.IORef as X
import Data.Text.Encoding as X hiding (decodeUtf8)
import Data.Time as X
import Data.Traversable as X
import Data.Void as X
import Debug.Trace as X

import Data.ByteString as X (ByteString)
import Data.HashMap.Strict as X (HashMap)
import Data.Set as X (Set)
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Numeric.Natural as X (Natural)

-- Local stuff:

import GHC.Stack.Types (HasCallStack)

import qualified Data.Text as Text
import qualified Prelude

{- ORMOLU_ENABLE -}

identity :: a -> a
identity a =
  a

{-# WARNING error "'error' remains in code" #-}
error :: HasCallStack => [Char] -> a
error =
  Prelude.error

panic :: HasCallStack => Text -> a
panic =
  error . Text.unpack

showText :: Show a => a -> Text
showText =
  Text.pack . show
