-- | General widgets that don't reference the game.
module Cr2d.Replica.Widget where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import Cr2d.Prelude

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as P
import qualified Concur.Replica.DOM.Props as P

-- | Modified from the concur-replica examples.
inputOnEnter
  :: Text -- ^ Placeholder
  -> Text -- ^ Starting value
  -> Widget HTML Text
inputOnEnter p v = do
  res <-
    H.input
      [ P.autofocus True
      , P.placeholder p
      , P.value v
      , Left <$> P.onInput
      , Right <$> P.onKeyDown
      ]
  case res of
    Left e ->
      inputOnEnter p (P.targetValue (P.target e))

    Right e ->
      if P.kbdKey e == "Enter"
        then
          pure v

        else
          inputOnEnter p v
