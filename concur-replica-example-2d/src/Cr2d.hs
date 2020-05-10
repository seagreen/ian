module Cr2d where

import Concur.Replica (Attr(..), Attrs, HTML, VDOM(..), clientDriver)
import Cr2d.Prelude
import Cr2d.Ship (Ship)
import Cr2d.UI (startApp)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai.Middleware.Static (static)
import Network.WebSockets (defaultConnectionOptions)

import qualified Concur.Replica.Run
import qualified Cr2d.Game as Game
import qualified Data.Map as Map

run :: IO ()
run = do
  tm <- newTVarIO =<< getCurrentTime
  ref <- newIORef mempty
  _ <- forkIO $ forever $ updateThread tm ref
  Concur.Replica.Run.run
    8080
    index
    defaultConnectionOptions
    static
    (startApp tm ref)

updateThread :: TVar UTCTime -> IORef (HashMap Text Ship) -> IO ()
updateThread tm ref = do
  threadDelay 100_000
  atomicModifyIORef' ref f
  atomically . writeTVar tm =<< getCurrentTime
  where
    f :: HashMap Text Ship -> (HashMap Text Ship, ())
    f hm =
      (Game.update hm, ())

index :: HTML
index =
  [ VLeaf "!doctype" (fl [("html", ABool True)]) Nothing
  , VNode "html" mempty Nothing
      [ VNode "head" mempty Nothing
          [ VLeaf "meta" (fl [("charset", AText "utf-8")]) Nothing
          , VNode "title" mempty Nothing [VText "Title"]
          , VLeaf "link" (fl [ ("href", AText "custom.css")
                             , ("rel", AText "stylesheet")
                             ]) Nothing
          ]
      , VNode "body" mempty Nothing
          [ VNode "script" (fl [("language", AText "javascript")]) Nothing
              [ VRawText (decodeUtf8 clientDriver) ]
          ]
      ]
  ]
  where
    fl :: [(Text, Attr)] -> Attrs
    fl = Map.fromList
