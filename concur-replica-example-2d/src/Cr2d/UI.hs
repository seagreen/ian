module Cr2d.UI where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import qualified Concur.Replica.DOM as H
import qualified Cr2d.Picture as Picture
import Cr2d.Point
import Cr2d.Prelude hiding (div, span)
import Cr2d.Replica.Widget
import Cr2d.Ship
import qualified Data.HashMap.Strict as HashMap
import qualified Network.Wai.Handler.Replica as HR

startApp :: TVar UTCTime -> IORef (HashMap Text Ship) -> HR.Context -> Widget HTML a
startApp tm ref ctx = do
  name <- selectName ref
  chan <- liftIO (newKeypressChan ctx)
  playGame tm ref chan name

newKeypressChan :: HR.Context -> IO (TChan Natural)
newKeypressChan HR.Context {HR.registerCallback, HR.call} = do
  chan <- atomically newTChan
  cb <- registerCallback $ \key -> atomically (writeTChan chan key)
  call cb "window.onkeydown = function(event) { callCallback(arg, event.keyCode) };"
  pure chan

data NameChoiceResult
  = NameSuccess
  | NameAlreadyInUse
  deriving (Eq, Show)

selectName :: IORef (HashMap Text Ship) -> Widget HTML Text
selectName ref = do
  name <- inputOnEnter "Player name" mempty
  -- let name = "test"
  res <- liftIO $ atomicModifyIORef' ref (f name)
  case res of
    NameAlreadyInUse ->
      H.div
        []
        [ selectName ref,
          H.div [] [H.text ("Already in use: " <> name)]
        ]
    NameSuccess ->
      pure name
  where
    f :: Text -> HashMap Text Ship -> (HashMap Text Ship, NameChoiceResult)
    f name hm =
      case HashMap.lookup name hm of
        Nothing ->
          ( HashMap.insert name (Ship (Point 0 0) 0 Straight) hm,
            NameSuccess
          )
        Just _ ->
          ( hm,
            NameAlreadyInUse
          )

playGame :: TVar UTCTime -> IORef (HashMap Text Ship) -> TChan Natural -> Text -> Widget HTML a
playGame tm ref keypressChan name = do
  thisUpdate <- liftIO $ readTVarIO tm
  hm <- liftIO $ readIORef ref
  res <-
    H.div
      []
      [ Picture.board name hm
          <|> Nothing <$ checkForGameUpdate thisUpdate -- will return once a second has passed
          <|> Just <$> liftIO listenForKey
      ]
  case res of
    Nothing ->
      pure ()
    Just turn ->
      liftIO $ atomicModifyIORef' ref (changeTurn turn)

  playGame tm ref keypressChan name
  where
    checkForGameUpdate :: UTCTime -> Widget HTML ()
    checkForGameUpdate thisUpdate = do
      liftIO (waitForChange tm thisUpdate)
    changeTurn :: Turn -> HashMap Text Ship -> (HashMap Text Ship, ())
    changeTurn turn hm =
      (HashMap.adjust f name hm, ())
      where
        f :: Ship -> Ship
        f (Ship pos rotation _) =
          Ship pos rotation turn
    listenForKey :: IO Turn
    listenForKey = do
      keyCode <- atomically (readTChan keypressChan)
      case keyCode of
        37 ->
          pure Port
        38 ->
          pure Straight
        39 ->
          pure Starboard
        _ ->
          listenForKey

waitForChange :: TVar UTCTime -> UTCTime -> IO ()
waitForChange tm thisUpdate =
  atomically do
    lastUpdated <- readTVar tm
    when (thisUpdate == lastUpdated) retry
