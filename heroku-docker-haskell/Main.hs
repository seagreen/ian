{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import System.Environment (getEnv)
import Prelude

main :: IO ()
main = do
  port <- getPort
  putStrLn ("Listening on port: " <> show port)
  Warp.runSettings (settings port) app
  where
    settings :: Int -> Warp.Settings
    settings port =
      Warp.defaultSettings
        { Warp.settingsHost = "0.0.0.0",
          Warp.settingsPort = port
        }

getPort :: IO Int
getPort =
  read <$> getEnv "PORT"

app :: Wai.Application
app _request respond =
  respond $
    Wai.responseLBS
      Http.status200
      [(Http.hContentType, "text/plain")]
      "heroku-docker-haskell example running."
