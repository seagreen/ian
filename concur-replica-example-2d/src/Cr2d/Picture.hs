module Cr2d.Picture (board) where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import Concur.Replica.DOM.Props (Props)
import Cr2d.Point
import Cr2d.Prelude
import Cr2d.Ship

import qualified Concur.Replica.DOM as Html
import qualified Concur.Replica.SVG as S
import qualified Concur.Replica.SVG.Props as SP
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

board :: Text -> HashMap Text Ship -> Widget HTML a
board ourName hm =
  S.svg [SP.width "400", SP.height "400", SP.version "1.1"] $
    [ S.rect [SP.x "0", SP.y "0", SP.width "400", SP.height "400"] []
    , originMarkerPicture originPos
    , shipPicture ourName (rotation ourShip) "green" (200, 200) -- our ship
    ]
    <> fmap f (HashMap.toList (HashMap.delete ourName hm)) -- enemy ships
    <> [S.text [SP.transform "translate(20 380)", SP.stroke "white"]
         [Html.text (  "x = " <> showText (round ourX :: Int)
                    <> ", y = " <> showText (round ourY :: Int)
                    )
         ]
       ]
  where
    originPos = boardPosition (position ourShip) (Point 0 0)

    ourShip :: Ship
    ourShip =
      case HashMap.lookup ourName hm of
        Nothing ->
          panic "ship not found"

        Just ship ->
          ship

    Point ourX ourY = position ourShip

    f :: (Text, Ship) -> Widget HTML a
    f (name, Ship pnt rotation _) =
      let
        (boardX, boardY) = boardPosition (position ourShip) pnt
      in
        shipPicture name rotation "red" (boardX, boardY)

boardPosition :: Point -> Point -> (Double, Double)
boardPosition (Point ourX ourY) (Point targetX targetY) =
  (adjustedX + 200, negate adjustedY + 200)
  where
    adjustedX = targetX - ourX

    adjustedY = targetY - ourY

shipPicture :: Text -> Double -> Text -> (Double, Double) -> Widget HTML a
shipPicture name rotation color (x, y) =
  S.g [SP.transform ("translate(" <> showText x <> " " <> showText y <> ")")]
    [ S.polygon [ points [(-w, -5), (0, 20), (w, -5)], SP.stroke color, SP.strokeWidth "5", SP.fill "none"
                , SP.transform ("rotate(" <> showText (negate (radiansToDegrees rotation) - 90) <> ")")
                ]
        []
    , S.text [SP.transform "translate(0 30)", SP.stroke "white"]
        [Html.text name]
    ]
  where
    w = 10

points :: [(Double, Double)] -> Props a
points xs =
  SP.points (Text.unwords (f <$> xs))
  where
    f :: (Double, Double) -> Text
    f (x, y) =
      Text.pack (show x) <> "," <> Text.pack (show y)

originMarkerPicture :: (Double, Double) -> Widget HTML a
originMarkerPicture (x, y) =
  S.g []
    [ S.line [SP.stroke "blue", SP.strokeWidth "5", SP.fill "none", SP.x1 (showText (x - 10)), SP.y1 (showText y), SP.x2 (showText (x + 10)), SP.y2 (showText y)] []
    , S.line [SP.stroke "blue", SP.strokeWidth "5", SP.fill "none", SP.x1 (showText x), SP.y1 (showText (y - 10)), SP.x2 (showText x), SP.y2 (showText (y + 10))] []
    ]

radiansToDegrees :: Double -> Double
radiansToDegrees n =
  (n * 180) / pi
