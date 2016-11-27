module Svg.Util exposing (..)

import Math.Vector2 exposing (..)


translateString : Vec2 -> String
translateString p =
    "translate("
        ++ (p |> getX |> toString)
        ++ ","
        ++ (p |> getY |> toString)
        ++ ")"


rotateString : Float -> String
rotateString angle =
    "rotate("
        ++ (toString <| 180 / pi * angle)
        ++ ")"
