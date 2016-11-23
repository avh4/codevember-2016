module TreeDiagram.Svg exposing (draw)

{-| Provides a draw function for drawing trees as SVGs.

@docs draw
-}

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SA
import TreeDiagram exposing (..)


svgPosition : Coord -> Svg msg -> Svg msg
svgPosition ( x, y ) svg =
    Svg.g
        [ SA.transform <| "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")" ]
        [ svg ]


svgCompose : Int -> Int -> List (Svg msg) -> Svg msg
svgCompose width height svgs =
    Svg.svg
        [ SA.width <| toString width
        , SA.height <| toString height
        ]
        [ Svg.g [] svgs ]


svgTransform : Int -> Int -> Coord -> Coord
svgTransform width height coord =
    let
        ( x, y ) =
            coord

        svgX =
            x + ((toFloat width) / 2)

        svgY =
            ((toFloat height) / 2) - y
    in
        ( svgX, svgY )


svgDrawable : Drawable (Svg msg) (Html msg)
svgDrawable =
    Drawable svgPosition svgCompose svgTransform


{-| Draws the tree using the provided functions for drawings nodes and edges.
    TreeLayout contains some more options for positioning the tree.
-}
draw : TreeLayout -> NodeDrawer a (Svg msg) -> EdgeDrawer (Svg msg) -> Tree a -> Html msg
draw layout drawNode drawLine tree =
    draw_ svgDrawable layout drawNode drawLine tree
