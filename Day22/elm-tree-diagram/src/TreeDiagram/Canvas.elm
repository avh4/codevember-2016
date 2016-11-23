module TreeDiagram.Canvas exposing (draw)

{-| Provides a draw function for drawing trees as canvas images.

@docs draw
-}

import Collage exposing (..)
import Element exposing (..)
import TreeDiagram exposing (..)


canvasPosition : Coord -> Form -> Form
canvasPosition coord form =
    move coord form


canvasCompose : Int -> Int -> List Form -> Element
canvasCompose width height forms =
    collage width height forms


canvasTransform : Int -> Int -> Coord -> Coord
canvasTransform width height coord =
    coord


canvasDrawable : Drawable Form Element
canvasDrawable =
    Drawable canvasPosition canvasCompose canvasTransform


{-| Draws the tree using the provided functions for drawings nodes and edges.
    TreeLayout contains some more options for positioning the tree.
-}
draw : TreeLayout -> NodeDrawer a Form -> EdgeDrawer Form -> Tree a -> Element
draw layout drawNode drawLine tree =
    draw_ canvasDrawable layout drawNode drawLine tree
