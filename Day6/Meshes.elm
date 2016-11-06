module Day6.Meshes exposing (cube)

import Color exposing (..)
import Math.Vector3 exposing (..)
import WebGL exposing (..)


-- MESHES - create a cube in which each vertex has a position and color


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


cube : Color -> Drawable Vertex
cube color =
    let
        { hue, saturation, lightness, alpha } =
            Color.toHsl color

        lighter =
            Color.hsla hue saturation (lightness + 0.1) alpha

        darker =
            Color.hsla hue saturation (lightness - 0.1) alpha

        rft =
            vec3 1 1 1

        -- right, front, top
        lft =
            vec3 -1 1 1

        -- left,  front, top
        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
        Triangle
            << List.concat
        <|
            [ face color rft rfb rbb rbt
              -- right
            , face color rft rfb lfb lft
              -- front
            , face lighter rft lft lbt rbt
              -- top
            , face darker rfb lfb lbb rbb
              -- bottom
            , face lighter lft lfb lbb lbt
              -- left
            , face darker rbt rbb lbb lbt
              -- back
            ]


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    toRgb rawColor
            in
                vec3
                    (toFloat c.red / 255)
                    (toFloat c.green / 255)
                    (toFloat c.blue / 255)

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]
