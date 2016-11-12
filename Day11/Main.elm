module Main exposing (..)

import Html exposing (Html)
import Html.App
import Html.Attributes
import WebGL


--

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html.App as Html
import AnimationFrame
import Html.Attributes exposing (width, height)


-- type alias Model =
--     ()
--
--
-- main : Program Never
-- main =
--     Html.App.beginnerProgram
--         { model = ()
--         , update = \msg model -> model
--         , view = view
--         }
--
--
-- view : Model -> Html msg
-- view model =
--     WebGL.toHtml
--         [ Html.Attributes.width 750
--         , Html.Attributes.height 500
--         ]
--         scene
--
--


type alias VertexAttributes =
    { position : Vec3
    }


type alias Uniforms =
    { camera : Mat4
    , time : Float
    }


type alias Varyings =
    { vposition : Vec3 }



--
-- scene : List WebGL.Renderable
-- scene =
--     [ WebGL.render vertexShader fragmentShader mesh uniforms
--     ]
--
--
-- uniforms : Uniforms
-- uniforms =
--     {}
--
--
-- mesh : WebGL.Drawable VertexAttributes
-- mesh =
--     WebGL.Triangle
--         [ ( VertexAttributes 0 0
--           , VertexAttributes 1 0
--           , VertexAttributes 1 1
--           )
--         , ( VertexAttributes 0 0
--           , VertexAttributes 0 1
--           , VertexAttributes 1 1
--           )
--         ]


main : Program Never
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = scene >> WebGL.toHtml [ width 750, height 500 ]
        , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
        , update = (\dt theta -> ( theta + dt / 1000, Cmd.none ))
        }



-- MESHES - create a cube in which each vertex has a position and color


cube : Drawable VertexAttributes
cube =
    let
        rft =
            vec3 1 1 0

        lft =
            vec3 -1 1 0

        lbt =
            vec3 -1 -1 0

        rbt =
            vec3 1 -1 0
    in
        Triangle
            << List.concat
        <|
            [ face rft lft lbt rbt
            ]


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( VertexAttributes, VertexAttributes, VertexAttributes )
face a b c d =
    let
        vertex position =
            VertexAttributes position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- VIEW


scene : Float -> List Renderable
scene t =
    [ render vertexShader fragmentShader cube (uniforms t) ]


uniforms : Float -> Uniforms
uniforms t =
    { camera = makeOrtho2D 0 1 0 0.66666
    , time = t
    }



-- SHADERS


vertexShader : Shader VertexAttributes Uniforms Varyings
vertexShader =
    [glsl|

attribute vec3 position;

uniform mat4 camera;

varying vec3 vposition;

void main () {
    gl_Position = camera * vec4(position, 1.0);
    vposition = position;
}

|]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

precision mediump float;

uniform float time;

varying vec3 vposition;

float hue2rgb(float f1, float f2, float hue) {
    if (hue < 0.0)
        hue += 1.0;
    else if (hue > 1.0)
        hue -= 1.0;
    float res;
    if ((6.0 * hue) < 1.0)
        res = f1 + (f2 - f1) * 6.0 * hue;
    else if ((2.0 * hue) < 1.0)
        res = f2;
    else if ((3.0 * hue) < 2.0)
        res = f1 + (f2 - f1) * ((2.0 / 3.0) - hue) * 6.0;
    else
        res = f1;
    return res;
}

vec3 hsl2rgb(vec3 hsl) {
    vec3 rgb;

    if (hsl.y == 0.0) {
        rgb = vec3(hsl.z); // Luminance
    } else {
        float f2;

        if (hsl.z < 0.5)
            f2 = hsl.z * (1.0 + hsl.y);
        else
            f2 = hsl.z + hsl.y - hsl.y * hsl.z;

        float f1 = 2.0 * hsl.z - f2;

        rgb.r = hue2rgb(f1, f2, hsl.x + (1.0/3.0));
        rgb.g = hue2rgb(f1, f2, hsl.x);
        rgb.b = hue2rgb(f1, f2, hsl.x - (1.0/3.0));
    }
    return rgb;
}

vec3 hsl2rgb(float h, float s, float l) {
    return hsl2rgb(vec3(h, s, l));
}




void main () {
  float x = vposition.x + 0.02 * sin(vposition.y * 20.0 + time);
  float y = vposition.y + 0.02 * sin(vposition.x * 13.0 + time / 3.0);

  float u = fract(10.0 * (x - y));
  float v = fract(10.0 * (x + y));

  float uc = pow(2.0 * u - 1.0, 6.0);
  float uv = pow(2.0 * v - 1.0, 6.0);

  // float final = max(uc, uv);

  vec3 color1 = uc * hsl2rgb(vposition.y, 0.7, 0.5);
  vec3 color2 = uv * hsl2rgb(vposition.x, 0.6, 0.7);

  gl_FragColor = vec4(color1 + color2 , 1.0);
}
|]
