module Main exposing (..)

import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html.App as Html
import AnimationFrame
import Html.Attributes exposing (width, height)
import Day6.Meshes exposing (cube)
import Animation exposing (..)
import Color exposing (Color)


type alias Model =
    Float


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg theta =
    case msg of
        Tick dt ->
            ( theta + dt / 1000, Cmd.none )


main : Program Never
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view =
            scene
                >> WebGL.toHtml
                    [ width 750
                    , height 500
                    , Html.Attributes.style [ ( "background-color", "black" ) ]
                    ]
        , subscriptions = (\model -> AnimationFrame.diffs Tick)
        , update = update
        }



-- VIEW


cubes =
    { red = cube Color.red
    , blue = cube Color.blue
    , green = cube Color.green
    , purple = cube Color.purple
    , yellow = cube Color.yellow
    }


scene : Model -> List Renderable
scene t =
    let
        angleAnim =
            animation 1 |> from 0 |> to (degrees 360) |> duration 1

        angle d =
            animate t (angleAnim |> delay d)
    in
        [ render vertexShader fragmentShader (cubes.red) (uniforms 0 0 (angle 0))
        , render vertexShader fragmentShader (cubes.blue) (uniforms -1 0 (angle 0))
        , render vertexShader fragmentShader (cubes.green) (uniforms 1 0 (angle 0))
        , render vertexShader fragmentShader (cubes.purple) (uniforms 0 1 (angle 0.4))
        , render vertexShader fragmentShader (cubes.yellow) (uniforms 0 -1 (angle -0.4))
        ]


type alias Uniforms =
    { rotation : Mat4
    , transform : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Int -> Int -> Float -> Uniforms
uniforms x y angle =
    { transform =
        Math.Matrix4.identity
            |> translate3 (3 * toFloat x) (3 * toFloat y) 0
    , rotation =
        (makeRotate angle (vec3 0 1 0))
    , perspective = makePerspective 45 (3 / 2) 0.01 100
    , camera =
        makeLookAt
            (vec3 0 0 15)
            (vec3 0 0 0)
            (vec3 0 1 0)
    , shade = 0.8
    }



-- SHADERS


vertexShader : Shader { position : Vec3, color : Vec3 } Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
uniform mat4 transform;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * rotation * transform * vec4(position, 1.0);
    vcolor = color;
}

|]


fragmentShader : Shader {} { u | shade : Float } { vcolor : Vec3 }
fragmentShader =
    [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
