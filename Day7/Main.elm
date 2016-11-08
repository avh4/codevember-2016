module Main exposing (..)

import Color exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import WebGL exposing (..)
import Html.App as Html
import AnimationFrame
import Html.Attributes exposing (width, height)


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
        , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
        , update = (\dt theta -> ( theta + dt / 1000, Cmd.none ))
        }



-- MESHES - create a cube in which each vertex has a position and color


type alias VertexAttributes =
    { color : Vec3
    , position : Vec3
    , normal : Vec3
    }


cube : Drawable VertexAttributes
cube =
    let
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
            [ face purple rft rfb rbb rbt
              -- right
            , face purple rft rfb lfb lft
              -- front
            , face purple rft lft lbt rbt
              -- top
            , face purple rfb lfb lbb rbb
              -- bottom
            , face purple lft lfb lbb lbt
              -- left
            , face purple rbt rbb lbb lbt
              -- back
            ]


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( VertexAttributes, VertexAttributes, VertexAttributes )
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

        normal =
            Math.Vector3.cross a b

        vertex position =
            VertexAttributes color position normal
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- VIEW


scene : Float -> List Renderable
scene t =
    let
        box x y =
            render vertexShader fragmentShader cube (uniforms x y (x + y) t)

        row x =
            [-5..5]
                |> List.map (box x)
    in
        [-5..5]
            |> List.map row
            |> List.concat


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , transform : Mat4
    , camera : Mat4
    , shade : Float
    , lightPos : Vec3
    , lightColor : Vec3
    , cameraPos : Vec3
    }


type alias Varyings =
    { vcolor : Vec3
    , vnormal : Vec3
    , vpos : Vec3
    }


uniforms : Float -> Float -> Float -> Float -> Uniforms
uniforms x y r t =
    let
        cameraPos =
            (vec3 0 -40 20)
    in
        { rotation =
            mul (makeRotate (3 * r) (vec3 0 1 0)) (makeRotate (2 * r) (vec3 1 0 0))
        , perspective = makePerspective 45 (3 / 2) 0.01 100
        , camera = makeLookAt cameraPos (vec3 0 0 0) (vec3 0 1 0)
        , cameraPos = cameraPos
        , shade = 0.8
        , transform =
            Math.Matrix4.identity
                |> translate3 (x * 2.5) (2.5 * y) 0
        , lightPos =
            vec3 0 -30 5
                |> Math.Matrix4.transform (makeRotate t Math.Vector3.k)
        , lightColor = vec3 1 1 1
        }



-- SHADERS


vertexShader : Shader VertexAttributes Uniforms Varyings
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
uniform mat4 transform;
varying vec3 vcolor;
varying vec3 vnormal;
varying vec3 vpos;
void main () {
    gl_Position = perspective * camera * rotation * transform * vec4(position, 1.0);
    vcolor = color;
    vnormal = normal;
    vpos = vec3(transform * vec4(position, 1.0));
}

|]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

precision mediump float;
uniform float shade;
uniform vec3 lightPos;
uniform vec3 lightColor;
uniform vec3 cameraPos;
varying vec3 vcolor;
varying vec3 vnormal;
varying vec3 vpos;
void main () {
    // Ambient lighting
    vec3 ambient = shade * vcolor;

    // Diffuse lighting
    vec3 norm = normalize(vnormal);
    vec3 lightDir = normalize(lightPos - vpos);
    float diff = max(dot(vnormal, lightDir), 0.0);
    vec3 diffuse = diff * lightColor;

    // Specular lighting
    float specularStrength = 0.8;
    vec3 cameraDir = normalize(cameraPos - vpos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(cameraDir, reflectDir), 0.0), 128.0);
    vec3 specular = specularStrength * spec * lightColor;

    vec3 result = (ambient + diffuse + specular) * vcolor;
    gl_FragColor = vec4(result, 1.0);
}

|]
