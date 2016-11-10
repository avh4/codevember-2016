module Main exposing (..)

import Color exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html.App as Html
import AnimationFrame
import Html.Attributes exposing (width, height)


main : Program Never
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = scene >> WebGL.toHtml [ width 400, height 400 ]
        , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
        , update = (\dt theta -> ( theta + dt / 5000, Cmd.none ))
        }



-- MESHES - create a cube in which each vertex has a position and color


type alias Vertex =
    { color : Vec3
    , position : Vec3
    , normal : Vec3
    }


triangle : Drawable Vertex
triangle =
    let
        at =
            vec3 0 0 1

        bt =
            vec3 1 0 1

        ct =
            vec3 0 1 1

        ab =
            vec3 0 0 0

        bb =
            vec3 1 0 0

        cb =
            vec3 0 1 0
    in
        Triangle
            << List.concat
        <|
            [ tri (Color.rgb 255 0 255) at bt ct
            , tri Color.purple ab bb cb
            , face Color.blue at bt bb ab
            , face Color.green bt ct cb bb
            , face Color.yellow ct at ab cb
            ]


tri : Color -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
tri color a b c =
    let
        normal =
            Math.Vector3.cross a b
    in
        [ ( Vertex (colorToVec color) a normal
          , Vertex (colorToVec color) b normal
          , Vertex (colorToVec color) c normal
          )
        ]


colorToVec : Color -> Vec3
colorToVec rawColor =
    let
        c =
            toRgb rawColor
    in
        vec3
            (toFloat c.red / 255)
            (toFloat c.green / 255)
            (toFloat c.blue / 255)


square : Drawable Vertex
square =
    let
        rft =
            -- right, front, top
            vec3 1 1 1

        lft =
            -- left,  front, top
            vec3 0 1 1

        lbt =
            vec3 0 0 1

        rbt =
            vec3 1 0 1

        rbb =
            vec3 1 0 0

        rfb =
            vec3 1 1 0

        lfb =
            vec3 0 1 0

        lbb =
            vec3 0 0 0
    in
        Triangle
            << List.concat
        <|
            [ face green rft rfb rbb rbt
              -- right
            , face blue rft rfb lfb lft
              -- front
            , face yellow rft lft lbt rbt
              -- top
            , face red rfb lfb lbb rbb
              -- bottom
            , face purple lft lfb lbb lbt
              -- left
            , face orange rbt rbb lbb lbt
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

        normal =
            Math.Vector3.cross a b

        vertex position =
            Vertex color position normal
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- VIEW


scene : Float -> List Renderable
scene t =
    [ render vertexShader fragmentShader square (uniforms (Color.rgb 0x83 0xC8 0x33) 0 0 1 -45 t)
    , render vertexShader fragmentShader triangle (uniforms (Color.rgb 0xEF 0xA5 0x00) 0 0 1 45 t)
    , render vertexShader fragmentShader triangle (uniforms (Color.rgb 0x5A 0x63 0x78) 0 0 2 135 t)
    , render vertexShader fragmentShader triangle (uniforms (Color.rgb 0x5F 0xB4 0xCA) 0 0 2 225 t)
    , render vertexShader fragmentShader triangle (uniforms (Color.rgb 0xEF 0xA5 0x00) (sqrt 0.5) (-1 * sqrt 0.5) 1 -45 t)
    , render vertexShader fragmentShader triangle (uniforms (Color.rgb 0x5F 0xB4 0xCA) (2 * sqrt 0.5) (2 * sqrt 0.5) (sqrt 2) 180 t)
    ]


type alias Uniforms =
    { rotation : Mat4
    , transform : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    , shapeColor : Vec3
    , cameraPos : Vec3
    , lightColor : Vec3
    , lightPos : Vec3
    }


uniforms : Color -> Float -> Float -> Float -> Float -> Float -> Uniforms
uniforms color x y scale angle t =
    let
        cameraPos =
            (vec3 2 3 5)
    in
        { rotation =
            mul (makeRotate (3 * t) (vec3 0 1 0)) (makeRotate (2 * t) (vec3 1 0 0))
        , transform =
            Math.Matrix4.identity
                |> Math.Matrix4.translate (vec3 x y 0)
                |> Math.Matrix4.scale (vec3 scale scale 1)
                |> Math.Matrix4.rotate (degrees angle) Math.Vector3.k
        , perspective = makePerspective 45 1 0.01 100
        , camera = makeLookAt cameraPos (vec3 0 0 0) (vec3 0 1 0)
        , shade = 0.8
        , shapeColor = colorToVec color
        , cameraPos = cameraPos
        , lightColor = vec3 1 1 1
        , lightPos = vec3 -2 2 4
        }



-- SHADERS


type alias Varyings =
    { vcolor : Vec3
    , vnormal : Vec3
    , vpos : Vec3
    }


vertexShader : Shader { attr | position : Vec3, color : Vec3, normal : Vec3 } Uniforms Varyings
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 normal;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
uniform mat4 transform;
uniform vec3 shapeColor;
varying vec3 vcolor;
varying vec3 vnormal;
varying vec3 vpos;
void main () {
    gl_Position = perspective * camera * rotation * transform * vec4(position, 1.0);
    vcolor = shapeColor;
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
