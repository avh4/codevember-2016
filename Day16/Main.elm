module Main exposing (..)

import Html exposing (Html)
import Collage
import Element
import Color
import Transform
import AnimationFrame
import Time exposing (Time)


type alias Model =
    { now : Time }


initialModel : Model
initialModel =
    { now = 0 }


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            ( { model | now = now }, Cmd.none )


type alias Wheel =
    { width : Float
    , height : Float
    }


wheel : Wheel -> Collage.Form
wheel model =
    Collage.group
        [ Collage.oval model.width model.height |> Collage.filled Color.white
        , Collage.oval (model.width - 50) (model.height - 50) |> Collage.filled Color.green
        , Collage.rect 10 model.height |> Collage.filled Color.green
        ]


type alias Biker =
    { torsoWidth : Float
    , torsoHeight : Float
    , torsoAngle : Float
    , headSize : Float
    , headTilt : Float
    , t : Float
    }


biker : Biker -> Collage.Form
biker model =
    let
        head color =
            Collage.group
                [ [ Collage.circle (model.headSize / 2 + 10)
                        |> Collage.filled color
                  , Collage.rect (model.headSize + 20) (model.headSize / 2 + 10)
                        |> Collage.filled Color.green
                        |> Collage.move ( 0, -(model.headSize / 2 + 10) / 2 )
                  ]
                    |> Collage.groupTransform
                        (Transform.rotation -model.headTilt)
                , Collage.circle (model.headSize / 2)
                    |> Collage.filled color
                ]

        body color =
            [ head color
                |> Collage.move ( 0, model.torsoHeight / 2 + (model.headSize / 2) + 10 )
            , Collage.rect model.torsoWidth model.torsoHeight
                |> Collage.filled color
            , Collage.rect 20 40
                |> Collage.filled color
                |> Collage.rotate (degrees 80)
                |> Collage.move ( model.torsoWidth / 2, model.torsoHeight * 0.35 )
            ]
                |> Collage.groupTransform
                    (Transform.identity
                        |> Transform.multiply (Transform.translation 0 (model.torsoHeight / 2))
                        |> Transform.multiply (Transform.rotation -model.torsoAngle)
                    )

        leg movement =
            [ Collage.group
                [ Collage.rect 20 60 |> Collage.filled Color.white
                , [ Collage.rect 20 40
                        |> Collage.filled Color.white
                        |> Collage.move ( 0, -20 )
                  ]
                    |> Collage.groupTransform
                        (Transform.identity
                            |> Transform.multiply (Transform.rotation (degrees -(2 * movement)))
                            |> Transform.multiply (Transform.translation 0 -30)
                        )
                ]
                |> Collage.move ( 0, -30 )
            ]
                |> Collage.groupTransform
                    (Transform.identity
                        |> Transform.multiply (Transform.rotation (degrees movement))
                        |> Transform.multiply (Transform.rotation (degrees 30))
                    )
    in
        Collage.group
            [ body Color.white
            , leg (25 + -(50 / 2) * sin (model.t / 200))
            , leg (25 + (50 / 2) * sin (model.t / 200))
            ]


bike : Float -> Collage.Form
bike t =
    let
        wheelModel =
            { width = 80, height = 120 }
    in
        Collage.group
            [ wheel wheelModel |> Collage.move ( -80, -wheelModel.height / 2 )
            , wheel wheelModel |> Collage.move ( 80, -wheelModel.height / 2 )
            , biker
                { torsoWidth = 35
                , torsoHeight = 100
                , torsoAngle = degrees 30
                , headSize = 30
                , headTilt = degrees -30
                , t = t
                }
                |> Collage.move ( -30, 0 )
            ]


view : Model -> Html msg
view model =
    [ Collage.rect 300 400
        |> Collage.filled Color.green
    , bike model.now
        |> Collage.move ( 0, -60 )
    , Collage.path [ ( -120, -30 ), ( 0, 0 ), ( 120, -30 ) ]
        |> Collage.traced
            { color = Color.white
            , width = 20
            , cap = Collage.Flat
            , join = Collage.Sharp 10
            , dashing = []
            , dashOffset = 0
            }
        |> Collage.move ( 0, 170 + -10 * cos (0.3 + model.now / 1000) )
    , Collage.path [ ( -120, -30 ), ( 0, 0 ), ( 120, -30 ) ]
        |> Collage.traced
            { color = Color.white
            , width = 20
            , cap = Collage.Flat
            , join = Collage.Sharp 10
            , dashing = []
            , dashOffset = 0
            }
        |> Collage.move ( 0, 130 + -10 * cos (model.now / 1000) )
    ]
        |> Collage.collage 750 500
        |> Element.toHtml


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , subscriptions = \_ -> AnimationFrame.times Tick
        , update = update
        , view = view
        }
