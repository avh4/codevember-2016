module Main exposing (..)

import Animation
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes
import Html.Attributes
import Color


type alias Model =
    { shape : Animation.State
    , background : Animation.State
    }


initialModel : Model
initialModel =
    { shape =
        Animation.style
            [ Animation.points (regularPolygon 3)
            , Animation.translate (Animation.px 50) (Animation.px 50)
            , Animation.scale 15.0
            , Animation.fill Color.red
            , Animation.rotate (Animation.turn 0)
            ]
            |> Animation.queue
                [ Animation.loop
                    [ Animation.wait 500
                    , Animation.to
                        [ Animation.scale 30
                        , Animation.points (regularPolygon 50)
                        , Animation.fill Color.blue
                        , Animation.rotate (Animation.turn 0.6)
                        ]
                    , Animation.wait 500
                    , Animation.to
                        [ Animation.points (star 5)
                        , Animation.scale 25.0
                        , Animation.fill (Color.hsl (degrees 345) 0.6 0.7)
                        , Animation.rotate (Animation.turn 0.3)
                        , Animation.translate (Animation.px 75) (Animation.px 80)
                        ]
                    , Animation.wait 500
                    , Animation.to
                        [ Animation.points (regularPolygon 3)
                        , Animation.scale 15.0
                        , Animation.fill Color.red
                        , Animation.translate (Animation.px 50) (Animation.px 50)
                        ]
                    ]
                ]
    , background =
        Animation.style
            [ Animation.fill Color.black
            ]
            |> Animation.queue
                [ Animation.loop
                    [ Animation.wait 500
                    , Animation.to
                        [ Animation.fill Color.yellow
                        ]
                    , Animation.wait 500
                    , Animation.to
                        [ Animation.fill Color.orange
                        ]
                    , Animation.wait 500
                    , Animation.to
                        [ Animation.fill Color.black
                        ]
                    ]
                ]
    }


normalizePolygon : Int -> List ( Float, Float ) -> List ( Float, Float )
normalizePolygon targetNumberOfPoints originalPoints =
    let
        originalLength =
            List.length originalPoints
    in
        List.indexedMap
            (\i point ->
                List.repeat (targetNumberOfPoints // originalLength) point
            )
            originalPoints
            |> List.concat


regularPolygon : Int -> List ( Float, Float )
regularPolygon sides =
    List.range 1 sides
        |> List.map
            (\i ->
                ( cos (2 * pi * toFloat i / toFloat sides)
                , sin (2 * pi * toFloat i / toFloat sides)
                )
            )


star : Int -> List ( Float, Float )
star points =
    List.range 1 (points * 2)
        |> List.map
            (\i ->
                let
                    distance =
                        if i % 2 == 0 then
                            1.0
                        else
                            0.5
                in
                    ( distance * cos (2 * pi * toFloat i / toFloat points)
                    , distance * sin (2 * pi * toFloat i / toFloat points)
                    )
            )


type Msg
    = Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate animMsg ->
            ( { model
                | shape = Animation.update animMsg model.shape
                , background = Animation.update animMsg model.background
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Svg.svg
        [ Html.Attributes.style [ ( "max-height", "100vh" ) ]
        , Svg.Attributes.viewBox "0 0 100 100"
        ]
        [ Svg.rect
            ([ Svg.Attributes.width "100"
             , Svg.Attributes.height "100"
             , Svg.Attributes.x "0"
             , Svg.Attributes.y "0"
             ]
                ++ Animation.render model.background
            )
            []
        , Svg.polygon (Animation.render model.shape) []
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , subscriptions =
            \model ->
                Animation.subscription Animate
                    [ model.shape
                    , model.background
                    ]
        , update = update
        , view = view
        }
