module Main exposing (..)

import Graphics.Render exposing (..)
import Color exposing (Color)
import Animation exposing (..)
import Html.App
import Time exposing (Time)
import Ease


type alias Model =
    { startTime : Maybe Time
    , time : Time
    }


type Msg
    = Tick Time


update msg model =
    case msg of
        Tick newTime ->
            case model.startTime of
                Nothing ->
                    ( { model
                        | startTime = Just newTime
                        , time = 0
                      }
                    , Cmd.none
                    )

                Just startTime ->
                    ( { model | time = newTime - startTime }
                    , Cmd.none
                    )


view model =
    let
        lineYAnim =
            animation 500
                |> from -500
                |> to 0
                |> duration 700
                |> ease Ease.inOutQuint

        lineXAnim =
            animation 500
                |> from 50
                |> to 0
                |> duration 700
                |> ease Ease.inOutQuint

        diagLine del x =
            segment ( 0 + x, -250 ) ( x - 50, 250 )
                |> solid 11 (Color.rgb 255 250 250)
                |> move
                    (animate model.time (lineXAnim |> delay (50 * del)))
                    (animate model.time (lineYAnim |> delay (50 * del)))

        circleSizeAnim =
            animation 1500
                |> from 0
                |> to 1
                |> duration 400
                |> ease Ease.outElastic

        circle del scale x y =
            let
                size =
                    scale * 30 * (animate model.time (circleSizeAnim |> delay (del * 50)))
            in
                ellipse size size
                    |> solidFill (Color.rgb 213 251 217)
                    |> move x y
    in
        group
            [ rectangle 750 500
                |> solidFill (Color.rgb 251 53 35)
            , group
                [ diagLine 0 0
                , diagLine 1 -30
                , diagLine 2 -60
                , diagLine 3 -90
                , diagLine 4 -150
                ]
                |> move -50 0
            , circle 0 1.0 20 -50
            , circle 1 0.6 120 -140
            , circle 2 0.3 250 100
            , circle 3 1.7 180 120
            ]
            |> svg 750 500


main =
    Html.App.program
        { init = ( { startTime = Nothing, time = 0 }, Cmd.none )
        , subscriptions = \_ -> Time.every 20 Tick
        , update = update
        , view = view
        }
