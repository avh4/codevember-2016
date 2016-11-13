module Main exposing (..)

import Collage
import Element
import Day13.Eye as Eye
import Html exposing (Html)
import Html.App
import Time exposing (Time)
import Animation


type alias Model =
    { blinkStart : Time
    , now : Time
    }


initialModel : Model
initialModel =
    { blinkStart = 0
    , now = 0
    }


type Msg
    = Tick Time
    | StartBlink Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { model | now = t }, Cmd.none )

        StartBlink t ->
            ( { model | blinkStart = t }, Cmd.none )


view : Model -> Html msg
view model =
    let
        closeAnimation =
            Animation.animation model.blinkStart
                |> Animation.from 1.0
                |> Animation.to 0
                |> Animation.duration 200

        openAnimation =
            Animation.animation model.blinkStart
                |> Animation.from 0.0
                |> Animation.to 1.0
                |> Animation.duration 200
                |> Animation.delay 200
    in
        Collage.collage 750
            500
            [ Eye.eye
                ((Animation.animate model.now closeAnimation)
                    + (Animation.animate model.now openAnimation)
                )
            ]
            |> Element.toHtml


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Time.every 20 Tick
                    , Time.every 3000 StartBlink
                    ]
        , update = update
        , view = view
        }
