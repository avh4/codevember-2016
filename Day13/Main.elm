module Main exposing (..)

import Collage
import Element
import Day13.Eye as Eye exposing (Eye)
import Html exposing (Html)
import Html.App
import Time exposing (Time)


type alias Model =
    { eyes : List ( ( Float, Float ), Eye )
    , now : Time
    }


initialModel : Model
initialModel =
    { eyes =
        [ ( ( 0, 0 )
          , { irisSize = 120
            , pupilSize = 50
            , eyeSize = 200
            , blinkStart = 0
            }
          )
        , ( ( 150, 150 )
          , { irisSize = 60
            , pupilSize = 25
            , eyeSize = 100
            , blinkStart = 0
            }
          )
        ]
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
            ( { model
                | eyes =
                    List.map (\( p, eye ) -> ( p, Eye.startBlink t eye ))
                        model.eyes
              }
            , Cmd.none
            )


view : Model -> Html msg
view model =
    model.eyes
        |> List.map
            (\( ( x, y ), eye ) ->
                Eye.view model.now eye
                    |> Collage.move ( x, y )
            )
        |> Collage.collage 750 500
        |> Element.toHtml


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Time.every 40 Tick
                    , Time.every 3000 StartBlink
                    ]
        , update = update
        , view = view
        }
