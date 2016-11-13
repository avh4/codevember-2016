module Main exposing (..)

import Collage
import Element
import Day13.Eye as Eye exposing (Eye)
import Html exposing (Html)
import Html.App
import Time exposing (Time)
import Random


type alias Model =
    { eyes : List ( ( Float, Float ), Eye )
    , now : Time
    }


initialModel : Model
initialModel =
    { eyes = []
    , now = 0
    }


type Msg
    = Tick Time
    | StartBlink Time
    | NewEyes (List ( ( Float, Float ), Eye ))


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

        NewEyes eyes ->
            ( { model | eyes = eyes }
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



-- ( ( 0, 0 )
--   , { irisSize = 120
--     , pupilSize = 50
--     , eyeSize = 200
--     , blinkStart = 0
--     }
--   )
-- , ( ( 150, 150 )
--   , { irisSize = 60
--     , pupilSize = 25
--     , eyeSize = 100
--     , blinkStart = 0
--     }
--   )
-- pupilGenerator : Float -> Random.Generator Float
-- pupilGenerator irisSize =
--     Random.float (min (irisSize / 10) 10) (irisSize * 0.9)
--
--
-- irisGenerator : Float -> Random.Generator Float
-- irisGenerator eyeSize =
--     Random.float (min (eyeSize / 10) 10) (eyeSize * 0.9)


createEye : Float -> Float -> Float -> Eye
createEye eyeSize irisRatio pupilRatio =
    { irisSize = eyeSize * irisRatio
    , pupilSize = eyeSize * irisRatio * pupilRatio
    , eyeSize = eyeSize
    , blinkStart = 0
    }


randomEye : Random.Generator ( ( Float, Float ), Eye )
randomEye =
    Random.map2 (,)
        (Random.map2 (,) (Random.float -250 250) (Random.float -150 150))
        (Random.map3 createEye
            (Random.float 50 300)
            (Random.float 0.2 0.9)
            (Random.float 0.2 0.9)
        )


main : Program Never
main =
    Html.App.program
        { init =
            ( initialModel
            , Random.generate NewEyes (Random.list 3 randomEye)
            )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Time.every 40 Tick
                    , Time.every 3000 StartBlink
                    ]
        , update = update
        , view = view
        }
