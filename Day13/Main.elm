module Main exposing (..)

import Collage
import Element
import Day13.Eye as Eye exposing (Eye)
import Html exposing (Html)
import Html.App
import Time exposing (Time)
import Random
import Random.Extra
import Color exposing (Color)


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


map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Random.Generator a
    -> Random.Generator b
    -> Random.Generator c
    -> Random.Generator d
    -> Random.Generator e
    -> Random.Generator f
    -> Random.Generator g
    -> Random.Generator h
map7 f genA genB genC genD genE genF genG =
    Random.Extra.map6 f genA genB genC genD genE genF
        |> flip Random.Extra.andMap genG


whitesColorGenerator : Random.Generator Color
whitesColorGenerator =
    Random.map3 Color.hsl
        (Random.float (degrees -50) (degrees 60))
        (Random.float 0 1)
        (Random.float 0.9 1.0)


irisColorGenerator : Random.Generator Color
irisColorGenerator =
    Random.map3 Color.hsl
        (Random.float (degrees 0) (degrees 360))
        (Random.float 0.5 0.7)
        (Random.float 0.1 0.7)


skinColorGenerator : Random.Generator Color
skinColorGenerator =
    Random.map3 Color.hsl
        (Random.float (degrees 25) (degrees 45))
        (Random.Extra.constant 0.55)
        (Random.float 0.2 0.7)


randomEye : Random.Generator ( ( Float, Float ), Eye )
randomEye =
    Random.map2 (,)
        (Random.map2 (,) (Random.float -250 250) (Random.float -150 150))
        (map7 Eye
            (Random.float 50 250)
            (Random.float 0.2 0.7)
            (Random.float 0.2 0.9)
            (Random.Extra.constant 0)
            skinColorGenerator
            irisColorGenerator
            whitesColorGenerator
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
