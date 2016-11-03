module Main exposing (..)

import Graphics.Render exposing (..)
import Color exposing (Color)
import Html exposing (Html)
import Html.App
import Time


background : Color
background =
    Color.rgb 30 40 200


foreground : Color
foreground =
    Color.rgb 30 200 90


transparent : Color
transparent =
    Color.rgba 0 0 0 0


singleCircle : Float -> Form msg
singleCircle size =
    ellipse size size
        |> solidFillWithBorder transparent 4 foreground


setOfCircles : Int -> Form msg
setOfCircles offset =
    [0..25]
        |> List.map (\i -> i * 30 + (toFloat (offset % 30)))
        |> List.map singleCircle
        |> group


type alias Model =
    { offset : Int }


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd msg )
update () model =
    ( { model | offset = model.offset + 1 }, Cmd.none )


view : Model -> Html msg
view model =
    group
        [ rectangle 750 500 |> solidFill background
        , setOfCircles model.offset
        , setOfCircles model.offset |> move 70 70
        , setOfCircles model.offset |> move -70 70
        , setOfCircles model.offset |> move 70 -70
        , setOfCircles model.offset |> move -70 -70
        , setOfCircles model.offset |> move 240 150
        , setOfCircles model.offset |> move -240 150
        , setOfCircles model.offset |> move 240 -150
        , setOfCircles model.offset |> move -240 -150
        ]
        |> svg 750 500


main =
    Html.App.program
        { init = ( { offset = 0 }, Cmd.none )
        , subscriptions = \_ -> Time.every 100 (always ())
        , update = update
        , view = view
        }
