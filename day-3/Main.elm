module Main exposing (..)

import Graphics.Render exposing (..)
import Color exposing (Color)
import Html.App
import Html exposing (Html)
import Random


border =
    solidFillWithBorder (Color.rgba 0 0 0 0)


tile : Color -> Form msg
tile color =
    let
        arc r =
            group
                [ ellipse r r
                    |> border 10 color
                , rectangle (r * 2) 100
                    |> solidFill (Color.rgb 200 200 200)
                    |> move 0 50
                , segment ( -r, 0 ) ( -r, 50 )
                    |> solid 10 color
                , segment ( r, 0 ) ( r, 50 )
                    |> solid 10 color
                ]
    in
        group
            [ ellipse 100 100
                |> solidFill (Color.rgb 200 200 200)
            , rectangle 200 100
                |> solidFill (Color.rgb 200 200 200)
                |> move 0 50
            , arc 85
            , arc 65
            , arc 45
            ]


row : Color -> Color -> Float -> Form msg
row color1 color2 tileWidth =
    let
        colorFor i =
            if i % 3 == 0 then
                color2
            else
                color1

        makeTile i =
            tile (colorFor i) |> move (toFloat i * tileWidth) 0
    in
        [0..6]
            |> List.map makeTile
            |> group


view : Model -> Html msg
view model =
    let
        tileWidth =
            190
    in
        group
            [ rectangle 750 500
                |> solidFill (Color.rgb 240 240 240)
            , [0..7]
                |> List.map
                    (\i ->
                        row model.color1 model.color2 tileWidth
                            |> move
                                (if i % 2 == 0 then
                                    tileWidth / 2 + tileWidth
                                 else
                                    0
                                )
                                (100 * toFloat i)
                    )
                |> group
                |> move -600 -300
                |> move (model.shiftX * -tileWidth) (model.shiftY * -200)
            ]
            |> svg 750 500


type alias Model =
    { color1 : Color
    , color2 : Color
    , shiftX : Float
    , shiftY : Float
    }


type Msg
    = NewModel Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel newModel ->
            ( newModel, Cmd.none )


colorGenerator : Random.Generator Model
colorGenerator =
    let
        color1 =
            Random.map3 Color.rgb
                (Random.int 0 100)
                (Random.int 0 100)
                (Random.int 0 100)

        color2 =
            Random.map3 Color.rgb
                (Random.int 80 180)
                (Random.int 80 180)
                (Random.int 80 180)
    in
        Random.map4 Model
            color1
            color2
            (Random.float 0 1)
            (Random.float 0 1)


main : Program Never
main =
    Html.App.program
        { init =
            ( { color1 = Color.rgb 70 30 70
              , color2 = Color.rgb 10 30 70
              , shiftX = 0
              , shiftY = 0
              }
            , Random.generate NewModel colorGenerator
            )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
