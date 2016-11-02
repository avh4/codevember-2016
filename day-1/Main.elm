module Main exposing (..)

import Graphics.Render exposing (..)
import Color exposing (rgb)
import Random
import Random.Extra
import Html
import Html.Attributes
import Html.App


canvasSize =
    { width = 750, height = 500 }


type Move
    = Straight
    | TurnLeft
    | TurnRight


randomMove : Random.Generator Move
randomMove =
    Random.Extra.frequency
        [ ( 2, Random.Extra.constant Straight )
        , ( 1, Random.Extra.constant TurnLeft )
        , ( 1, Random.Extra.constant TurnRight )
        ]


type Direction
    = North
    | South
    | East
    | West


createPath : List Move -> List Point
createPath moves =
    let
        distance =
            24

        init =
            ( ( 0, 0 ), North, [] )

        step move ( ( x, y ), dir, result ) =
            let
                newDir =
                    case ( dir, move ) of
                        ( _, Straight ) ->
                            dir

                        ( North, TurnLeft ) ->
                            West

                        ( North, TurnRight ) ->
                            East

                        ( West, TurnLeft ) ->
                            South

                        ( West, TurnRight ) ->
                            North

                        ( East, TurnLeft ) ->
                            North

                        ( East, TurnRight ) ->
                            South

                        ( South, TurnLeft ) ->
                            East

                        ( South, TurnRight ) ->
                            West

                newPos =
                    case newDir of
                        North ->
                            ( x, y - distance )

                        South ->
                            ( x, y + distance )

                        East ->
                            ( x + distance, y )

                        West ->
                            ( x - distance, y )
            in
                ( newPos, newDir, newPos :: result )
    in
        List.foldl step init moves
            |> (\( _, _, result ) -> result)


view seedBase =
    case seedBase of
        Nothing ->
            rectangle canvasSize.width canvasSize.height
                |> solidFill (rgb 15 15 15)
                |> svg canvasSize.width canvasSize.height

        Just seed ->
            let
                drawLine l =
                    line l
                        { color = (rgb 220 200 220)
                        , width = 3
                        , cap = Square
                        , join = Sharp
                        , dashing = [ 80, 40 ]
                        , dashOffset = 0
                        }
            in
                group
                    [ rectangle canvasSize.width canvasSize.height
                        |> solidFill (rgb 15 15 15)
                    , [0..20]
                        |> List.map (\x -> Random.initialSeed (x + seed))
                        |> List.map (Random.step (Random.list 25 randomMove) >> fst)
                        |> List.map (createPath >> polyline >> drawLine)
                        |> group
                    ]
                    |> svg canvasSize.width canvasSize.height


type alias Model =
    Maybe Int


type alias Msg =
    Int


update msg model =
    Just msg


main : Program Never
main =
    Html.App.program
        { init = ( Nothing, Random.generate identity (Random.int -1000000 10000000) )
        , subscriptions = \_ -> Sub.none
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        }
