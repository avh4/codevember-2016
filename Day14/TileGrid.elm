module Day14.TileGrid exposing (Model, init, changeColors, update, view)

import Color exposing (Color)
import Collage
import Dict exposing (Dict)
import Time exposing (Time)
import Fifo exposing (Fifo)
import Animation
import Array
import Random
import Random.Array


outAnimation startTime =
    Animation.animation startTime
        |> Animation.from 1.0
        |> Animation.to 0.0
        |> Animation.duration 800


inAnimation startTime =
    Animation.animation startTime
        |> Animation.from 0.0
        |> Animation.to 1.0
        |> Animation.duration 800
        |> Animation.delay 800


type Model
    = Model
        { tiles : Dict ( Int, Int ) Color
        , currentAnimations : Dict ( Int, Int ) ( Time, Color )
        , pendingAnimations : Fifo ( ( Int, Int ), Color )
        , lastStartTime : Time
        }


fold : (Int -> Int -> a -> a) -> a -> a
fold step initial =
    let
        foldRow y acc =
            List.foldl (step y) acc [-2..2]
    in
        List.foldl foldRow initial [-2..2]


init : List Color -> Model
init colors =
    let
        colorFor x y =
            colors
                |> List.drop ((x + y) % List.length colors)
                |> List.head
                |> Maybe.withDefault Color.black

        insertTile y x dict =
            Dict.insert ( x, y ) (colorFor x y) dict
    in
        Model
            { tiles =
                fold insertTile Dict.empty
            , pendingAnimations = Fifo.empty
            , currentAnimations = Dict.empty
            , lastStartTime = 0
            }


changeColors : Time -> List Color -> Model -> Model
changeColors now newColors (Model model) =
    let
        colorFor x y =
            newColors
                |> List.drop ((x + y) % List.length newColors)
                |> List.head
                |> Maybe.withDefault Color.black

        newAnimations =
            fold
                (\x y acc -> Array.push ( ( x, y ), colorFor x y ) acc)
                Array.empty

        ( shuffledAnimations, newSeed ) =
            Random.step (Random.Array.shuffle newAnimations)
                (Random.initialSeed <| floor now)
    in
        Model
            { model
                | pendingAnimations =
                    Array.foldl
                        Fifo.insert
                        model.pendingAnimations
                        shuffledAnimations
            }
            |> startNewAnimations now


startNextTileAnimation : Time -> Model -> Model
startNextTileAnimation now (Model model) =
    case Fifo.remove model.pendingAnimations of
        ( Just ( ( x, y ), newColor ), remainingPendingsAnimations ) ->
            Model
                { model
                    | currentAnimations =
                        Dict.insert ( x, y ) ( now, newColor ) model.currentAnimations
                    , pendingAnimations = remainingPendingsAnimations
                }

        ( Nothing, remainingPendingsAnimations ) ->
            Model { model | pendingAnimations = remainingPendingsAnimations }


clearFinishedAnimations : Time -> Model -> Model
clearFinishedAnimations now (Model model) =
    let
        isDone key ( startTime, newColor ) =
            Animation.isDone now (inAnimation startTime)

        ( finishedAnimations, remainingAnimations ) =
            Dict.partition isDone model.currentAnimations

        finalizeColor ( x, y ) ( startTime, newColor ) acc =
            Dict.insert ( x, y ) newColor acc
    in
        Model
            { model
                | currentAnimations = remainingAnimations
                , tiles = Dict.foldl finalizeColor model.tiles finishedAnimations
            }


startNewAnimations : Time -> Model -> Model
startNewAnimations now (Model model) =
    if model.lastStartTime + 200 < now then
        Model { model | lastStartTime = now }
            |> startNextTileAnimation now
            |> startNextTileAnimation now
            |> startNextTileAnimation now
    else
        Model model


update : Time -> Model -> Model
update now (Model model) =
    (Model model)
        |> clearFinishedAnimations now
        |> startNewAnimations now


viewTile : Time -> Maybe ( Time, Color ) -> Color -> Collage.Form
viewTile now animation color =
    case animation of
        Nothing ->
            Collage.square 60
                |> Collage.filled color

        Just ( startTime, newColor ) ->
            Collage.group
                [ let
                    h =
                        30 * (Animation.animate now <| outAnimation startTime)

                    fw =
                        30 + 7 * (cos <| degrees 90 * (Animation.animate now <| outAnimation startTime))

                    bw =
                        30 - 7 * (cos <| degrees 90 * (Animation.animate now <| outAnimation startTime))
                  in
                    Collage.polygon [ ( -fw, -h ), ( -bw, h ), ( bw, h ), ( fw, -h ) ]
                        |> Collage.filled color
                , let
                    h =
                        30 * (Animation.animate now <| inAnimation startTime)

                    fw =
                        30 - 7 * (cos <| degrees 90 * (Animation.animate now <| inAnimation startTime))

                    bw =
                        30 + 7 * (cos <| degrees 90 * (Animation.animate now <| inAnimation startTime))
                  in
                    Collage.polygon [ ( -fw, -h ), ( -bw, h ), ( bw, h ), ( fw, -h ) ] |> Collage.filled newColor
                ]


view : Time -> Model -> Collage.Form
view now (Model model) =
    let
        tile y x =
            model.tiles
                |> Dict.get ( x, y )
                |> Maybe.map (viewTile now (Dict.get ( x, y ) model.currentAnimations))
                |> Maybe.withDefault (Collage.group [])
                |> Collage.move ( toFloat x * 70, 0 )

        tileRow y =
            [-2..2]
                |> List.map (tile y)
                |> Collage.group
                |> Collage.move ( 0, toFloat y * 70 )
    in
        [-2..2]
            |> List.map tileRow
            |> Collage.group
