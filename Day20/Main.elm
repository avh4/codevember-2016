module Main exposing (..)

import Svg exposing (Svg)
import Svg.Attributes
import Dict exposing (Dict)
import Html.Attributes
import Html
import Html.Events
import Random
import List.Extra


type alias Model =
    { cols : Int
    , rows : Int
    , cells : Dict ( Int, Int ) Tile
    , initialCells : Dict ( Int, Int ) Tile
    }


type alias Tile =
    { topSkew : Float
    , bottomSkew : Float
    , leftSkew : Float
    , rightSkew : Float
    }


initialModel : Model
initialModel =
    { cols = 4
    , rows = 4
    , cells = Dict.empty
    , initialCells = Dict.empty
    }


tile : Float -> ( ( Int, Int ), Tile ) -> Svg msg
tile rotation ( ( x, y ), tile ) =
    let
        corner x0 y0 horizSkew vertSkew =
            -- y1 = y0 + x1*sin(bottom)
            -- x2 = x0 + y2*sin(right)
            --
            -- x1 == x2, y1 == y2
            -- y1 = y0 + (x0 + y2*sin(right))*sin(bottom)
            -- y1 = y0 + (x0 + y1*sin(right))*sin(bottom)
            -- y1 = y0 + x0*sin(bottom) + y1*sin(right)*sin(bottom)
            -- y1 - y1*sin(right)*sin(bottom) = y0 + x0*sin(bottom)
            -- y1 * (1 - sin(right)*sin(bottom)) = y0 + x0*sin(bottom)
            -- y1 = (y0 + x0*sin(bottom)) / (1 - sin(right)*sin(bottom))
            -- x2 = x0 + y1*sin(right)
            let
                y =
                    (y0 + x0 * sin horizSkew)
                        / (1 - sin vertSkew * sin horizSkew)

                x =
                    x0 + y * sin vertSkew
            in
                toString x ++ "," ++ toString y

        a =
            corner -5 -5 tile.topSkew tile.leftSkew

        b =
            corner 5 -5 tile.topSkew tile.rightSkew

        c =
            corner 5 5 tile.bottomSkew tile.rightSkew

        d =
            corner -5 5 tile.bottomSkew tile.leftSkew
    in
        Svg.polygon
            [ Svg.Attributes.points (String.join " " [ a, b, c, d ])
            , Svg.Attributes.transform
                ("translate("
                    ++ toString (5 + x * 15)
                    ++ ","
                    ++ toString (5 + y * 15)
                    ++ ")"
                    ++ " rotate("
                    ++ toString -(rotation / pi * 180)
                    ++ ")"
                )
            , Svg.Attributes.fill "#763"
            ]
            []


view : Model -> Svg Msg
view model =
    let
        rotations =
            List.foldl
                (\( ( x, y ), tile ) rotationData ->
                    Dict.get ( x - 1, y ) model.cells
                        |> Maybe.map .rightSkew
                        |> Maybe.map (\r -> r - tile.leftSkew)
                        |> Maybe.withDefault 0
                        |> (+) (Dict.get ( x - 1, y ) rotationData |> Maybe.withDefault 0)
                        |> (\value -> Dict.insert ( x, y ) value rotationData)
                )
                Dict.empty
                (Dict.toList model.cells)
    in
        Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "-10"
                , Html.Attributes.max "10"
                , Html.Events.onInput DebugRight
                ]
                []
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "-10"
                , Html.Attributes.max "10"
                , Html.Events.onInput DebugLeft
                ]
                []
            , Svg.svg
                [ Svg.Attributes.viewBox "0 0 100 100"
                , Html.Attributes.style
                    [ ( "max-height", "100vh" )
                    ]
                ]
                [ Svg.rect
                    [ Svg.Attributes.fill "#eeb"
                    , Svg.Attributes.x "0"
                    , Svg.Attributes.y "0"
                    , Svg.Attributes.width "100"
                    , Svg.Attributes.height "100"
                    ]
                    []
                , Dict.toList model.cells
                    |> List.map
                        (\( ( x, y ), tileData ) ->
                            tile
                                (Dict.get ( x, y ) rotations |> Maybe.withDefault 0)
                                ( ( x, y ), tileData )
                        )
                    |> Svg.g
                        [ Svg.Attributes.transform
                            ("translate("
                                ++ (toString <| (100 - 15 * toFloat model.cols) / 2)
                                ++ ","
                                ++ (toString <| (100 - 15 * toFloat model.rows) / 2)
                                ++ ")"
                            )
                        ]
                ]
            ]


type Msg
    = DebugRight String
    | DebugLeft String
    | NewTileSkews (List Tile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTileSkews tiles ->
            tiles
                |> List.Extra.greedyGroupsOf model.cols
                |> List.indexedMap
                    (\y tilesInRow ->
                        tilesInRow
                            |> List.indexedMap
                                (\x tile ->
                                    ( ( x, y ), tile )
                                )
                    )
                |> List.concat
                |> Dict.fromList
                |> (\newCells ->
                        ( { model
                            | cells = newCells
                            , initialCells = newCells
                          }
                        , Cmd.none
                        )
                   )

        DebugRight string ->
            case String.toFloat string of
                Ok f ->
                    ( { model
                        | cells =
                            Dict.map
                                (\_ cell ->
                                    { cell
                                        | rightSkew = cell.rightSkew + degrees f
                                    }
                                )
                                model.initialCells
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        DebugLeft string ->
            case String.toFloat string of
                Ok f ->
                    ( { model
                        | cells =
                            Dict.map
                                (\_ cell ->
                                    { cell
                                        | leftSkew = cell.leftSkew + degrees f
                                    }
                                )
                                model.initialCells
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


tileSkews : Random.Generator Tile
tileSkews =
    Random.map4 Tile
        (Random.float (degrees -15) (degrees 15))
        (Random.float (degrees -15) (degrees 15))
        (Random.float (degrees -15) (degrees 15))
        (Random.float (degrees -15) (degrees 15))


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( initialModel
            , Random.generate NewTileSkews
                (Random.list
                    (initialModel.rows * initialModel.cols)
                    tileSkews
                )
            )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
