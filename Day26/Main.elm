module Main exposing (..)

import Board exposing (Board)
import Html exposing (Html)
import Color.Convert
import Svg exposing (Svg)
import Svg.Attributes
import Html.Attributes
import Svg.Events
import Set exposing (Set)
import Random
import Time exposing (Time)
import Task


type alias Model =
    { board : Maybe Board
    , selectedTile : Maybe Board.Position
    }


initialModel : Model
initialModel =
    { board = Nothing
    , selectedTile = Nothing
    }


type Msg
    = ClickTile Board.Position
    | CreateGame Random.Seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickTile pos ->
            case model.selectedTile of
                Nothing ->
                    ( { model | selectedTile = Just pos }
                    , Cmd.none
                    )

                Just selected ->
                    ( { model
                        | board =
                            model.board
                                |> Maybe.map (Board.makeMove selected pos)
                        , selectedTile = Nothing
                      }
                    , Cmd.none
                    )

        CreateGame seed ->
            ( { board = Board.new seed |> Just
              , selectedTile = Nothing
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        renderTile pos tile piece =
            Svg.g []
                [ case tile of
                    Board.Tile [] ->
                        Svg.rect
                            [ Svg.Attributes.x "-5"
                            , Svg.Attributes.y "-5"
                            , Svg.Attributes.width "10"
                            , Svg.Attributes.height "10"
                            , Svg.Attributes.fill "transparent"
                            , Svg.Events.onClick (ClickTile pos)
                            ]
                            []

                    Board.Tile (color :: _) ->
                        Svg.rect
                            [ Svg.Attributes.x "-5"
                            , Svg.Attributes.y "-5"
                            , Svg.Attributes.width "10"
                            , Svg.Attributes.height "10"
                            , Svg.Attributes.fill (Color.Convert.colorToHex color)
                            , Svg.Events.onClick (ClickTile pos)
                            ]
                            []
                , case piece of
                    Nothing ->
                        Html.text ""

                    Just ( player, piece ) ->
                        Svg.g
                            [ Svg.Events.onClick (ClickTile pos)
                            ]
                            [ Svg.circle
                                [ Svg.Attributes.r "4"
                                , Svg.Attributes.fill <|
                                    case player of
                                        Board.Player1 ->
                                            "#111"

                                        Board.Player2 ->
                                            "#eee"
                                , if model.selectedTile == Just pos then
                                    Svg.Attributes.stroke "#22e"
                                  else
                                    Svg.Attributes.stroke "none"
                                ]
                                []
                            , case piece of
                                Board.BasicPiece ->
                                    Html.text ""

                                Board.TerraformingPiece shape ->
                                    Set.toList shape
                                        |> List.map
                                            (\( x, y ) ->
                                                Svg.rect
                                                    [ Svg.Attributes.x "-0.5"
                                                    , Svg.Attributes.y "-0.5"
                                                    , Svg.Attributes.width "1"
                                                    , Svg.Attributes.height "1"
                                                    , Svg.Attributes.fill <|
                                                        case player of
                                                            Board.Player1 ->
                                                                "#eee"

                                                            Board.Player2 ->
                                                                "#111"
                                                    , Svg.Attributes.transform
                                                        ("translate("
                                                            ++ toString (toFloat x * 1.2)
                                                            ++ ","
                                                            ++ toString (toFloat y * 1.2)
                                                            ++ ")"
                                                        )
                                                    ]
                                                    []
                                            )
                                        |> Svg.g []
                            ]
                ]

        combineRow tiles =
            List.indexedMap
                (\col tile ->
                    Svg.g [ Svg.Attributes.transform ("translate(" ++ toString (col * 10) ++ ",0)") ]
                        [ tile ]
                )
                tiles
    in
        case model.board of
            Nothing ->
                Html.text ""

            Just board ->
                Board.view renderTile board
                    |> List.indexedMap
                        (\row tiles ->
                            Svg.g [ Svg.Attributes.transform ("translate(0," ++ toString (row * 10) ++ ")") ]
                                (combineRow tiles)
                        )
                    |> Svg.svg
                        [ Svg.Attributes.viewBox "-5 -5 80 80"
                        , Html.Attributes.style [ ( "max-height", "100vh" ) ]
                        ]


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( initialModel
            , Time.now
                |> Task.map (floor >> Random.initialSeed)
                |> Task.perform CreateGame
            )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
