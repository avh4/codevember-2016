module Main exposing (..)

import Board exposing (Board)
import Html exposing (Html)
import Color.Convert
import Svg exposing (Svg)
import Svg.Attributes
import Html.Attributes
import Svg.Events


type alias Model =
    { board : Board
    , selectedTile : Maybe Board.Position
    }


initialModel : Model
initialModel =
    { board = Board.new
    , selectedTile = Nothing
    }


type Msg
    = ClickTile Board.Position


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
                        | board = Board.makeMove selected pos model.board
                        , selectedTile = Nothing
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    let
        renderTile pos tile =
            case tile of
                Board.DoesntExist ->
                    Svg.g [] []

                Board.Empty color ->
                    Svg.rect
                        [ Svg.Attributes.x "-5"
                        , Svg.Attributes.y "-5"
                        , Svg.Attributes.width "10"
                        , Svg.Attributes.height "10"
                        , Svg.Attributes.fill (Color.Convert.colorToHex color)
                        , Svg.Events.onClick (ClickTile pos)
                        ]
                        []

                Board.Occupied piece color ->
                    Svg.g
                        [ Svg.Events.onClick (ClickTile pos)
                        ]
                        [ renderTile pos (Board.Empty color)
                        , Svg.circle
                            [ Svg.Attributes.r "4"
                            , Svg.Attributes.fill <|
                                case piece of
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
                        ]

        combineRow tiles =
            List.indexedMap
                (\col tile ->
                    Svg.g [ Svg.Attributes.transform ("translate(" ++ toString (col * 10) ++ ",0)") ]
                        [ tile ]
                )
                tiles
    in
        Board.view renderTile model.board
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
        { init = ( initialModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
