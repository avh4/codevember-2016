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
import AnimationFrame
import Color exposing (Color)
import Ease


type alias Model =
    { board : Maybe Board
    , selectedTile : Maybe Board.Position
    , now : Time
    , animation :
        Maybe ( Time, Board, List ( ( Board.Player, Board.Piece ), Board.Position, Board.Position ), List ( Color, Board.Position, Board.Position ) )
    }


initialModel : Model
initialModel =
    { board = Nothing
    , selectedTile = Nothing
    , now = 0
    , animation = Nothing
    }


type Msg
    = ClickTile Board.Position
    | CreateGame Random.Seed
    | Tick Time


updateMove : Board.Position -> Board.Position -> Model -> Model
updateMove from to model =
    let
        result =
            model.board
                |> Maybe.andThen (Board.makeMove from to)
    in
        case result of
            Just { newBoard, partialBoard, movingPieces, movingTiles } ->
                { model
                    | board = Just newBoard
                    , animation =
                        Just
                            ( model.now
                            , partialBoard
                            , movingPieces
                            , movingTiles
                            )
                }

            Nothing ->
                model


animationDuration : Time
animationDuration =
    1000


checkForAnimationEnd : Model -> Model
checkForAnimationEnd model =
    case model.animation of
        Nothing ->
            model

        Just ( startTime, _, _, _ ) ->
            if model.now >= startTime + animationDuration then
                { model | animation = Nothing }
            else
                model


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
                    ( model
                        |> updateMove selected pos
                        |> (\m -> { m | selectedTile = Nothing })
                    , Cmd.none
                    )

        CreateGame seed ->
            ( { board = Board.new seed |> Just
              , selectedTile = Nothing
              , now = 0
              , animation = Nothing
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model | now = model.now + dt }
                |> checkForAnimationEnd
            , Cmd.none
            )


renderPiece : Maybe msg -> Bool -> Board.Player -> Board.Piece -> Svg msg
renderPiece onClick isSelected player piece =
    Svg.g
        (case onClick of
            Nothing ->
                []

            Just handler ->
                [ Svg.Events.onClick handler ]
        )
        [ Svg.circle
            [ Svg.Attributes.r "4"
            , Svg.Attributes.fill <|
                case player of
                    Board.Player1 ->
                        "#111"

                    Board.Player2 ->
                        "#eee"
            , if isSelected then
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


renderBoard : Board -> Maybe Board.Position -> List (Svg Msg)
renderBoard board selectedTile =
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
                        renderPiece
                            (Just <| ClickTile pos)
                            (selectedTile == Just pos)
                            player
                            piece
                ]

        combineRow tiles =
            List.indexedMap
                (\col tile ->
                    Svg.g [ Svg.Attributes.transform ("translate(" ++ toString (col * 10) ++ ",0)") ]
                        [ tile ]
                )
                tiles
    in
        Board.view renderTile board
            |> List.indexedMap
                (\row tiles ->
                    Svg.g [ Svg.Attributes.transform ("translate(0," ++ toString (row * 10) ++ ")") ]
                        (combineRow tiles)
                )


view : Model -> Html Msg
view model =
    case ( model.animation, model.board ) of
        ( Nothing, Nothing ) ->
            Html.text ""

        ( Nothing, Just board ) ->
            renderBoard board model.selectedTile
                |> Svg.svg
                    [ Svg.Attributes.viewBox "-5 -5 80 80"
                    , Html.Attributes.style [ ( "max-height", "100vh" ) ]
                    ]

        ( Just ( startTime, partialBoard, movingPieces, movingTiles ), _ ) ->
            let
                p =
                    (model.now - startTime)
                        / animationDuration
                        |> max 0
                        |> min 1
                        |> Ease.inOutQuad

                interp a b =
                    toFloat a + p * toFloat (b - a)

                place ( x1, y1 ) ( x2, y2 ) svg =
                    -- r = (sqrt (x2 - x1) ^2 + (y2 - y1)^2) / 2
                    -- p0 = arccos ((cx - x1) / r)  -- x1 = cx + r * cos p0
                    -- x = cx + r * cos (p - p0)
                    -- y = cy + r * sin (p - p0)
                    let
                        cx =
                            toFloat (x1 + x2) / 2

                        cy =
                            toFloat (y1 + y2) / 2

                        r =
                            sqrt (toFloat (x2 - x1) ^ 2 + toFloat (y2 - y1) ^ 2) / 2

                        p0 =
                            acos ((cx - toFloat x1) / r)

                        x =
                            cx + -r * cos (p * pi - p0) - 1

                        y =
                            cy + -r * sin (p * pi - p0) - 1
                    in
                        [ svg ]
                            |> Svg.g
                                [ Svg.Attributes.transform <|
                                    "translate("
                                        ++ toString (x * 10)
                                        ++ ","
                                        ++ toString (y * 10)
                                        ++ ")"
                                ]
            in
                [ renderBoard partialBoard Nothing
                    |> Svg.g []
                , movingTiles
                    |> List.map
                        (\( color, from, to ) ->
                            Svg.rect
                                [ Svg.Attributes.x "-5"
                                , Svg.Attributes.y "-5"
                                , Svg.Attributes.width "10"
                                , Svg.Attributes.height "10"
                                , Svg.Attributes.fill (Color.Convert.colorToHex color)
                                , Html.Attributes.style [ ( "box-shadow", "-5px -5px 5px #888" ) ]
                                ]
                                []
                                |> place from to
                        )
                    |> Svg.g []
                , movingPieces
                    |> List.map
                        (\( ( player, piece ), from, to ) ->
                            renderPiece Nothing False player piece
                                |> place from to
                        )
                    |> Svg.g []
                ]
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
        , subscriptions =
            \model ->
                if model.animation == Nothing then
                    Sub.none
                else
                    AnimationFrame.diffs Tick
        , update = update
        , view = view
        }
