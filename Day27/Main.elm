module Main exposing (..)

import Svg exposing (Svg)
import Svg.Attributes
import Html exposing (Html)
import Mouse
import Math.Vector2 exposing (..)
import AnimationFrame
import Time exposing (Time)
import Coin exposing (..)
import Svg.Util exposing (..)
import Window
import Task
import Random


type alias Point =
    Vec2


type alias Model =
    { flatCoins : List FlatCoin
    , flippingCoins : List FlippingCoin
    , fallingCoins : List FallingCoin
    , windowSize : Window.Size
    }


initialModel : Model
initialModel =
    { flatCoins = []
    , flippingCoins = []
    , fallingCoins = []
    , windowSize = { width = 0, height = 0 }
    }


type Msg
    = MouseMove Mouse.Position
    | Tick Time
    | WindowResized Window.Size
    | NewCoins (List FlatCoin)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove { x, y } ->
            let
                mousePosition =
                    (vec2 (toFloat x) (toFloat y))

                containsMouse coin =
                    (distance mousePosition coin.center)
                        <= coin.radius

                ( coinsToFlip, coinsToKeep ) =
                    List.partition containsMouse model.flatCoins

                ( stoppedFlipping, stillFlipping, flipped ) =
                    List.foldl
                        (\coin ( stopped, still, flipped ) ->
                            case pushFlippingCoin mousePosition coin of
                                Stopped flatCoin ->
                                    ( flatCoin :: stopped, still, flipped )

                                Continue flippingCoin ->
                                    ( stopped, flippingCoin :: still, flipped )

                                Flipped flippedCoin ->
                                    ( stopped, still, flippedCoin :: flipped )
                        )
                        ( [], [], [] )
                        model.flippingCoins
            in
                ( { model
                    | flatCoins = coinsToKeep ++ stoppedFlipping
                    , flippingCoins =
                        coinsToFlip
                            |> List.map (startFlip mousePosition)
                            |> List.append stillFlipping
                    , fallingCoins =
                        flipped
                            |> List.append model.fallingCoins
                  }
                , Cmd.none
                )

        Tick t ->
            let
                ( finishedFalling, stillFalling ) =
                    List.foldl
                        (\coin ( stopped, still ) ->
                            case advanceFallingCoin coin of
                                Err flatCoin ->
                                    ( flatCoin :: stopped, still )

                                Ok flippingCoin ->
                                    ( stopped, flippingCoin :: still )
                        )
                        ( [], [] )
                        model.fallingCoins
            in
                ( { model
                    | fallingCoins = stillFalling
                    , flatCoins = finishedFalling ++ model.flatCoins
                  }
                , Cmd.none
                )

        WindowResized newSize ->
            ( { model | windowSize = newSize }
            , if
                (List.length model.flatCoins
                    + List.length model.flippingCoins
                    + List.length model.fallingCoins
                )
                    == 0
              then
                Random.generate NewCoins (Random.list 20 (randomCoins newSize))
              else
                Cmd.none
            )

        NewCoins newCoins ->
            ( { model
                | flatCoins = newCoins
                , flippingCoins = []
                , fallingCoins = []
              }
            , Cmd.none
            )


randomCoins : Window.Size -> Random.Generator FlatCoin
randomCoins { width, height } =
    Random.map2 FlatCoin
        (Random.map2 vec2
            (Random.float (0.1 * toFloat width) (0.9 * toFloat width))
            (Random.float (0.1 * toFloat height) (0.9 * toFloat height))
        )
        (Random.float 5 (0.2 * toFloat (min width height)))


renderFlatCoin : FlatCoin -> Svg msg
renderFlatCoin coin =
    Svg.circle
        [ Svg.Attributes.r (toString coin.radius)
        , Svg.Attributes.transform
            (translateString coin.center)
        , Svg.Attributes.fill "#ec3"
        ]
        []


renderFlippingCoin : FlippingCoin -> Svg msg
renderFlippingCoin coin =
    let
        orientation =
            atan2 (getY coin.centerDirection) (getX coin.centerDirection)
    in
        Svg.circle
            [ Svg.Attributes.r (toString coin.radius)
            , Svg.Attributes.transform <|
                String.join " "
                    [ translateString coin.anchor
                    , rotateString orientation
                    , "scale(" ++ (toString <| cos coin.flipAngle) ++ ",1)"
                    , rotateString -orientation
                    , translateString
                        (coin.centerDirection
                            |> scale coin.radius
                        )
                    ]
            , Svg.Attributes.fill "#f73"
            ]
            []


renderFallingCoin : FallingCoin -> Svg msg
renderFallingCoin coin =
    let
        orientation =
            atan2 (getY coin.centerDirection) (getX coin.centerDirection)
    in
        Svg.circle
            [ Svg.Attributes.r (toString coin.radius)
            , Svg.Attributes.transform <|
                String.join " "
                    [ translateString coin.anchor
                    , rotateString orientation
                    , "scale(" ++ (toString <| cos coin.fallAngle) ++ ",1)"
                    , rotateString -orientation
                    , translateString
                        (coin.centerDirection
                            |> scale coin.radius
                        )
                    ]
            , Svg.Attributes.fill "#73f"
            ]
            []


view : Model -> Html Msg
view model =
    Svg.svg
        [ Svg.Attributes.width (toString model.windowSize.width)
        , Svg.Attributes.height (toString model.windowSize.height)
        , Svg.Attributes.viewBox
            ("0 0 "
                ++ (toString model.windowSize.width)
                ++ " "
                ++ (toString model.windowSize.height)
            )
        ]
        [ model.flatCoins
            |> List.map renderFlatCoin
            |> Svg.g []
        , model.flippingCoins
            |> List.map renderFlippingCoin
            |> Svg.g []
        , model.fallingCoins
            |> List.map renderFallingCoin
            |> Svg.g []
        ]


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( initialModel
            , Window.size
                |> Task.perform WindowResized
            )
        , subscriptions =
            \model ->
                Sub.batch
                    [ Mouse.moves MouseMove
                    , if model.fallingCoins == [] then
                        Sub.none
                      else
                        AnimationFrame.times Tick
                    , Window.resizes WindowResized
                    ]
        , update = update
        , view = view
        }
