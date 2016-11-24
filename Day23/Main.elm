module Main exposing (..)

import Color exposing (Color)
import Svg exposing (Svg)
import Svg.Attributes
import Html exposing (Html)
import Html.Attributes
import Color.Convert
import Time exposing (Time)
import AnimationFrame
import Random
import Random.Color
import Color.Mixing
import Mouse
import Window
import Task


type alias Blade =
    { color : Color
    , size : Float
    , angle : Float
    }


type alias Model =
    { blades : List Blade
    , done : Bool
    , mousePosition : { x : Int, y : Int }
    , windowSize : { width : Int, height : Int }
    }


initialModel : Model
initialModel =
    { blades =
        List.range 0 50
            |> List.map
                (\i ->
                    { color = Color.darkGreen
                    , size = 5
                    , angle = degrees <| (toFloat i / 50 * 40) - 20
                    }
                )
    , done = False
    , mousePosition = { x = 0, y = 0 }
    , windowSize = { width = 1, height = 1 }
    }


grow : Float -> Color -> Int -> List Blade -> List Blade
grow xPercent growthColor whichBlade blades =
    List.indexedMap
        (\i blade ->
            if i == whichBlade then
                { blade
                    | size =
                        blade.size + 0.7 * ((1 - abs ((toFloat i / 50) - xPercent)) ^ 8)
                    , color =
                        blade.color
                            |> Color.Mixing.mix 0.01 growthColor
                }
            else
                blade
        )
        blades


renderBlade : Int -> Blade -> Svg msg
renderBlade i blade =
    Svg.polygon
        [ Svg.Attributes.points <| "-1.2,0 0," ++ toString (-blade.size) ++ " 1.2,0"
        , Svg.Attributes.fill (Color.Convert.colorToHex blade.color)
        , Svg.Attributes.transform <|
            String.join " "
                [ "translate(" ++ toString (i - 50 // 2) ++ ",0)"
                , "rotate(" ++ toString (blade.angle / pi * 180) ++ ")"
                ]
        ]
        []


view : Model -> Html msg
view model =
    Svg.svg
        [ Svg.Attributes.viewBox "-50 -50 100 100"
        , Html.Attributes.style
            [ ( "max-height", "100vh" ) ]
        ]
        [ Svg.rect
            [ Svg.Attributes.x "-150"
            , Svg.Attributes.y "-150"
            , Svg.Attributes.width "300"
            , Svg.Attributes.height "300"
            , Svg.Attributes.fill "#222"
            ]
            []
        , List.indexedMap renderBlade model.blades
            |> Svg.g [ Svg.Attributes.transform "translate(0,35)" ]
        ]


type Msg
    = Tick Time
    | GrowBlades (List ( Int, Color ))
    | MouseMoved Mouse.Position
    | WindowSize Window.Size


checkDone : Model -> Model
checkDone model =
    if List.any (\blade -> blade.size > 80) model.blades then
        { model | done = True }
    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            ( model
            , Random.generate
                GrowBlades
                (Random.list 10
                    (Random.map2 (,)
                        (Random.int 0 (List.length model.blades))
                        (Random.Color.rgb)
                    )
                )
            )

        GrowBlades growths ->
            let
                growBlade ( i, color ) blades =
                    grow
                        (toFloat model.mousePosition.x
                            / toFloat model.windowSize.width
                        )
                        color
                        i
                        blades
            in
                ( { model
                    | blades =
                        List.foldl growBlade model.blades growths
                  }
                    |> checkDone
                , Cmd.none
                )

        MouseMoved pos ->
            ( { model | mousePosition = pos }
            , Cmd.none
            )

        WindowSize newSize ->
            ( { model | windowSize = newSize }
            , Cmd.none
            )


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( initialModel
            , Window.size |> Task.perform WindowSize
            )
        , subscriptions =
            \model ->
                Sub.batch
                    [ if model.done then
                        Sub.none
                      else
                        AnimationFrame.times Tick
                    , Mouse.moves MouseMoved
                    , Window.resizes WindowSize
                    ]
        , update = update
        , view = view
        }
