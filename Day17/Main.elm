module Main exposing (..)

import Html exposing (Html)
import Collage
import Element
import Color exposing (Color)
import Color.Mixing
import Time exposing (Time)
import AnimationFrame
import Char
import Html.Attributes
import Html.Events
import Window
import Task


theShape : String -> Time -> Float -> Color -> Collage.Form
theShape text t size color =
    if size <= 5 || text == "" then
        Collage.group []
    else
        let
            scale =
                0.4

            push =
                0.55

            child index drop mixColor angle =
                let
                    c =
                        text
                            |> String.dropLeft index
                            |> String.uncons
                            |> Maybe.map (\( char, _ ) -> char)
                            |> Maybe.withDefault '.'

                    newColor =
                        if Char.isUpper c then
                            color
                                |> Color.Mixing.mix 0.5 mixColor
                                |> Color.Mixing.fadeIn 0.2
                        else if c == ' ' then
                            color
                                |> Color.Mixing.mix 0.5 Color.black
                                |> Color.Mixing.fadeIn 0.2
                        else
                            color
                                |> Color.Mixing.mix 0.2 mixColor
                                |> Color.Mixing.fadeOut 0.2
                in
                    theShape (String.dropLeft drop text)
                        (t * 1.2)
                        (size * scale)
                        newColor
                        |> Collage.move
                            ( size * push * cos (degrees <| angle + 10 * sin (t / 1000))
                            , size * push * sin (degrees <| angle + 10 * sin (t / 1000))
                            )
        in
            Collage.group
                [ Collage.circle size
                    |> Collage.filled color
                , Collage.circle size
                    |> Collage.outlined (Collage.solid Color.charcoal)
                , child 0 5 Color.red 30
                , child 1 10 Color.blue 150
                , child 2 15 Color.green 270
                ]


type alias Model =
    { now : Time
    , text : String
    , size : Maybe Window.Size
    }


type Msg
    = Tick Time
    | NewText String
    | WindowSize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { model | now = t }, Cmd.none )

        NewText text ->
            ( { model | text = text }, Cmd.none )

        WindowSize size ->
            ( { model | size = Just size }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.size of
        Nothing ->
            Html.text ""

        Just size ->
            Html.div [ Html.Attributes.style [ ( "position", "relative" ) ] ]
                [ Html.div [ Html.Attributes.style [ ( "position", "absolute" ) ] ]
                    [ [ theShape
                            model.text
                            model.now
                            300
                            Color.yellow
                      ]
                        |> Collage.collage size.width size.height
                        |> Element.toHtml
                    ]
                , Html.div
                    [ Html.Attributes.style
                        [ ( "position", "absolute" )
                        , ( "width", "100%" )
                        , ( "padding", "10px" )
                        , ( "margin", "0" )
                        , ( "box-sizing", "border-box" )
                        ]
                    ]
                    [ Html.input
                        [ Html.Attributes.style
                            [ ( "width", "100%" )
                            , ( "font-size", "24px" )
                            , ( "box-sizing", "border-box" )
                            ]
                        , Html.Attributes.defaultValue model.text
                        , Html.Events.onInput NewText
                        ]
                        []
                    ]
                ]


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { now = 0
              , text = "A delightful language for reliable webapps."
              , size = Nothing
              }
            , Window.size |> Task.perform WindowSize
            )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ AnimationFrame.times Tick
                    , Window.resizes WindowSize
                    ]
        , update = update
        , view = view
        }
