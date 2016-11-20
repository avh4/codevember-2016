module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Time exposing (Time)


type alias Model =
    { current : String
    , nextLines : List String
    , author : String
    , finalFade : Maybe Time
    , done : Bool
    }


initialModel : Model
initialModel =
    { current = ""
    , nextLines =
        [ "At last to be identified!"
        , "At last, the lamps upon thy side,"
        , "The rest of life to see!"
        , "Past midnight, past the morning star!"
        , "Past sunrise! Ah! what leagues there are"
        , "Between our feet and day!"
        ]
    , author = "Emily Dickinson"
    , finalFade = Nothing
    , done = False
    }


type Msg
    = Tick Time


nextStep : String -> String -> String
nextStep current target =
    if current == "" then
        String.left 1 target
    else if String.left 1 current /= String.left 1 target then
        String.dropRight 1 current
    else
        (String.left 1 current
            ++ nextStep
                (String.dropLeft 1 current)
                (String.dropLeft 1 target)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            case model.nextLines of
                [] ->
                    if model.current == "" then
                        case model.finalFade of
                            Nothing ->
                                ( { model | finalFade = Just 0 }, Cmd.none )

                            Just fade ->
                                if fade >= 1.0 then
                                    ( { model | done = True }, Cmd.none )
                                else
                                    ( { model | finalFade = Just <| fade + 0.01 }
                                    , Cmd.none
                                    )
                    else
                        ( { model | current = String.dropRight 1 model.current }
                        , Cmd.none
                        )

                next :: rest ->
                    ( { model
                        | current = nextStep model.current next
                        , nextLines =
                            if model.current == next then
                                rest
                            else
                                model.nextLines
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style
            [ ( "background-color", "black" )
            , ( "width", "100vw" )
            , ( "height", "100vh" )
            , ( "font-family", "serif" )
            ]
        ]
        [ Html.div
            [ Html.Attributes.style
                [ ( "position", "relative" )
                , ( "top", "50%" )
                , ( "transform", "translateY(-50%)" )
                , ( "text-align", "center" )
                , ( "color", "#ddc" )
                , ( "font-size", "9vw" )
                ]
            ]
            [ Html.text model.current ]
        , Html.div
            [ Html.Attributes.style
                [ ( "position", "relative" )
                , ( "top", "70%" )
                , ( "transform", "translateY(-50%)" )
                , ( "text-align", "center" )
                , ( "padding-left", "20%" )
                , ( "color", "#665" )
                , ( "font-size", "6vw" )
                , ( "opacity"
                  , model.finalFade
                        |> Maybe.withDefault 0
                        |> toString
                  )
                ]
            ]
            [ Html.text model.author ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , subscriptions =
            \model ->
                if model.done then
                    Sub.none
                else
                    Time.every 100 Tick
        , update = update
        , view = view
        }
