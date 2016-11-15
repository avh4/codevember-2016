module Main exposing (..)

import Html exposing (Html)
import Html.App
import Html.Events
import Html.Attributes
import Color exposing (Color)
import Time exposing (Time)
import Collage
import Element
import AnimationFrame
import Day14.TileGrid as TileGrid
import Random
import Random.Extra


type alias Model =
    { tileGrid : TileGrid.Model
    , now : Time
    }


initialModel : Model
initialModel =
    { tileGrid = TileGrid.init [ Color.red, Color.green, Color.blue, Color.orange, Color.brown ]
    , now = 0
    }


type Msg
    = Tick Time
    | NewColors
    | ChangeColors (List Color)


randomColor : Random.Generator Color
randomColor =
    Random.Extra.choices
        [ Random.Extra.constant Color.red
        , Random.Extra.constant Color.blue
        , Random.Extra.constant Color.green
        , Random.Extra.constant Color.yellow
        , Random.Extra.constant Color.orange
        , Random.Extra.constant Color.purple
        , Random.Extra.constant Color.charcoal
        , Random.Extra.constant Color.brown
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick now ->
            ( { model
                | now = now
                , tileGrid = TileGrid.update now model.tileGrid
              }
            , Cmd.none
            )

        NewColors ->
            ( model
            , Random.generate ChangeColors (Random.list 3 randomColor)
            )

        ChangeColors newColors ->
            ( { model
                | tileGrid =
                    TileGrid.changeColors model.now
                        newColors
                        model.tileGrid
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick NewColors, Html.Attributes.style [ ( "font-size", "300%" ) ] ] [ Html.text "Flip!" ]
        , [ TileGrid.view model.now model.tileGrid ]
            |> Collage.collage 750 400
            |> Element.toHtml
        ]


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
        , subscriptions = \_ -> AnimationFrame.times Tick
        , update = update
        , view = view
        }
