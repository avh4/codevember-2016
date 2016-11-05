module Main exposing (..)

import Graphics.Render exposing (..)
import Color exposing (Color)
import Html exposing (Html)
import Html.App
import Random
import Random.Extra
import Http
import Task
import Json.Decode
import Color.Convert


triangle : Float -> Color -> Form msg
triangle size color =
    polygon [ ( 0, 0 ), ( abs size, 0 ), ( abs size / 2, size * sqrt 3 / 2 ) ]
        |> solidFill color


triangleRow : List Color -> List Triangle -> Form msg
triangleRow pallete triangles =
    let
        chooseColor i =
            if List.length pallete == 0 then
                Color.black
            else
                pallete
                    |> List.drop (i % List.length pallete)
                    |> List.head
                    |> Maybe.withDefault Color.black

        step i { size, color } =
            triangle (toFloat size * 50) (chooseColor color)
                |> move (50 * toFloat i) 0
    in
        triangles
            |> List.indexedMap step
            |> group


type alias Model =
    { pallete : List Color
    , triangleData : List (List Triangle)
    }


view : Model -> Html msg
view model =
    group
        [ rectangle 750 500 |> solidFill (Color.rgb 255 255 240)
        , model.triangleData
            |> List.indexedMap (\i sizes -> triangleRow model.pallete sizes |> move 0 (toFloat i * 60))
            |> group
            |> move -180 -120
        ]
        |> svg 750 500


modelGenerator : Random.Generator (List (List Triangle))
modelGenerator =
    let
        randomSize =
            Random.int -2 2

        randomRow =
            Random.list 6 (Random.map2 Triangle randomSize (Random.int 0 50))
    in
        Random.list 4 randomRow


type alias Triangle =
    { size : Int, color : Int }


type Msg
    = NewTriangleData (List (List Triangle))
    | NewPallete (List Color)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        NewPallete colors ->
            ( { model | pallete = colors }, Cmd.none )

        NewTriangleData newData ->
            ( { model | triangleData = newData }, Cmd.none )


palleteDecoder : Json.Decode.Decoder (List Color)
palleteDecoder =
    Json.Decode.tuple1 identity
        (Json.Decode.at [ "colors" ]
            (Json.Decode.map (List.filterMap identity) <|
                Json.Decode.list (Json.Decode.map Color.Convert.hexToColor Json.Decode.string)
            )
        )


main : Program Never
main =
    Html.App.program
        { init =
            ( { pallete = []
              , triangleData = []
              }
            , Cmd.batch
                [ Random.generate NewTriangleData modelGenerator
                , Http.get palleteDecoder
                    "https://crossorigin.me/https://www.colourlovers.com/api/palettes/random?format=json"
                    |> Task.perform (always <| NewPallete []) NewPallete
                ]
            )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
