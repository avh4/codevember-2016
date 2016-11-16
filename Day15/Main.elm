port module Main exposing (..)

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
import Json.Decode


port requestImageColors : String -> Cmd msg


port imageColorResults : (List ( Int, Int, Int ) -> msg) -> Sub msg


type alias Model =
    { tileGrid : TileGrid.Model
    , now : Time
    , colorImage : String
    , silhouetteImage : String
    }


imageSet : List ( String, String )
imageSet =
    [ ( "rhino-color.jpg", "rhino-silhoutte.png" )
    , ( "crow-color.jpg", "crow-silhouette.png" )
    , ( "fish-color.jpg", "fish-silhouette.png" )
    , ( "dragon-color.jpg", "dragon-silhouette.png" )
    , ( "quill-color.png", "quill-silhouette.png" )
    , ( "reindeer-color.jpg", "reindeer-silhouette.png" )
    , ( "saxophone-color.jpg", "saxophone-silhouette.png" )
    , ( "stork-color.jpg", "stork-silhouette.png" )
    , ( "tea-color.jpg", "tea-silhouette.png" )
    , ( "swan-color.jpg", "swan-silhouette.png" )
    , ( "unicorn-color.jpg", "unicorn-silhouette.png" )
    , ( "beard-color.jpg", "beard-silhouette.png" )
    ]


initialModel : Model
initialModel =
    { tileGrid = TileGrid.init [ Color.grey ]
    , now = 0
    , colorImage = "rhino-color.jpg"
    , silhouetteImage = "rhino-silhoutte.png"
    }


type Msg
    = Tick Time
    | NewColors
    | ChangeColors (List Color)
    | ColorImageLoaded
    | AnalyzedColors (List ( Int, Int, Int ))
    | NewImage Time
    | ChangeImage (Maybe ( String, String ))


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

        NewImage now ->
            ( model
            , Random.generate ChangeImage (Random.Extra.sample imageSet)
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

        ChangeImage Nothing ->
            ( model, Cmd.none )

        ChangeImage (Just ( colorImage, silhouetteImage )) ->
            ( { model | colorImage = colorImage, silhouetteImage = silhouetteImage }
            , Cmd.none
            )

        ColorImageLoaded ->
            ( model
            , requestImageColors "mainImage"
            )

        AnalyzedColors rgbTuples ->
            let
                newColors =
                    rgbTuples
                        |> List.map (\( r, g, b ) -> Color.rgb r g b)
            in
                update (ChangeColors newColors) model


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style
            [ ( "position", "relative" )
            , ( "width", "750px" )
            , ( "height", "500px" )
            ]
        ]
        [ Html.img
            [ Html.Attributes.style [ ( "display", "none" ) ]
            , Html.Attributes.id "mainImage"
            , Html.Attributes.src model.colorImage
            , Html.Events.on "load" (Json.Decode.succeed ColorImageLoaded)
            ]
            []
        , Html.div [ Html.Attributes.style [ ( "position", "absolute" ) ] ]
            [ Element.layers
                [ [ TileGrid.view model.now model.tileGrid
                        |> Collage.rotate (degrees 45)
                  ]
                    |> Collage.collage 750 500
                ]
                |> Element.toHtml
            ]
        , Html.div
            [ Html.Attributes.style
                [ ( "position", "absolute" )
                , ( "width", "280px" )
                , ( "height", "250px" )
                , ( "top", "50%" )
                , ( "left", "50%" )
                , ( "transform", "translateX(-50%) translateY(-50%)" )
                ]
            ]
            [ Html.img
                [ Html.Attributes.style
                    [ ( "margin", "auto" )
                    , ( "position", "absolute" )
                    , ( "top", "50%" )
                    , ( "left", "50%" )
                    , ( "object-fit", "contain" )
                    , ( "transform", "translateX(-50%) translateY(-50%)" )
                    , ( "max-width", "100%" )
                    , ( "max-height", "100%" )
                    , ( "-webkit-filter", "drop-shadow(0 0 50px rgba(255,255,255,1.0))" )
                    ]
                , Html.Attributes.src model.silhouetteImage
                , Html.Attributes.width 300
                ]
                []
            ]
        ]


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ AnimationFrame.times Tick
                    , imageColorResults AnalyzedColors
                    , Time.every 5000 NewImage
                    ]
        , update = update
        , view = view
        }
