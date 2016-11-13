module Main exposing (..)

import Html exposing (Html)
import Html.App
import Random
import Random.Color
import Random.Extra
import Collage
import Color exposing (Color)
import Element
import Time exposing (Time)
import Window
import Task


smallShape : Collage.Shape -> Float -> Float -> Float -> Color -> Collage.Form
smallShape shape x y s color =
    shape
        |> Collage.filled color
        |> Collage.scale s
        |> Collage.move ( x, y )


withBackground : Color -> List Collage.Form -> Collage.Form
withBackground background content =
    Collage.group
        [ Collage.rect 750 500 |> Collage.filled background
        , Collage.group content
        ]


shapesArt :
    Collage.Shape
    -> Int
    -> List Color
    -> ( Random.Generator Float, Random.Generator Float, Random.Generator Float )
    -> Random.Generator Collage.Form
shapesArt shape n pallette ( xgen, ygen, sgen ) =
    Random.map
        (List.head pallette |> Maybe.withDefault Color.black |> withBackground)
        (Random.list n
            (Random.map4 (smallShape shape)
                xgen
                ygen
                sgen
                (Random.Extra.sample (List.drop 1 pallette)
                    |> Random.map (Maybe.withDefault Color.yellow)
                )
            )
        )


monochromePalette : Float -> Random.Generator (List Color)
monochromePalette hue =
    Random.list 3
        (Random.map2 (Color.hsl hue)
            (Random.float 0.3 7)
            (Random.float 0 1)
        )


randomPalette : Random.Generator (List Color)
randomPalette =
    Random.Extra.frequency
        [ ( 5, Random.list 5 (Random.Color.rgb) )
        , ( 1, Random.list 3 (Random.Color.greyscale) )
        , ( 1, Random.Extra.flatMap monochromePalette (Random.float 0 360) )
        ]


randomShape : Float -> Random.Generator Collage.Shape
randomShape size =
    Random.Extra.choices
        [ Random.Extra.constant <| Collage.square size
        , Random.Extra.constant <| Collage.circle size
        , Random.Extra.constant <| Collage.ngon 3 size
        ]


randomArt : Random.Generator Collage.Form
randomArt =
    Random.Extra.choices
        [ -- Random.map Solid
          --     (Random.Color.rgb)
          -- ,
          randomSmallShapesArt
        , randomLargeShapesArt
        ]


randomLargeShapesArt : Random.Generator Collage.Form
randomLargeShapesArt =
    Random.Extra.flatMap4 shapesArt
        (randomShape 200)
        (Random.int 1 3)
        randomPalette
        (Random.Extra.frequency
            [ ( 2
              , Random.Extra.constant
                    ( (Random.float -175 175)
                    , (Random.float -150 150)
                    , (Random.float 0.1 3)
                    )
              )
            ]
        )


randomSmallShapesArt : Random.Generator Collage.Form
randomSmallShapesArt =
    Random.Extra.flatMap4 shapesArt
        (randomShape 20)
        (Random.int 1 200)
        randomPalette
        (Random.Extra.frequency
            [ ( 2
              , Random.Extra.constant
                    ( (Random.float -375 375)
                    , (Random.float -250 250)
                    , (Random.float 0.1 3)
                    )
              )
            , ( 1
              , Random.Extra.constant
                    ( (Random.float -375 375)
                    , (Random.float 0 0)
                    , (Random.float 0.1 3)
                    )
              )
            , ( 3
              , let
                    gen p =
                        ( Random.map (\i -> toFloat i * 375 / toFloat p) (Random.int -p p)
                        , Random.map (\i -> toFloat i * 250 / toFloat p) (Random.int -p p)
                        , Random.map (\i -> toFloat i) (Random.int 1 3)
                        )
                in
                    Random.map gen (Random.int 1 5)
              )
            ]
        )


type alias Model =
    { art : Collage.Form
    , size : Maybe Window.Size
    }


view : Model -> Html msg
view model =
    case model.size of
        Nothing ->
            Html.text ""

        Just { width, height } ->
            Collage.collage width
                height
                [ model.art ]
                |> Element.toHtml


type Msg
    = NewArt Collage.Form
    | Tick Time
    | Resize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewArt art ->
            ( { model | art = art }, Cmd.none )

        Tick _ ->
            ( model, Random.generate NewArt randomArt )

        Resize newSize ->
            ( { model | size = Just newSize }, Cmd.none )


main : Program Never
main =
    Html.App.program
        { init =
            ( { art = Collage.group []
              , size = Nothing
              }
            , Cmd.batch
                [ Random.generate NewArt randomArt
                , Window.size |> Task.perform Resize Resize
                ]
            )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Time.every 1000 Tick
                    , Window.resizes Resize
                    ]
        , update = update
        , view = view
        }
