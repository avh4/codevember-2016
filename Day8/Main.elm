module Main exposing (..)

import Html.App
import Html exposing (Html)
import Html.Attributes
import Graphics.Render as Render
import Color exposing (Color)
import Time exposing (Time)
import Random
import Random.Extra
import Random.Color


yellowTime : Int
yellowTime =
    50


greenTime : Int
greenTime =
    200


roadSize : Int
roadSize =
    300


type TrafficLights
    = NorthGreen Int
    | NorthYellow Int
    | EastGreen Int
    | EastYellow Int
    | EastGreenLeft Int


type Direction
    = Forward
    | Backward


type WhichRoad
    = Top Direction
    | Left Direction
    | Bottom Direction
    | Right Direction


type Location
    = Arriving Float
    | Waiting
    | Crossing Float
    | Leaving Float


type alias Person =
    { color : Color
    , location : Location
    , road : WhichRoad
    , speed : Float
    }


type alias Model =
    { lights : TrafficLights
    , people : List Person
    }


initialModel : Model
initialModel =
    { lights = NorthGreen greenTime
    , people = []
    }


type Msg
    = Tick Time
    | NewPeople (List Person)


updateLights : TrafficLights -> TrafficLights
updateLights current =
    let
        advance current t next nextTime =
            if t <= 0 then
                next nextTime
            else
                current (t - 1)
    in
        case current of
            NorthGreen t ->
                advance NorthGreen t NorthYellow yellowTime

            NorthYellow t ->
                advance NorthYellow t EastGreen greenTime

            EastGreen t ->
                advance EastGreen t EastYellow yellowTime

            EastYellow t ->
                advance EastYellow t NorthGreen greenTime

            EastGreenLeft t ->
                advance EastGreenLeft t EastGreen greenTime


updatePerson : TrafficLights -> Person -> Person
updatePerson lights ({ color, location, road, speed } as person) =
    case location of
        Arriving t ->
            if t <= 0 then
                { person | location = Waiting }
            else
                { person | location = Arriving (t - speed) }

        Waiting ->
            case ( road, lights ) of
                ( Left _, NorthGreen _ ) ->
                    { person | location = Crossing (toFloat roadSize) }

                ( Right _, NorthGreen _ ) ->
                    { person | location = Crossing (toFloat roadSize) }

                ( Top _, EastGreen _ ) ->
                    { person | location = Crossing (toFloat roadSize) }

                ( Bottom _, EastGreen _ ) ->
                    { person | location = Crossing (toFloat roadSize) }

                _ ->
                    { person | location = Waiting }

        Crossing t ->
            if t <= 0 then
                { person | location = Leaving 0 }
            else
                { person | location = Crossing (t - speed) }

        Leaving t ->
            if t >= 350 then
                { person | location = Arriving t }
            else
                { person | location = Leaving (t + speed) }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick _ ->
            { model
                | lights = updateLights model.lights
                , people = model.people |> List.map (updatePerson model.lights)
            }

        NewPeople newPeople ->
            { model | people = newPeople }


view : Model -> Html msg
view model =
    Html.div [ Html.Attributes.style [ ( "position", "relative" ) ] ]
        [ Html.img
            [ Html.Attributes.src "/Day8/Street_intersection_diagram.svg"
            , Html.Attributes.width 530
            , Html.Attributes.height 380
            , Html.Attributes.style [ ( "position", "absolute" ) ]
            ]
            []
        , Html.div [ Html.Attributes.style [ ( "position", "absolute" ) ] ]
            [ Render.group
                [ renderLights model.lights
                , model.people
                    |> List.map renderPerson
                    |> Render.group
                ]
                |> Render.svg 530 380
            ]
        ]


renderPerson : Person -> Render.Form msg
renderPerson { color, location, road } =
    let
        ( x, y, tx, ty ) =
            case road of
                Left Forward ->
                    ( -90, 105, 0, 1 )

                Right Forward ->
                    ( 90, 105, 0, 1 )

                Top Forward ->
                    ( 90, -105, 1, 0 )

                Bottom Forward ->
                    ( 90, 105, 1, 0 )

                Left Backward ->
                    ( -90, -105, 0, -1 )

                Right Backward ->
                    ( 90, -105, 0, -1 )

                Top Backward ->
                    ( -90, -105, -1, 0 )

                Bottom Backward ->
                    ( -90, 105, -1, 0 )
    in
        case location of
            Waiting ->
                Render.ellipse 3 3
                    |> Render.solidFill color
                    |> Render.move x y

            Arriving t ->
                Render.ellipse 3 3
                    |> Render.solidFill color
                    |> Render.move (x + tx * t) (y + ty * t)

            Crossing t ->
                Render.ellipse 3 3
                    |> Render.solidFill color
                    |> Render.move
                        (x + tx * (t - toFloat roadSize))
                        (y + ty * (t - toFloat roadSize))

            Leaving t ->
                Render.ellipse 3 3
                    |> Render.solidFill color
                    |> Render.move
                        (x + tx * (-t - toFloat roadSize))
                        (y + ty * (-t - toFloat roadSize))


renderLights : TrafficLights -> Render.Form msg
renderLights lights =
    case lights of
        NorthGreen _ ->
            Render.ellipse 9 9
                |> Render.solidFillWithBorder (Color.green) 2 (Color.black)
                |> Render.move -76 -114

        NorthYellow _ ->
            Render.ellipse 9 9
                |> Render.solidFillWithBorder (Color.yellow) 2 (Color.black)
                |> Render.move -76 -114

        EastGreen _ ->
            Render.ellipse 9 9
                |> Render.solidFillWithBorder (Color.green) 2 (Color.black)
                |> Render.move -115 94

        EastGreenLeft _ ->
            Render.ellipse 9 9
                |> Render.solidFillWithBorder (Color.green) 2 (Color.yellow)
                |> Render.move -115 94

        EastYellow _ ->
            Render.ellipse 9 9
                |> Render.solidFillWithBorder (Color.yellow) 2 (Color.black)
                |> Render.move -115 94


randomPersonColor : Random.Generator Color
randomPersonColor =
    Random.Color.rgb


randomLocation : Random.Generator Location
randomLocation =
    Random.Extra.choices
        [ Random.map Arriving (Random.float 0 500)
        , Random.Extra.constant Waiting
        ]


randomDirection : Random.Generator Direction
randomDirection =
    Random.Extra.choices
        [ Random.Extra.constant Forward
        , Random.Extra.constant Backward
        ]


randomRoad : Random.Generator WhichRoad
randomRoad =
    Random.Extra.choices
        [ Random.map Top randomDirection
        , Random.map Left randomDirection
        , Random.map Bottom randomDirection
        , Random.map Right randomDirection
        ]


randomPerson : Random.Generator Person
randomPerson =
    Random.map4 Person
        randomPersonColor
        randomLocation
        randomRoad
        (Random.float 1 4)


main : Program Never
main =
    Html.App.program
        { init =
            ( initialModel
            , Random.generate NewPeople
                (Random.list 200 randomPerson)
            )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Time.every 20 Tick
        , view = view
        }
