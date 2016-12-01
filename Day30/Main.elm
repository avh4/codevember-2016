module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Random
import Random.Extra
import Time exposing (Time)


boardWidth =
    15


boardHeight =
    10


tileWidth =
    101


tileDepth =
    83


tileHeight =
    41


type Goal
    = Collecting ResourceType ( Int, Int )
    | ReturningHome ResourceType
    | Done


type alias Character =
    { location : ( Int, Int )
    , image : String
    , home : ( Int, Int )
    , goal : Goal
    }


type ResourceType
    = OrangeGem
    | BlueGem
    | Rock


type alias Resource =
    { location : ( Int, Int )
    , resourceType : ResourceType
    }


resourceImg : ResourceType -> String
resourceImg resourceType =
    case resourceType of
        OrangeGem ->
            "PlanetCute PNG/Gem Orange.png"

        BlueGem ->
            "PlanetCute PNG/Gem Blue.png"

        Rock ->
            "PlanetCute PNG/Rock.png"


type alias Model =
    { characters : List Character
    , resources : List Resource
    , seed : Random.Seed
    }


initialModel : Model
initialModel =
    { characters = []
    , resources = []
    , seed = Random.initialSeed 0
    }


type Msg
    = Start ( List ( Int, Int ), List Resource, Int )
    | Tick Time


characterImages =
    [ "PlanetCute PNG/Character Horn Girl.png"
    , "PlanetCute PNG/Character Cat Girl.png"
    , "PlanetCute PNG/Character Pink Girl.png"
    , "PlanetCute PNG/Character Princess Girl.png"
    , "PlanetCute PNG/Character Boy.png"
    ]


makeChar : ( Int, Int ) -> String -> Character
makeChar location image =
    { location = location
    , image = image
    , home = location
    , goal = Done
    }


moveToward : Bool -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
moveToward bool ( toX, toY ) ( fromX, fromY ) =
    if bool then
        if toX < fromX then
            ( fromX - 1, fromY )
        else
            ( fromX + 1, fromY )
    else if toY < fromY then
        ( fromX, fromY - 1 )
    else
        ( fromX, fromY + 1 )


updateCharacter : Random.Seed -> List Resource -> Character -> ( Character, List Resource, Random.Seed )
updateCharacter seed resources char =
    case char.goal of
        Done ->
            let
                ( resource, seed1 ) =
                    Random.step (Random.Extra.sample resources) seed
            in
                case resource of
                    Nothing ->
                        ( char, resources, seed )

                    Just r ->
                        ( { char | goal = Collecting r.resourceType r.location }, resources, seed1 )

        Collecting rType rLoc ->
            let
                resourceDoesntExist =
                    resources
                        |> List.filter (\r -> r == Resource rLoc rType)
                        |> List.length
                        |> (\count -> count == 0)
            in
                if resourceDoesntExist then
                    ( { char | goal = Done }
                    , resources
                    , seed
                    )
                else if rLoc == char.location then
                    ( { char | goal = ReturningHome rType }
                    , resources |> List.filter (\r -> r /= Resource rLoc rType)
                    , seed
                    )
                else
                    let
                        ( dir, seed1 ) =
                            Random.step Random.bool seed
                    in
                        ( { char
                            | location =
                                char.location |> moveToward dir rLoc
                          }
                        , resources
                        , seed1
                        )

        ReturningHome rType ->
            if char.location == char.home then
                ( { char | goal = Done }
                , Resource char.location rType :: resources
                , seed
                )
            else
                let
                    ( dir, seed1 ) =
                        Random.step Random.bool seed
                in
                    ( { char
                        | location =
                            char.location |> moveToward dir char.home
                      }
                    , resources
                    , seed1
                    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ( characterLocations, resources, seed ) ->
            ( { characters = List.map2 makeChar characterLocations characterImages
              , resources = resources
              , seed = Random.initialSeed seed
              }
            , Cmd.none
            )

        Tick t ->
            let
                step c ( chars, res, seed ) =
                    let
                        ( newChar, newRes, newSeed ) =
                            updateCharacter seed model.resources c
                    in
                        ( newChar :: chars, newRes, newSeed )

                ( newChar, newRes, finalSeed ) =
                    List.foldr
                        step
                        ( [], model.resources, model.seed )
                        model.characters
            in
                ( { model
                    | characters = newChar
                    , resources = newRes
                    , seed = finalSeed
                  }
                , Cmd.none
                )


renderTile z ( x, y ) imageSrc =
    Html.img
        [ Html.Attributes.src imageSrc
        , Html.Attributes.width tileWidth
        , Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "left", (toString <| x * tileWidth) ++ "px" )
            , ( "top", (toString <| y * tileDepth - z * tileHeight - 50) ++ "px" )
            ]
        ]
        []


renderRow y =
    List.range 0 boardWidth
        |> List.map (\x -> renderTile 0 ( x, y ) "PlanetCute PNG/Grass Block.png")
        |> Html.div []


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style
            [ ( "position", "relative" )
            , ( "width", "100vw" )
            , ( "height", "100vh" )
            ]
        ]
        [ List.range 0 boardHeight
            |> List.map (\y -> renderRow y)
            |> Html.div []
        , model.characters
            |> List.map (\c -> renderTile 1 c.home "PlanetCute PNG/Selector.png")
            |> Html.div []
        , model.resources
            |> List.sortBy (\c -> Tuple.second c.location)
            |> List.map (\c -> renderTile 1 c.location (resourceImg c.resourceType))
            |> Html.div []
        , model.characters
            |> List.map (\c -> renderTile 1 c.location c.image)
            |> Html.div []
        ]


randomLocation : Random.Generator ( Int, Int )
randomLocation =
    Random.map2 (,)
        (Random.int 0 boardWidth)
        (Random.int 0 boardHeight)


randomResource : Random.Generator Resource
randomResource =
    Random.map2 Resource
        randomLocation
        (Random.Extra.frequency
            [ ( 1, Random.Extra.constant OrangeGem )
            , ( 2, Random.Extra.constant BlueGem )
            , ( 5, Random.Extra.constant Rock )
            ]
        )


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( initialModel
            , Random.generate Start
                (Random.map3 (,,)
                    (Random.list 5 randomLocation)
                    (Random.list 40 randomResource)
                    (Random.int -10000000 10000000)
                )
            )
        , subscriptions =
            \_ ->
                Time.every 40 Tick
        , update = update
        , view = view
        }
