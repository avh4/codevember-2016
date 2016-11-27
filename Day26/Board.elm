module Board
    exposing
        ( Board
        , Tile(..)
        , Piece(..)
        , Player(..)
        , Position
        , new
        , makeMove
        , view
        )

import Color exposing (Color)
import Dict exposing (Dict)
import Set exposing (Set)
import Random
import Random.Extra


type Player
    = Player1
    | Player2


type Piece
    = BasicPiece
    | TerraformingPiece TerraformingShape


type Tile
    = Tile (List Color)


type Board
    = Board
        { columns : Int
        , rows : Int
        , tiles : Dict Position Tile
        , pieces : Dict Position ( Player, Piece )
        }


type alias TerraformingShape =
    Set ( Int, Int )


type alias Position =
    ( Int, Int )


possibleShapes : List TerraformingShape
possibleShapes =
    [ Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
    , Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( -1, 0 ) ]
    , Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, 1 ) ]
    , Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( -1, 0 ), ( 1, 1 ) ]
    , Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( -1, 0 ), ( 1, -1 ) ]
    ]


new : Random.Seed -> Board
new seed =
    Board
        { columns = 8
        , rows = 8
        , tiles = Dict.empty
        , pieces =
            List.foldl
                (\i ( pieces, seed1 ) ->
                    let
                        ( p1Shape, seed2 ) =
                            Random.step (Random.Extra.sample possibleShapes) seed1

                        ( p2Shape, seed3 ) =
                            Random.step (Random.Extra.sample possibleShapes) seed2
                    in
                        ( pieces
                            |> Dict.insert ( 2 - i % 2, i )
                                ( Player1
                                , p1Shape
                                    |> Maybe.map TerraformingPiece
                                    |> Maybe.withDefault BasicPiece
                                )
                            |> Dict.insert ( 8 - i % 2, i )
                                ( Player2
                                , p2Shape
                                    |> Maybe.map TerraformingPiece
                                    |> Maybe.withDefault BasicPiece
                                )
                        , seed3
                        )
                )
                ( Dict.empty, seed )
                (List.range 1 8)
                |> Tuple.first
        }
        |> mapTiles
            (\( row, col ) _ ->
                if (row + col) % 2 == 0 then
                    Tile [ Color.red ]
                else
                    Tile [ Color.yellow ]
            )


mapTiles : (Position -> Tile -> Tile) -> Board -> Board
mapTiles fn (Board board) =
    let
        mapTile row col tiles =
            let
                pos =
                    ( col, row )

                tile =
                    Dict.get pos tiles
                        |> Maybe.withDefault (Tile [])
            in
                tiles
                    |> Dict.insert pos
                        (fn pos tile)

        mapRow row tiles =
            List.foldl (mapTile row) tiles (List.range 1 board.columns)

        mapAll tiles =
            List.foldl mapRow tiles (List.range 1 board.rows)
    in
        Board { board | tiles = mapAll board.tiles }


pullTopColor : Tile -> ( Maybe Color, Tile )
pullTopColor (Tile colors) =
    case colors of
        [] ->
            ( Nothing, (Tile colors) )

        topColor :: rest ->
            ( Just topColor, Tile rest )


pushTopColor : Color -> Tile -> Tile
pushTopColor newColor (Tile colors) =
    Tile (newColor :: colors)


add : Position -> Position -> Position
add ( x, y ) ( x1, y1 ) =
    ( x + x1, y + y1 )


terraformBoard : TerraformingShape -> Position -> Position -> Dict Position Tile -> Dict Position Tile
terraformBoard relativeFroms fromCenter toCenter tiles =
    let
        calcMovement : ( Int, Int ) -> ( Position, Tile, Maybe Color, Position )
        calcMovement rel =
            let
                fromPos =
                    add rel fromCenter

                ( movingColor, reminaingTile ) =
                    tiles
                        |> Dict.get fromPos
                        |> Maybe.withDefault (Tile [])
                        |> pullTopColor
            in
                ( add rel fromCenter
                , reminaingTile
                , movingColor
                , add rel toCenter
                )

        movements : List ( Position, Tile, Maybe Color, Position )
        movements =
            relativeFroms
                |> Set.toList
                |> List.map calcMovement

        withTopsRemoved : Dict Position Tile
        withTopsRemoved =
            List.foldl
                (\( pos, tile, _, _ ) tiles ->
                    Dict.insert pos tile tiles
                )
                tiles
                movements

        withColorsPushed : Dict Position Tile
        withColorsPushed =
            List.foldl
                (\( _, _, color, pos ) ->
                    Dict.update pos
                        (\tile ->
                            case color of
                                Nothing ->
                                    tile

                                Just color ->
                                    Maybe.map (pushTopColor color) tile
                        )
                )
                withTopsRemoved
                movements
    in
        withColorsPushed


makeMove : Position -> Position -> Board -> Board
makeMove from to (Board board) =
    let
        piece =
            Dict.get from board.pieces

        isValidMove =
            not (Dict.member to board.pieces)

        newTiles =
            case piece of
                Just ( _, TerraformingPiece shape ) ->
                    terraformBoard shape from to board.tiles

                _ ->
                    board.tiles
    in
        case ( piece, isValidMove ) of
            ( Just piece, True ) ->
                Board
                    { board
                        | pieces =
                            board.pieces
                                |> Dict.remove from
                                |> Dict.insert to piece
                        , tiles = newTiles
                    }

            _ ->
                Board board


view : (Position -> Tile -> Maybe ( Player, Piece ) -> result) -> Board -> List (List result)
view renderTile (Board board) =
    let
        makeRow row =
            List.range 1 board.columns
                |> List.map (makeTile row)

        makeTile row col =
            Dict.get ( col, row ) board.tiles
                |> Maybe.withDefault (Tile [])
                |> (\tile ->
                        renderTile ( col, row )
                            tile
                            (Dict.get ( col, row ) board.pieces)
                   )
    in
        List.range 1 board.rows
            |> List.map makeRow
