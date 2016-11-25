module Board exposing (Board, Tile(..), Piece(..), Position, new, makeMove, view)

import Color exposing (Color)
import Dict exposing (Dict)


type Piece
    = Player1
    | Player2


type Tile
    = Empty Color
    | Occupied Piece Color
    | DoesntExist


type Board
    = Board
        { columns : Int
        , rows : Int
        , tiles : Dict Position Tile
        }


type alias Position =
    ( Int, Int )


new : Board
new =
    Board
        { columns = 8
        , rows = 8
        , tiles = Dict.empty
        }
        |> map
            (\( row, col ) _ ->
                if (row + col) % 2 == 0 then
                    Empty Color.red
                else
                    Empty Color.yellow
            )
        |> map
            (\( row, col ) tile ->
                case ( tile, (row + col) % 2 ) of
                    ( Empty color, 0 ) ->
                        if row == 1 || row == 2 then
                            Occupied Player1 color
                        else if row == 7 || row == 8 then
                            Occupied Player2 color
                        else
                            tile

                    _ ->
                        tile
            )


map : (Position -> Tile -> Tile) -> Board -> Board
map fn (Board board) =
    let
        mapTile row col tiles =
            let
                pos =
                    ( col, row )

                tile =
                    Dict.get pos tiles
                        |> Maybe.withDefault DoesntExist
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


makeMove : Position -> Position -> Board -> Board
makeMove from to (Board board) =
    let
        fromTile =
            Dict.get from board.tiles
                |> Maybe.withDefault DoesntExist

        toTile =
            Dict.get to board.tiles
                |> Maybe.withDefault DoesntExist

        ( newFromTile, newToTile ) =
            case ( fromTile, toTile ) of
                ( Occupied piece fromColor, Empty toColor ) ->
                    ( Empty fromColor, Occupied piece toColor )

                _ ->
                    ( fromTile, toTile )
    in
        Board
            { board
                | tiles =
                    board.tiles
                        |> Dict.insert from newFromTile
                        |> Dict.insert to newToTile
            }


view : (Position -> Tile -> result) -> Board -> List (List result)
view renderTile (Board board) =
    let
        makeRow row =
            List.range 1 board.columns
                |> List.map (makeTile row)

        makeTile row col =
            Dict.get ( col, row ) board.tiles
                |> Maybe.withDefault DoesntExist
                |> renderTile ( col, row )
    in
        List.range 1 board.rows
            |> List.map makeRow
