module Particle exposing (Particle, inBounds, update, random)

import Math.Vector2 exposing (..)
import Random


type alias Particle =
    { pos : Vec2
    , kind : Bool
    }


inBounds : ( Float, Float ) -> ( Float, Float ) -> Particle -> Bool
inBounds ( minx, miny ) ( maxx, maxy ) particle =
    let
        x =
            getX (Debug.log "p" particle.pos)

        y =
            getY particle.pos
    in
        (x >= minx && x <= maxx && y >= miny && y <= maxy)


update : List Particle -> Particle -> Particle
update neighbors particle =
    let
        ( sameNeighbors, oppositeNeighbors ) =
            List.partition (\n -> n.kind == particle.kind) neighbors

        oppositeCenter =
            List.foldl (\n -> add n.pos) (vec2 0 0) oppositeNeighbors
                |> scale (toFloat <| List.length oppositeNeighbors)

        sameCenter =
            List.foldl (\n -> add n.pos) (vec2 0 0) sameNeighbors
                |> scale (toFloat <| List.length sameNeighbors)

        targetPos =
            oppositeCenter
                |> add (Math.Vector2.negate sameCenter)
    in
        if targetPos == particle.pos then
            particle
        else
            { particle
                | pos =
                    particle.pos
                        |> add (direction targetPos particle.pos)
            }



-- let
--     ( x, y ) =
--         toTuple particle.pos
--
--     dvel =
--         vec2
--             (sin (x / 100 + t))
--             (cos (x / 100 + t))
--             |> scale 5
-- in
--     { particle
--         | pos =
--             particle.pos
--                 |> add (scale dt particle.vel)
--         , vel =
--             particle.vel
--                 |> add dvel
--     }


randomVec2 : ( Float, Float ) -> ( Float, Float ) -> Random.Generator Vec2
randomVec2 ( minx, miny ) ( maxx, maxy ) =
    Random.map2 vec2
        (Random.float minx maxx)
        (Random.float miny maxy)


random : Random.Generator Particle
random =
    Random.map2 Particle
        (randomVec2 ( -375, -250 ) ( 275, 250 ))
        (Random.bool)
