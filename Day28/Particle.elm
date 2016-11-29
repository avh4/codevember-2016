module Particle exposing (Particle, inBounds, update, random)

import Math.Vector2 exposing (..)
import Random


type alias Particle =
    { pos : Vec2
    , vel : Vec2
    }


inBounds : ( Float, Float ) -> ( Float, Float ) -> Particle -> Bool
inBounds ( minx, miny ) ( maxx, maxy ) particle =
    let
        x =
            getX particle.pos

        y =
            getY particle.pos
    in
        x >= minx && x <= maxx && y >= miny && y <= maxy


update : Float -> Float -> Particle -> Particle
update t dt particle =
    let
        ( x, y ) =
            toTuple particle.pos

        dvel =
            vec2
                (sin (x / 100 + t))
                (cos (x / 100 + t))
                |> scale 5
    in
        { particle
            | pos =
                particle.pos
                    |> add (scale dt particle.vel)
            , vel =
                particle.vel
                    |> add dvel
        }


randomVec2 : ( Float, Float ) -> ( Float, Float ) -> Random.Generator Vec2
randomVec2 ( minx, miny ) ( maxx, maxy ) =
    Random.map2 vec2
        (Random.float minx maxx)
        (Random.float miny maxy)


random : Random.Generator Particle
random =
    Random.map2 Particle
        (randomVec2 ( -375, -250 ) ( 275, 250 ))
        (randomVec2 ( -1, -1 ) ( 1, 1 ))
