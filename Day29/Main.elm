module Main exposing (..)

import Math.Vector2 exposing (..)
import Time exposing (Time)
import AnimationFrame
import Particle exposing (Particle)
import Html exposing (Html)
import Collage
import Element
import Color exposing (Color)
import Random


particleCount : Int
particleCount =
    200


type alias Model =
    { particles : List Particle
    , t : Time
    , i : Int
    }


initialModel : Model
initialModel =
    { particles =
        [ Particle (vec2 -10 0) True
        , Particle (vec2 10 0) False
        , Particle (vec2 20 10) False
        ]
    , t = 0
    , i = 0
    }


type Msg
    = Tick Time
    | NewParticles (List Particle)
    | AddParticles (List Particle)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { model
                | t = model.t + 0.1
                , i = model.i + 1
                , particles =
                    List.indexedMap
                        (\i p ->
                            -- if (i % 20) == (model.i % 20) then
                            Particle.update
                                (List.filter (\n -> distance n.pos p.pos < 100) model.particles)
                                p
                         -- else
                         -- p
                        )
                        model.particles
                        |> List.filter (Particle.inBounds ( -375, -250 ) ( 375, 250 ))
              }
            , if List.length model.particles < particleCount then
                Random.generate AddParticles (Random.list 5 Particle.random)
              else
                Cmd.none
            )

        NewParticles particles ->
            ( model
            , Cmd.none
            )

        AddParticles particle ->
            ( { model | particles = particle ++ model.particles }
            , Cmd.none
            )


drawParticle : Particle -> Collage.Form
drawParticle particle =
    let
        color =
            if particle.kind then
                Color.black
            else
                Color.white

        --     a =
        --         scale 0.2 particle.vel
        --
        --     b =
        --         scale -0.2 particle.vel
    in
        Collage.circle 3
            |> Collage.filled color
            --     Collage.segment
            --     (toTuple a)
            --     (toTuple b)
            -- |> Collage.traced
            --     { color = Color.hsl 0 0 (length particle.pos / 500)
            --     , width = 3
            --     , cap = Collage.Flat
            --     , join = Collage.Clipped
            --     , dashing = []
            --     , dashOffset = 0
            --     }
            |>
                Collage.move (toTuple particle.pos)


view : Model -> Html msg
view model =
    [ Collage.rect 750 500
        |> Collage.filled (Color.hsl (degrees 42) 0.73 0.66)
    , model.particles
        |> List.map drawParticle
        |> Collage.group
    ]
        |> Collage.collage 750 500
        |> Element.toHtml


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( initialModel
            , Random.generate NewParticles (Random.list particleCount Particle.random)
            )
        , subscriptions =
            \model ->
                if List.length model.particles > 0 then
                    AnimationFrame.times Tick
                else
                    Sub.none
        , update = update
        , view = view
        }
