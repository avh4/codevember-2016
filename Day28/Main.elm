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
    1000


type alias Model =
    { particles : List Particle
    , t : Time
    }


initialModel : Model
initialModel =
    { particles =
        [ Particle (vec2 0 0) (vec2 5 5)
        , Particle (vec2 10 0) (vec2 -2 3)
        ]
    , t = 0
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
                , particles =
                    List.map (Particle.update t 0.1) model.particles
                        |> List.filter (Particle.inBounds ( -375, -250 ) ( 375, 250 ))
              }
            , if List.length model.particles < particleCount then
                Random.generate AddParticles (Random.list 5 Particle.random)
              else
                Cmd.none
            )

        NewParticles particles ->
            ( { model | particles = particles }
            , Cmd.none
            )

        AddParticles particle ->
            ( { model | particles = particle ++ model.particles }
            , Cmd.none
            )


drawParticle : Particle -> Collage.Form
drawParticle particle =
    let
        a =
            scale 0.2 particle.vel

        b =
            scale -0.2 particle.vel
    in
        Collage.segment (toTuple a) (toTuple b)
            |> Collage.traced
                { color = Color.hsl 0 0 (length particle.pos / 500)
                , width = 3
                , cap = Collage.Flat
                , join = Collage.Clipped
                , dashing = []
                , dashOffset = 0
                }
            |> Collage.move (toTuple particle.pos)


view : Model -> Html msg
view model =
    [ Collage.rect 750 500
        |> Collage.filled (Color.hsl (degrees 206) 0.5 (0.5 + (0.3 * sin (model.t / 100))))
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
