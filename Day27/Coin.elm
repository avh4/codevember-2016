module Coin exposing (..)

import Math.Vector2 exposing (..)


type alias Point =
    Vec2


type alias FlatCoin =
    { center : Point
    , radius : Float
    }


type alias FlippingCoin =
    { anchor : Point
    , flipAngle : Float
    , centerDirection : Vec2
    , radius : Float
    }


type alias FallingCoin =
    { anchor : Point
    , fallAngle : Float
    , centerDirection : Vec2
    , radius : Float
    }


startFlip : Point -> FlatCoin -> FlippingCoin
startFlip mousePosition coin =
    { anchor =
        (direction mousePosition coin.center)
            |> Math.Vector2.negate
            |> scale coin.radius
            |> add coin.center
    , radius = coin.radius
    , centerDirection = direction mousePosition coin.center
    , flipAngle = 0
    }


type PushResult
    = Stopped FlatCoin
    | Continue FlippingCoin
    | Flipped FallingCoin


pushFlippingCoin : Point -> FlippingCoin -> PushResult
pushFlippingCoin mousePosition coin =
    let
        d =
            dot (sub mousePosition coin.anchor) coin.centerDirection
                / (2 * coin.radius)
    in
        if d >= 1 then
            Stopped
                { center = add coin.anchor (scale coin.radius coin.centerDirection)
                , radius = coin.radius
                }
        else if d <= 0.03 then
            Flipped
                { anchor = coin.anchor
                , radius = coin.radius
                , centerDirection = coin.centerDirection
                , fallAngle = degrees 90
                }
        else
            Continue
                { coin
                    | flipAngle =
                        acos d
                }


advanceFallingCoin : FallingCoin -> Result FlatCoin FallingCoin
advanceFallingCoin coin =
    let
        newAngle =
            coin.fallAngle + (degrees 3)
    in
        if newAngle >= (degrees 180) then
            Err
                { radius = coin.radius
                , center = add coin.anchor (scale -coin.radius coin.centerDirection)
                }
        else
            Ok { coin | fallAngle = newAngle }
