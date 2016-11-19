module Shapes exposing (curve)


type alias Point =
    ( Float, Float )


curve : Point -> Point -> Point -> Int -> List ( Float, Float )
curve start control end n =
    let
        ( start_x, start_y ) =
            start

        ( control_x, control_y ) =
            control

        ( end_x, end_y ) =
            end

        x t =
            ((1 - t) * (1 - t) * start_x)
                + (2 * (1 - t) * t * control_x)
                + (t * t * end_x)

        y t =
            ((1 - t) * (1 - t) * start_y)
                + (2 * (1 - t) * t * control_y)
                + (t * t * end_y)
    in
        List.range 0 n
            |> List.map
                (\i ->
                    ( x (toFloat i / toFloat n)
                    , y (toFloat i / toFloat n)
                    )
                )
