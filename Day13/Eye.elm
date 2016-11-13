module Day13.Eye exposing (eye)

import Collage
import Color
import Day13.Shapes as Shapes


eye : Collage.Form
eye =
    Collage.group
        [ iris 120
        , pupil 50
        , eyelids 120 200
        ]


iris : Float -> Collage.Form
iris size =
    Collage.circle size
        |> Collage.filled Color.yellow


pupil : Float -> Collage.Form
pupil size =
    Collage.circle size
        |> Collage.filled Color.black


eyelids : Float -> Float -> Collage.Form
eyelids height width =
    Collage.group
        [ Collage.polygon
            (Shapes.curve ( -width, 0 ) ( 0, (height * 1.2) ) ( width, 0 ) 20
                ++ Shapes.curve ( width, 0 ) ( 0, (height * 2) ) ( -width, 0 ) 20
            )
            |> Collage.filled Color.brown
        , Collage.polygon
            (Shapes.curve ( -width, 0 ) ( 0, -(height * 2) ) ( width, 0 ) 20
                ++ Shapes.curve ( width, 0 ) ( 0, -(height * 1.2) ) ( -width, 0 ) 20
            )
            |> Collage.filled Color.brown
        ]
