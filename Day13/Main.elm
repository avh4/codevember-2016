module Main exposing (..)

import Collage
import Element
import Day13.Eye as Eye


main =
    Collage.collage 750
        500
        [ Eye.eye
        ]
        |> Element.toHtml
