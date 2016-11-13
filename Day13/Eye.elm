module Day13.Eye exposing (Eye, startBlink, view)

import Collage
import Color exposing (Color)
import Day13.Shapes as Shapes
import Time exposing (Time)
import Animation
import Color.Mixing


-- MODEL


type alias Eye =
    { eyeSize : Float
    , irisRatio : Float
    , pupilRatio : Float
    , blinkStart : Time
    , eyelidColor : Color
    , irisColor : Color
    , whitesColor : Color
    }



-- UPDATE


startBlink : Time -> Eye -> Eye
startBlink now eye =
    { eye | blinkStart = now }



-- VIEW


view : Time -> Eye -> Collage.Form
view now model =
    let
        irisSize =
            model.eyeSize * model.irisRatio

        pupilSize =
            model.eyeSize * model.irisRatio * model.pupilRatio

        closeAnimation =
            Animation.animation model.blinkStart
                |> Animation.from 1.0
                |> Animation.to 0
                |> Animation.duration 200

        openAnimation =
            Animation.animation model.blinkStart
                |> Animation.from 0.0
                |> Animation.to 1.0
                |> Animation.duration 200
                |> Animation.delay 200

        rotationT =
            now / 1000

        opened =
            (Animation.animate now closeAnimation)
                + (Animation.animate now openAnimation)
    in
        Collage.group
            [ whites model.whitesColor irisSize model.eyeSize
            , iris
                (Color.Mixing.lighten 0.3 model.irisColor)
                model.irisColor
                pupilSize
                irisSize
                (sin rotationT)
                (sin (rotationT * 3))
                (sin (rotationT * 1.7))
            , pupil pupilSize
            , eyelids model.eyelidColor irisSize model.eyeSize (opened * 0.9)
            ]


iris : Color -> Color -> Float -> Float -> Float -> Float -> Float -> Collage.Form
iris bgcolor fgcolor pupilRadius size outerOffset midOffset pupilOffset =
    let
        pointsInCircle =
            40

        midRadius =
            (size + pupilRadius) / 2

        circlePoint r i =
            ( r * cos (degrees <| i * 360 / pointsInCircle)
            , r * sin (degrees <| i * 360 / pointsInCircle)
            )

        makeLine size midRadius outerOffset outerIndex innerOffset innerIndex =
            Collage.segment
                (circlePoint size (outerIndex + outerOffset))
                (circlePoint midRadius (innerIndex + innerOffset))
                |> Collage.traced (Collage.solid fgcolor)

        makeLines size midRadius outerOffset innerOffset outerIndex =
            [(outerIndex)..(outerIndex + 5)]
                |> List.map (makeLine size midRadius outerOffset outerIndex innerOffset)
                |> Collage.group

        makeRing size midRadius outerOffset innerOffset =
            [0..pointsInCircle]
                |> List.map (makeLines size midRadius outerOffset innerOffset)
                |> Collage.group
    in
        Collage.group
            [ Collage.circle size
                |> Collage.filled bgcolor
            , makeRing size midRadius outerOffset midOffset
            , makeRing midRadius pupilRadius midOffset pupilOffset
            ]


pupil : Float -> Collage.Form
pupil size =
    Collage.circle size
        |> Collage.filled Color.black


whites : Color -> Float -> Float -> Collage.Form
whites color height width =
    Collage.polygon
        (Shapes.curve ( width, 0 ) ( 0, (height * 2) ) ( -width, 0 ) 20
            ++ Shapes.curve ( -width, 0 ) ( 0, -(height * 2) ) ( width, 0 ) 20
        )
        |> Collage.filled color


eyelids : Color -> Float -> Float -> Float -> Collage.Form
eyelids color height width opened =
    Collage.group
        [ Collage.polygon
            (Shapes.curve ( -width, 0 ) ( 0, (height * 2 * opened) ) ( width, 0 ) 20
                ++ Shapes.curve ( width, 0 ) ( 0, (height * 2) ) ( -width, 0 ) 20
            )
            |> Collage.filled color
        , Collage.polygon
            (Shapes.curve ( -width, 0 ) ( 0, -(height * 2) ) ( width, 0 ) 20
                ++ Shapes.curve ( width, 0 ) ( 0, -(height * 2 * opened) ) ( -width, 0 ) 20
            )
            |> Collage.filled color
        ]
