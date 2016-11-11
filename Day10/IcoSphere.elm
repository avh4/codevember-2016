module Day10.IcoSphere exposing (sphere)

import Math.Vector3 exposing (..)


sphere : Int -> List ( Vec3, Vec3, Vec3 )
sphere subdivisions =
    icotris_v1
        |> subdivide subdivisions
        |> List.map normalizeTri


normalizeTri : ( Vec3, Vec3, Vec3 ) -> ( Vec3, Vec3, Vec3 )
normalizeTri ( a, b, c ) =
    ( normalize a
    , normalize b
    , normalize c
    )


normalize : Vec3 -> Vec3
normalize p =
    let
        l =
            length p

        ( x, y, z ) =
            toTuple p
    in
        vec3 (x / l) (y / l) (z / l)


midpoint : Vec3 -> Vec3 -> Vec3
midpoint a b =
    let
        ( x, y, z ) =
            toTuple a

        ( x', y', z' ) =
            toTuple b
    in
        vec3 ((x + x') / 2) ((y + y') / 2) ((z + z') / 2)


subdivide : Int -> List ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
subdivide frequency tris =
    let
        subdivideTri ( v1, v2, v3 ) =
            let
                a =
                    midpoint v1 v2

                b =
                    midpoint v2 v3

                c =
                    midpoint v3 v1
            in
                [ ( v1, a, c )
                , ( v2, b, a )
                , ( v3, c, b )
                , ( a, b, c )
                ]
    in
        if frequency <= 0 then
            tris
        else
            subdivide (frequency - 1) (List.concatMap subdivideTri tris)


icotris_v1 : List ( Vec3, Vec3, Vec3 )
icotris_v1 =
    let
        t =
            (1.0 + sqrt (5.0)) / 2.0

        vertex_0 =
            vec3 (-1) t 0

        vertex_1 =
            vec3 1 t 0

        vertex_2 =
            vec3 (-1) (-t) 0

        vertex_3 =
            vec3 1 (-t) 0

        vertex_4 =
            vec3 0 (-1) t

        vertex_5 =
            vec3 0 1 t

        vertex_6 =
            vec3 0 (-1) (-t)

        vertex_7 =
            vec3 0 1 (-t)

        vertex_8 =
            vec3 t 0 (-1)

        vertex_9 =
            vec3 t 0 1

        vertex_10 =
            vec3 (-t) 0 (-1)

        vertex_11 =
            vec3 (-t) 0 1
    in
        [ -- faces around point 0
          ( vertex_0, vertex_11, vertex_5 )
        , ( vertex_0, vertex_5, vertex_1 )
        , ( vertex_0, vertex_1, vertex_7 )
          {-
             , ( vertex_0 , vertex_1 , vertex_7 )
          -}
        , ( vertex_0, vertex_7, vertex_10 )
        , ( vertex_0, vertex_10, vertex_11 )
          -- 5 adjacent faces
        , ( vertex_1, vertex_5, vertex_9 )
        , ( vertex_5, vertex_11, vertex_4 )
        , ( vertex_11, vertex_10, vertex_2 )
        , ( vertex_10, vertex_7, vertex_6 )
        , ( vertex_7, vertex_1, vertex_8 )
          -- 5 adjacent faces  around point 3
        , ( vertex_3, vertex_9, vertex_4 )
        , ( vertex_3, vertex_4, vertex_2 )
        , ( vertex_3, vertex_2, vertex_6 )
        , ( vertex_3, vertex_6, vertex_8 )
        , ( vertex_3, vertex_8, vertex_9 )
          -- 5 adjacent faces
        , ( vertex_4, vertex_9, vertex_5 )
        , ( vertex_2, vertex_4, vertex_11 )
        , ( vertex_6, vertex_2, vertex_10 )
        , ( vertex_8, vertex_6, vertex_7 )
        , ( vertex_9, vertex_8, vertex_1 )
        ]
