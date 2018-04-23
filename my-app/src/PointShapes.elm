module PointShapes exposing (Point, triangle, square, parallelogram, rot, to, snap)


type alias Point =
    ( Float, Float )


triangle : Float -> List Point
triangle size =
    [ ( 0, 0 )
    , ( size, 0 )
    , ( 0, size )
    ]


square : Float -> List Point
square size =
    [ ( 0, 0 )
    , ( size, 0 )
    , ( size, size )
    , ( 0, size )
    ]


parallelogram : Float -> List Point
parallelogram size =
    [ ( 0, 0 )
    , ( -size, 0 )
    , ( -size * 2, -size )
    , ( -size, -size )
    ]


rot : Float -> List Point -> List Point
rot angle ps =
    let
        rad =
            degrees angle

        rotateCoord ( x, y ) =
            ( cos rad * x + sin rad * y
            , sin rad * -x + cos rad * y
            )
    in
        List.map rotateCoord ps


to : List Point -> Int -> Point
to ps pointNumber =
    ps
        |> List.drop (pointNumber - 1)
        |> List.head
        |> Maybe.withDefault ( 0, 0 )


snap : Int -> Point -> List Point -> List Point
snap pointNumber snapTarget ps =
    case
        ps
            |> List.drop (pointNumber - 1)
            |> List.head
    of
        Just pivot ->
            ps
                |> sub pivot
                |> add snapTarget

        Nothing ->
            ps


sub : Point -> List Point -> List Point
sub ( x, y ) =
    add ( -x, -y )


add : Point -> List Point -> List Point
add ( dx, dy ) ps =
    List.map (\( x, y ) -> ( x + dx, y + dy )) ps
