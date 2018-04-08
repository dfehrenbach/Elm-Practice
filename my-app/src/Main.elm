module Main exposing (..)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Html exposing (..)
import Color exposing (Color, rgba)
import Svg.Attributes exposing (viewBox, height, width, preserveAspectRatio)


---- MODEL ----


colors :
    { pastel : Float -> Color
    , skyBlue : Float -> Color
    , blue : Float -> Color
    , darkBlue : Float -> Color
    }
colors =
    { pastel = rgba 226 255 255
    , skyBlue = rgba 116 172 252
    , blue = rgba 85 71 235
    , darkBlue = rgba 14 11 40
    }


type alias Point =
    ( Float, Float )


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


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


draw : Color.Color -> List Point -> Collage msg
draw color ps =
    ps
        |> polygon
        |> styled ( uniform color, solid thick (uniform (Color.rgb 1 0 6)) )


logo : Collage msg
logo =
    let
        big1 =
            triangle 100
                |> rot (45 + 180)

        big2 =
            triangle 100
                |> rot (-45 + 180)

        sm1 =
            triangle 50
                |> rot -45

        par =
            parallelogram 50
                |> rot (45 + 180)
                |> snap 1 (to big1 2)

        med1 =
            triangle (50 * sqrt 2)
                |> rot 180
                |> snap 2 (to par 4)

        sq =
            square 50
                |> rot 45

        sm2 =
            triangle 50
                |> rot 45
                |> snap 2 (to big2 3)
    in
        group
            [ big1
                |> draw (colors.darkBlue 0.2)
            , big2
                |> draw (colors.blue 0.2)
            , med1
                |> draw (colors.blue 0.2)
            , sm1
                |> draw (colors.skyBlue 0.2)
            , par
                |> draw (colors.pastel 0.2)
            , sq
                |> draw (colors.pastel 0.2)
            , sm2
                |> draw (colors.skyBlue 0.2)
            ]


logoBox : Collage msg
logoBox =
    logo
        |> scale 0.125
        |> List.repeat 84
        |> horizontal
        |> List.repeat 43
        |> vertical


view : Model -> Html Msg
view model =
    logoBox
        |> svgExplicit
            [ Svg.Attributes.height "100%"
            , Svg.Attributes.width "100%"
            , Svg.Attributes.viewBox " -10 -10 1520 800"
            , Svg.Attributes.preserveAspectRatio "xMinYMin slice"
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
