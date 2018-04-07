module Main exposing (..)

import Svg exposing (..)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import Svg.Attributes exposing (..)


---- MODEL ----


colors =
    { gray = "#5a6378"
    , green = "#83c833"
    , orange = "#efa500"
    , blue = "#5fb4ca"
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


square : List Point
square =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 1, 1 )
    , ( 0, 1 )
    ]


parallelogram : List Point
parallelogram =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 2, -1 )
    , ( 1, -1 )
    ]


rotate : Float -> List Point -> List Point
rotate angle ps =
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


draw : String -> List Point -> Svg msg
draw color ps =
    polygon
        [ ps
            |> List.concatMap (\( x, y ) -> [ x, y ])
            |> List.map toString
            |> String.join ","
            |> points
        , fill color
        , stroke "white"
        , strokeWidth "0.07"
        ]
        []


view : Model -> Html Msg
view model =
    let
        big1 =
            triangle 2
                |> rotate (45 + 180)

        big2 =
            triangle 2
                |> rotate -45

        sm1 =
            triangle 1
                |> rotate (45 + 90)

        par =
            parallelogram
                |> rotate -45
                |> snap 1 (to big1 3)

        med1 =
            triangle (sqrt 2)
                |> rotate -90
                |> snap 3 (to par 4)

        sq =
            square
                |> rotate 45

        sm2 =
            triangle 1
                |> rotate 45
                |> snap 3 (to big2 2)
    in
        div []
            [ svg
                [ viewBox "-3 -3 10 10" ]
                [ big1
                    |> draw colors.gray
                , big2
                    |> draw colors.blue
                , sm1
                    |> draw colors.orange
                , par
                    |> draw colors.green
                , med1
                    |> draw colors.blue
                , sq
                    |> draw colors.green
                , sm2
                    |> draw colors.orange
                ]
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
