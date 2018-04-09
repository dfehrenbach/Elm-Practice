module Main exposing (..)

import Collage exposing (..)
import Color exposing (Color, rgba)
import Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import List.Extra as List
import Random exposing (generate, int, list)


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
    , darkBlue = rgba 14 11 100
    }


type alias Point =
    ( Float, Float )


type alias Model =
    { shuffledLogos : List (List Form)
    , lineSeeds : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( { shuffledLogos = []
      , lineSeeds = []
      }
    , generate Shuffle (list 800 (int 0 966))
    )



---- UPDATE ----


type Msg
    = Shuffle (List Int)
    | Seeding (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle indexes ->
            ( { model | shuffledLogos = selections indexes }
            , generate Seeding (list logosTall (int 2 8))
            )

        Seeding seeds ->
            ( { model | lineSeeds = seeds }, Cmd.none )


selections : List Int -> List (List Form)
selections indexes =
    indexes
        |> List.map
            (\i ->
                perms
                    |> List.getAt i
                    |> Maybe.withDefault logoShape
            )


perms : List (List Form)
perms =
    logoShape
        |> List.permutations
        |> List.removeIfIndex
            (\x -> x % 2 == 0 || x % 3 == 0 || x % 5 == 0 || x % 7 == 0 || x % 11 == 0 || x % 13 == 0)



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


draw : Color.Color -> List Point -> Form
draw color ps =
    let
        poly =
            polygon ps

        fpoly =
            filled color poly

        opoly =
            outlined { defaultLine | width = 4, color = Color.rgb 1 0 6 } poly
    in
        group [ fpoly, opoly ]


logoShape : List Form
logoShape =
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
        [ big1
            |> draw (colors.darkBlue 0.8)
        , big2
            |> draw (colors.blue 0.8)
        , sm1
            |> draw (colors.skyBlue 0.8)
        , par
            |> draw (colors.pastel 0.8)
        , sq
            |> draw (colors.pastel 0.8)
        , sm2
            |> draw (colors.skyBlue 0.8)
        , med1
            |> draw (colors.blue 0.8)
        ]


logosWide : Int
logosWide =
    106


logosTall : Int
logosTall =
    52


logo : Model -> Int -> Int -> Element
logo model i lineIndex =
    let
        lineSeed =
            model.lineSeeds |> List.getAt lineIndex |> Maybe.withDefault 8

        logoS =
            model.shuffledLogos |> List.getAt (i + (lineIndex * lineSeed)) |> Maybe.withDefault logoShape

        ratioTall =
            toFloat lineIndex / toFloat logosTall

        ratioWide =
            toFloat i / toFloat logosWide

        ratioMax =
            max ratioTall ratioWide

        takeNum =
            abs (6 - round (ratioMax * 5))

        trimmed =
            List.take takeNum logoS
    in
        trimmed
            |> group
            |> Collage.scale 0.125
            |> flip (::) []
            |> collage 18 18


calcOpacity : Int -> Element -> Element
calcOpacity i log =
    opacity (abs (1 - (toFloat i / toFloat logosWide))) log


logoLine : Model -> Int -> Html msg
logoLine model lineIndex =
    let
        line =
            List.range 1 logosWide
                |> List.map (\i -> logo model i lineIndex)
                |> List.indexedMap (calcOpacity)
                |> flow right
                |> Element.toHtml
    in
        div
            [ style
                [ ( "opacity", toString (abs (1 - (toFloat lineIndex / toFloat logosTall))) )
                ]
            ]
            [ line ]


constructBox : Model -> List (Html msg)
constructBox model =
    List.range 1 logosTall
        |> List.map (\lineIndex -> logoLine model lineIndex)


view : Model -> Html msg
view model =
    -- logobox2 model |> Element.toHtml
    div [] (constructBox model)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
