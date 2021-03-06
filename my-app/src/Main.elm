module Main exposing (..)

import Collage exposing (Form, polygon, circle, filled, outlined, defaultLine, group, collage, scale)
import Color exposing (Color, rgba)
import Element exposing (Element, opacity, flow, right, toHtml)
import Html exposing (Html, div, h1)
import Html.Attributes exposing (style)
import List.Extra as List
import Random exposing (generate, int, list)
import Color.Convert exposing (hexToColor)
import PointShapes exposing (Point, triangle, parallelogram, square, rot, to, snap)
import Mouse exposing (Position, position, moves)


---- MODEL ----


colors :
    { pastel : Color
    , skyBlue : Color
    , blue : Color
    , darkBlue : Color
    }
colors =
    { pastel = hexToColor ("190061" ++ "FF") |> Result.withDefault Color.black --par and square
    , skyBlue = hexToColor ("3500d3" ++ "FF") |> Result.withDefault Color.black --2 small triangles
    , blue = hexToColor ("282828" ++ "FF") |> Result.withDefault Color.black --Big & Med triangles
    , darkBlue = hexToColor ("0c0032" ++ "FF") |> Result.withDefault Color.black --Root Triangle
    }


logosWide : Int
logosWide =
    106


logosTall : Int
logosTall =
    50


type alias Model =
    { shuffledLogos : List (List Form)
    , lineSeeds : List Int
    , mousePos : Position
    }


init : ( Model, Cmd Msg )
init =
    ( { shuffledLogos = []
      , lineSeeds = []
      , mousePos = { x = 0, y = 0 }
      }
    , generate Shuffle (list 800 (int 0 966))
    )



---- UPDATE ----


type Msg
    = Shuffle (List Int)
    | Seeding (List Int)
    | MouseMsg Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle indexes ->
            ( { model | shuffledLogos = selections indexes }
            , generate Seeding (list logosTall (int 2 8))
            )

        Seeding seeds ->
            ( { model | lineSeeds = seeds }, Cmd.none )

        MouseMsg position ->
            ( { model | mousePos = position }, Cmd.none )


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
            (\x ->
                x % 2 == 0 || x % 3 == 0 || x % 5 == 0 || x % 7 == 0 || x % 11 == 0 || x % 13 == 0
            )



---- VIEW ----


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
            |> draw colors.darkBlue
        , big2
            |> draw colors.blue
        , sm1
            |> draw colors.skyBlue
        , par
            |> draw colors.pastel
        , sq
            |> draw colors.pastel
        , sm2
            |> draw colors.skyBlue
        , med1
            |> draw colors.blue
        ]


calcOpacity : Int -> Float
calcOpacity i =
    abs (1 - (toFloat i / toFloat logosWide))


logo : Model -> Int -> Int -> Html Msg
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

        logoHtml =
            trimmed
                |> group
                |> scale 0.125
                |> flip (::) []
                |> collage 18 18
                |> Element.toHtml
    in
        div
            [ style
                [ ( "display", "inline-block" )
                , ( "opacity", toString (calcOpacity i) )
                ]
            ]
            [ logoHtml ]


logoLine : Model -> Int -> Html Msg
logoLine model lineIndex =
    let
        line =
            List.range 1 logosWide
                |> List.map (\i -> logo model i lineIndex)
    in
        div
            [ style
                [ ( "opacity", toString (abs (1 - (toFloat lineIndex / toFloat logosTall))) )
                , ( "overflow", "hidden" )
                , ( "height", "18px" )
                , ( "white-space", "nowrap" )
                ]
            ]
            (line)


constructBackground : Model -> Html Msg
constructBackground model =
    let
        box =
            List.range 1 logosTall
                |> List.map (\lineIndex -> logoLine model lineIndex)
    in
        div
            [ style
                [ ( "overflow-y", "auto" )
                , ( "overflow-x", "hidden" )
                , ( "position", "absolute" )
                , ( "z-index", "-100" )
                ]
            ]
            (box)


view : Model -> Html Msg
view model =
    div []
        [ constructBackground model
        , h1
            [ style
                [ ( "color", "#e92c61" )
                , ( "text-shadow", "0 0 15px #E9A768" )
                , ( "font-family", "Brush Script Mt, Brush Script Std, cursive" )
                ]
            ]
            [ Html.text "Daniel Fehrenbach" ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Mouse.moves MouseMsg ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
