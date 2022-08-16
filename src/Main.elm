module Main exposing (..)

import Browser
import Data
import Date exposing (Date, Unit(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Html exposing (Html)
import List.Extra as ListEx
import Random
import Svg as S
import Svg.Attributes as SAtt
import Svg.Events as SEvents
import Time exposing (Month(..))



---- MODEL ----


type alias Model =
    { designs : List Data.Design
    , focusedDesign : Maybe Data.Design
    , randomNumbers : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( { designs = Data.getAllDesigns
      , focusedDesign = Nothing
      , randomNumbers = []
      }
    , Random.generate RandomNumbers gen1000Numbers
    )


gen1000Numbers : Random.Generator (List Int)
gen1000Numbers =
    Random.list 100 (Random.int -50 50)



---- UPDATE ----


type Msg
    = ClickedDesign Int
    | RandomNumbers (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDesign index ->
            ( { model | focusedDesign = ListEx.getAt index model.designs }, Cmd.none )

        RandomNumbers numbers ->
            ( { model | randomNumbers = numbers }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        portraitView model


portraitView : Model -> Element Msg
portraitView model =
    column
        [ width (fill |> maximum 1080)
        , centerX
        , height fill
        ]
        [ title
        , row [ width fill ]
            [ timeline model
            , details model.focusedDesign
            ]
        ]


title : Element msg
title =
    el
        (titleFont
            ++ [ width fill
               , padding 30
               , Background.color <| rgb255 143 192 169
               , Font.center
               ]
        )
    <|
        paragraph [] [ text "The Designed Protein Archive" ]



-- TODO:
-- * Signpost dots to aid navigation
-- * Zoom mechanism to show time range
-- * Plot of generation of designs over time


timeline : Model -> Element Msg
timeline { designs, randomNumbers } =
    let
        timelineDates =
            getFirstAndLastDate designs
    in
    column
        (bodyFont
            ++ [ width <| fillPortion 1
               , height fill
               , padding 20
               , Font.center
               , Background.color <| rgb255 105 109 125
               ]
        )
        [ paragraph [] [ text "Timeline" ]
        , column
            [ padding 10
            , spacing 3
            , centerX
            ]
            [ timelineButton FeatherIcons.search
            , timelineButton FeatherIcons.filter
            ]
        , column [ width <| px 80, centerX ]
            [ paragraph
                (bodyFont
                    ++ [ Font.center
                       ]
                )
                [ timelineDates.firstDate
                    |> Date.year
                    |> String.fromInt
                    |> text
                ]
            , html (timelineGraphic timelineDates randomNumbers designs)
            , paragraph
                (bodyFont
                    ++ [ Font.center
                       ]
                )
                [ timelineDates.lastDate
                    |> Date.year
                    |> String.fromInt
                    |> text
                ]
            ]
        ]


timelineButton : FeatherIcons.Icon -> Element msg
timelineButton icon =
    el
        [ Border.solid
        , Border.width 2
        ]
        (icon
            |> FeatherIcons.toHtml []
            |> html
        )


timelineGraphic : { firstDate : Date, lastDate : Date } -> List Int -> List Data.Design -> Html Msg
timelineGraphic { firstDate, lastDate } randomNumbers designs =
    let
        -- plot dimensions
        width =
            20

        height =
            240

        radius =
            2
    in
    S.svg
        [ SAtt.width "100%"
        , SAtt.height "100%"
        , "0 0 "
            ++ String.fromInt width
            ++ " "
            ++ String.fromInt height
            |> SAtt.viewBox
        ]
        ([ S.line
            [ SAtt.x1 <| String.fromInt <| width // 2
            , SAtt.y1 <| String.fromInt <| 0
            , SAtt.x2 <| String.fromInt <| width // 2
            , SAtt.y2 <| String.fromInt <| height
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         , S.line
            [ SAtt.x1 <| String.fromInt <| (width // 2) - 4
            , SAtt.y1 "1"
            , SAtt.x2 <| String.fromInt <| (width // 2) + 4
            , SAtt.y2 "1"
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         , S.line
            [ SAtt.x1 <| String.fromInt <| (width // 2) - 4
            , SAtt.y1 <| String.fromInt <| height - 1
            , SAtt.x2 <| String.fromInt <| (width // 2) + 4
            , SAtt.y2 <| String.fromInt <| height - 1
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         ]
            ++ (List.indexedMap
                    Tuple.pair
                    designs
                    |> List.map2 Tuple.pair randomNumbers
                    |> List.map
                        (designToMarker
                            { width = width
                            , height = height
                            , radius = radius
                            , firstDate = firstDate
                            , lastDate = lastDate
                            }
                        )
               )
        )


getFirstAndLastDate : List Data.Design -> { firstDate : Date, lastDate : Date }
getFirstAndLastDate designs =
    let
        sortedDesigns =
            List.sortWith
                (\a b -> Date.compare a.depositionDate b.depositionDate)
                designs

        firstDesignDate =
            List.head sortedDesigns
                |> Maybe.map .depositionDate
                |> Maybe.withDefault (Date.fromCalendarDate 1900 Jan 1)

        lastDesignDate =
            List.reverse sortedDesigns
                |> List.head
                |> Maybe.map .depositionDate
                |> Maybe.withDefault (Date.fromCalendarDate 2100 Dec 31)
    in
    { firstDate = firstDesignDate, lastDate = lastDesignDate }


designToMarker :
    { width : Int
    , height : Int
    , radius : Int
    , firstDate : Date
    , lastDate : Date
    }
    -> ( Int, ( Int, Data.Design ) )
    -> S.Svg Msg
designToMarker { width, height, radius, firstDate, lastDate } ( randomShift, ( index, design ) ) =
    S.circle
        [ SAtt.cx <| String.fromInt <| width // 2
        , SAtt.cy <|
            String.fromInt <|
                (dateToPosition
                    { firstDate = firstDate
                    , lastDate = lastDate
                    , date = design.depositionDate
                    , height = height
                    , radius = radius
                    }
                    + randomShift
                )
        , SAtt.r <| String.fromInt radius
        , SAtt.fill "#68b0ab"
        , SAtt.strokeWidth "0.5"
        , SAtt.stroke "black"
        , SAtt.cursor "pointer"
        , SEvents.onClick <| ClickedDesign index
        ]
        []


dateToPosition :
    { firstDate : Date
    , lastDate : Date
    , date : Date
    , height : Int
    , radius : Int
    }
    -> Int
dateToPosition { firstDate, lastDate, date, height, radius } =
    let
        dateRange =
            Date.diff Days lastDate firstDate
                |> toFloat

        dateDelta =
            Date.diff Days date firstDate
                |> toFloat

        fraction =
            dateDelta / dateRange
    in
    fraction
        * toFloat (height - (2 * radius))
        |> round
        |> (+) 3


details : Maybe Data.Design -> Element msg
details mDesign =
    column
        [ centerX
        , width <|
            fillPortion 11
        , height fill
        ]
        [ el
            (h1Font
                ++ [ width fill
                   , padding 20
                   , Background.color <| rgb255 255 255 255
                   ]
            )
          <|
            text "Design Details"
        , case mDesign of
            Nothing ->
                paragraph
                    (bodyFont
                        ++ [ Font.center ]
                    )
                    [ text "Click on the timeline for detailed information about a design."
                    ]

            Just design ->
                designDetailsView design
        ]


designDetailsView : Data.Design -> Element msg
designDetailsView design =
    column
        [ centerX
        , width fill
        , padding 20
        , spacing 30
        ]
        [ wrappedRow
            [ height fill
            , width fill
            , spacing 10
            ]
            [ image
                []
                { src = design.picturePath
                , description = "Structure of " ++ design.pdbCode
                }
            , column
                [ height fill
                , width fill
                , spacing 10
                , Font.alignLeft
                ]
                [ paragraph
                    bodyFont
                    [ text "PDB Code: "
                    , link
                        [ Font.color <| rgb255 104 176 171
                        , Font.underline
                        ]
                        { url =
                            "https://www.ebi.ac.uk/pdbe/entry/pdb/"
                                ++ design.pdbCode
                        , label =
                            design.pdbCode
                                |> text
                        }
                    ]
                , paragraph
                    bodyFont
                    [ "Deposition Date: "
                        ++ Date.toIsoString design.depositionDate
                        |> text
                    ]
                , paragraph
                    bodyFont
                    [ "Design Method: Rational"
                        |> text
                    ]
                , paragraph
                    bodyFont
                    [ "Experimental Method: "
                        ++ design.method
                        |> text
                    ]
                , paragraph
                    bodyFont
                    [ "Structural Keywords: "
                        ++ design.structuralKeywords
                        |> text
                    ]
                , paragraph
                    bodyFont
                    [ text "Publication: "
                    , link
                        [ Font.color <| rgb255 104 176 171
                        , Font.underline
                        ]
                        { url =
                            "https://www.doi.org/"
                                ++ design.doi
                        , label =
                            design.publicationTitle
                                |> text
                        }
                    ]
                , paragraph
                    bodyFont
                    [ "Authors: "
                        ++ String.join ", " design.authors
                        |> text
                    ]
                ]
            ]
        , column
            [ width fill
            , spacing 20
            ]
            [ paragraph
                h2Font
                [ text "Sequence"
                ]
            , paragraph
                monospacedFont
                [ String.join "\n" design.sequences
                    |> text
                ]
            ]
        , column
            [ width fill
            , spacing 20
            ]
            [ paragraph
                h2Font
                [ text "Description"
                ]
            , paragraph
                bodyFont
                [ """ Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin
                    id dignissim massa. Sed ut augue mi. Sed sit amet molestie sapien,
                    et euismod augue. Quisque at nulla eu lectus pretium commodo. Morbi
                    vel diam at dolor ornare vestibulum quis a nisl. Vivamus porta
                    semper sem eget maximus. Morbi a dolor vel purus ullamcorper
                    condimentum sed sed libero. Nunc tristique nulla id felis convallis
                    feugiat. Ut bibendum pulvinar ante a posuere. Cras laoreet tellus eu
                    felis porta accumsan."""
                    |> text
                ]
            ]
        , paragraph
            h2Font
            [ text "Structural Similarity"
            ]
        , paragraph
            h2Font
            [ text "Homologues"
            ]
        , paragraph
            h2Font
            [ text "Sequence Metrics"
            ]
        , paragraph
            h2Font
            [ text "DE-STRESS Metrics"
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- STYLE ----
-- https://coolors.co/faf3dd-c8d5b9-8fc0a9-68b0ab-696d7d


titleFont : List (Attribute msg)
titleFont =
    [ Font.family
        [ Font.typeface "Barlow"
        , Font.sansSerif
        ]
    , Font.size 40
    ]


h1Font : List (Attribute msg)
h1Font =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 32
    ]


h2Font : List (Attribute msg)
h2Font =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 24
    ]


bodyFont : List (Attribute msg)
bodyFont =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 16
    , Font.alignLeft
    ]


monospacedFont : List (Attribute msg)
monospacedFont =
    [ Font.family
        [ Font.typeface "Fira Code"
        , Font.sansSerif
        ]
    , Font.size 16
    ]
