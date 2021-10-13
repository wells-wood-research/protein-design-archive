module Main exposing (..)

import Browser
import Data
import Date exposing (Date, Unit(..))
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Svg as S
import Svg.Attributes as SAtt
import Time exposing (Month(..))



---- MODEL ----


type alias Model =
    { designs : List Data.Design }


init : ( Model, Cmd Msg )
init =
    ( { designs = Data.designs }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        portraitView model


portraitView : Model -> Element Msg
portraitView model =
    column
        [ width fill
        , height fill
        ]
        [ title
        , timeline model
        , details
        ]


title : Element msg
title =
    el
        [ width fill
        , padding 60
        , Background.color <| rgb255 143 192 169
        , headerFont
        , Font.size 40
        , Font.center
        ]
    <|
        text "The Designed Protein Archive"


timeline : Model -> Element Msg
timeline model =
    let
        timelineDates =
            getFirstAndLastDate model.designs
    in
    column
        [ width fill
        , padding 20
        , Background.color <| rgb255 105 109 125
        , bodyFont
        , Font.size 20
        ]
        [ paragraph [] [ text "Timeline" ]
        , column [ width fill ]
            [ html (timelineGraphic timelineDates model.designs)
            , row
                [ width fill
                , Font.alignLeft
                ]
                [ paragraph
                    []
                    [ timelineDates.firstDate
                        |> Date.year
                        |> String.fromInt
                        |> text
                    ]
                , paragraph
                    [ alignRight
                    , Font.alignRight
                    ]
                    [ timelineDates.lastDate
                        |> Date.year
                        |> String.fromInt
                        |> text
                    ]
                ]
            ]
        ]


timelineGraphic : { firstDate : Date, lastDate : Date } -> List Data.Design -> Html Msg
timelineGraphic { firstDate, lastDate } designs =
    let
        -- plot dimensions
        width =
            360

        height =
            20

        radius =
            3
    in
    S.svg
        [ SAtt.width "100%"
        , "0 0 "
            ++ String.fromInt width
            ++ " "
            ++ String.fromInt height
            |> SAtt.viewBox
        ]
        ([ S.line
            [ SAtt.x1 "0"
            , SAtt.y1 <| String.fromInt <| height // 2
            , SAtt.x2 <| String.fromInt <| width
            , SAtt.y2 <| String.fromInt <| height // 2
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         , S.line
            [ SAtt.x1 "1"
            , SAtt.y1 <| String.fromInt <| (height // 2) - 4
            , SAtt.x2 "1"
            , SAtt.y2 <| String.fromInt <| (height // 2) + 4
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         , S.line
            [ SAtt.x1 <| String.fromInt <| (width - 1)
            , SAtt.y1 <| String.fromInt <| (height // 2) - 4
            , SAtt.x2 <| String.fromInt <| (width - 1)
            , SAtt.y2 <| String.fromInt <| (height // 2) + 4
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         ]
            ++ List.map
                (designToMarker
                    { width = width
                    , height = height
                    , radius = radius
                    , firstDate = firstDate
                    , lastDate = lastDate
                    }
                )
                designs
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
    -> Data.Design
    -> S.Svg msg
designToMarker { width, height, radius, firstDate, lastDate } design =
    S.circle
        [ SAtt.cx <|
            String.fromInt <|
                dateToPosition
                    { firstDate = firstDate
                    , lastDate = lastDate
                    , date = design.depositionDate
                    , width = width
                    , radius = radius
                    }
        , SAtt.cy <| String.fromInt <| height // 2
        , SAtt.r <| String.fromInt radius
        ]
        []


dateToPosition :
    { firstDate : Date
    , lastDate : Date
    , date : Date
    , width : Int
    , radius : Int
    }
    -> Int
dateToPosition { firstDate, lastDate, date, width, radius } =
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
        * toFloat (width - (2 * radius))
        |> round
        |> (+) 3
        |> Debug.log "Position"


details : Element msg
details =
    el
        [ width fill
        , height <| fillPortion 5
        , padding 20
        , Background.color <| rgb255 255 255 255
        , bodyFont
        , Font.size 20
        ]
    <|
        text "Design Details"



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


headerFont : Attribute msg
headerFont =
    Font.family
        [ Font.typeface "Barlow"
        , Font.sansSerif
        ]


bodyFont : Attribute msg
bodyFont =
    Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
