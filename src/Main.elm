module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)



---- MODEL ----


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
    el
        [ width fill
        , padding 20
        , Background.color <| rgb255 105 109 125
        , bodyFont
        , Font.size 20
        ]
    <|
        text "Timeline"


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
