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
        [ width fill ]
        [ title

        --, timeline
        --, details
        ]


title : Element msg
title =
    el
        [ width fill
        , height <| fillPortion 1
        , padding 60
        , Background.color <| rgb255 143 192 169
        , headerFont
        , Font.size 40
        ]
    <|
        text "The Designed Protein Archive"



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
