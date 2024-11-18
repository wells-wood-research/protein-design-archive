module Pages.Help exposing (Model, Msg, page)

import Components.Title
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Get exposing (..)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Style
import View exposing (View)



{---
page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared >> Components.Title.view shared.mScreenWidthF
        }
---}


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = \_ -> init shared.mScreenWidthF shared.mScreenHeightF
        , update = update
        , subscriptions = subscriptions
        , view = view >> Components.Title.view shared.mScreenWidthF
        }



-- INIT


type alias Model =
    { mScreenWidthF : Maybe Float
    , mScreenHeightF : Maybe Float
    }


init : Maybe Float -> Maybe Float -> ( Model, Effect Msg )
init mSharedScreenWidthF mSharedScreenHeightF =
    ( { mScreenWidthF = mSharedScreenWidthF
      , mScreenHeightF = mSharedScreenHeightF
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        screenWidth =
            getScreenWidthInt model.mScreenWidthF
    in
    { title = "Help Page"
    , attributes =
        [ centerX
        , width
            (fill
                |> minimum screenWidth
            )
        ]
    , element = helpBody screenWidth
    }


helpBody : Int -> Element msg
helpBody screenWidth =
    column
        ([ centerX
         , width fill
         , paddingXY 30 10
         , spacing 30
         , height fill
         ]
            ++ Style.bodyFont
        )
        [ column
            [ width fill
            , spacing 20
            ]
            [ paragraph
                Style.h2Font
                [ text "Section header"
                ]
            , paragraph
                (Style.monospacedFont
                    ++ [ Font.justify
                       , width (fill |> maximum (getScreenWidthIntNgl <| Just (toFloat screenWidth)))
                       ]
                )
                [ "Helpful advice"
                    |> text
                ]
            ]
        ]
