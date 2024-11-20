module Pages.Help exposing (Model, Msg, page)

import Browser.Dom
import Browser.Events
import Components.Title
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Get exposing (..)
import Page exposing (Page)
import Plots exposing (RenderPlotState(..))
import Route exposing (Route)
import Shared
import Style
import Time
import View exposing (View)


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
    , replotTime : Int
    , renderPlotState : RenderPlotState
    }


init : Maybe Float -> Maybe Float -> ( Model, Effect Msg )
init mSharedScreenWidthF mSharedScreenHeightF =
    ( { mScreenWidthF = mSharedScreenWidthF
      , mScreenHeightF = mSharedScreenHeightF
      , replotTime = 3
      , renderPlotState = WillRender
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = RenderWhenReady Time.Posix
    | WindowResizes Int Int
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ViewportReset


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        RenderWhenReady _ ->
            case model.renderPlotState of
                AwaitingRender 0 ->
                    ( { model | renderPlotState = Rendered }
                    , Effect.resetViewport ViewportReset
                    )

                AwaitingRender remaining ->
                    ( { model | renderPlotState = AwaitingRender (remaining - 1) }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        WindowResizes width height ->
            let
                widthF =
                    toFloat width

                heightF =
                    toFloat height
            in
            ( { model | mScreenWidthF = Just widthF, mScreenHeightF = Just heightF, renderPlotState = AwaitingRender model.replotTime }, Effect.none )

        ViewportResult result ->
            case result of
                Ok viewport ->
                    ( { model | mScreenWidthF = Just viewport.viewport.width, mScreenHeightF = Just viewport.viewport.height }, Effect.resetViewport ViewportReset )

                Err _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\width height -> WindowResizes width height)



-- VIEW


view : Model -> View Msg
view model =
    let
        title =
            "PDA | Help"
    in
    let
        screenWidth =
            getScreenWidthInt model.mScreenWidthF

        screenHeight =
            getScreenWidthInt model.mScreenHeightF - 130
    in
    { title = title
    , attributes =
        [ centerX
        , width
            (fill
                |> minimum screenWidth
            )
        ]
    , element =
        column
            [ width
                (fill
                    |> minimum screenWidth
                )
            , height
                (fill
                    |> minimum screenHeight
                )
            ]
            [ paragraph
                (Style.h2Font
                    ++ [ width fill
                       , paddingXY 0 30
                       , Font.center
                       ]
                )
                [ text title ]
            , helpBody screenWidth
            ]
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
