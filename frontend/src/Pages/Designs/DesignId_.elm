module Pages.Designs.DesignId_ exposing (Model, Msg, designDetailsBody, designDetailsHeader, designDetailsView, details, page)

import AppError exposing (AppError(..))
import Browser.Dom
import Browser.Events
import Components.Title
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import File exposing (File)
import File.Download as Download
import Get exposing (..)
import Html
import Html.Attributes as HAtt
import Http
import List exposing (drop)
import Page exposing (Page)
import Plots exposing (RenderPlotState(..))
import ProteinDesign exposing (ProteinDesign, designDetailsFromProteinDesign)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Style
import Task
import Time
import Urls
import View exposing (View)


page : Shared.Model -> Route { designId : String } -> Page Model Msg
page { mScreenWidthF } route =
    Page.new
        { init = \_ -> init mScreenWidthF route.params.designId
        , update = update
        , subscriptions = subscriptions
        , view = view >> Components.Title.view
        }



-- INIT


type alias Model =
    { designId : String
    , design : RemoteData Http.Error ProteinDesign
    , csv : Maybe String
    , json : Maybe String
    , errors : List AppError
    , mScreenWidthF : Maybe Float
    , replotTime : Int
    , renderPlotState : RenderPlotState
    }


init : Maybe Float -> String -> ( Model, Effect Msg )
init mSharedScreenWidthF designId =
    ( { designId = designId
      , design = Loading
      , csv = Nothing
      , json = Nothing
      , errors = []
      , replotTime = 3
      , renderPlotState = WillRender
      , mScreenWidthF = mSharedScreenWidthF
      }
    , Effect.batch
        [ Effect.sendCmd (Task.attempt ViewportResult Browser.Dom.getViewport)
        , Effect.resetViewport ViewportReset
        , Effect.sendCmd (getData <| Urls.designDetailsFromId designId)
        ]
    )


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect =
            Http.expectJson DesignsDataReceived ProteinDesign.rawDesignDecoder
        }



-- UPDATE


type Msg
    = SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error ProteinDesign)
    | CsvRequested
    | JsonRequested
    | AddToDownloadList
    | RenderWhenReady Time.Posix
    | WindowResizes Int Int
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ViewportReset


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case model.design of
        RemoteData.NotAsked ->
            case msg of
                SendDesignsHttpRequest ->
                    ( { model | design = Loading }
                    , Effect.sendCmd (getData <| Urls.designDetailsFromId model.designId)
                    )

                _ ->
                    ( model, Effect.none )

        RemoteData.Loading ->
            case msg of
                SendDesignsHttpRequest ->
                    ( model, Effect.none )

                DesignsDataReceived (Ok design) ->
                    ( { model
                        | design = Success design
                        , csv = Just model.designId
                        , json = Just model.designId
                      }
                    , Effect.none
                    )

                DesignsDataReceived (Err e) ->
                    ( { model
                        | design = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width }, Effect.none )

                        Err _ ->
                            ( model, Effect.none )

                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mScreenWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                _ ->
                    ( model, Effect.none )

        RemoteData.Failure e ->
            case msg of
                _ ->
                    ( { model
                        | design = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

        RemoteData.Success _ ->
            case msg of
                CsvRequested ->
                    ( model
                    , case model.csv of
                        Just csvString ->
                            Effect.sendCmd (Download.string ("pda_" ++ model.designId ++ ".csv") "text/csv" csvString)

                        Nothing ->
                            Debug.todo "Display error"
                    )

                JsonRequested ->
                    ( model
                    , case model.json of
                        Just jsonString ->
                            Effect.sendCmd (Download.string ("pda_" ++ model.designId ++ ".json") "application/json" jsonString)

                        Nothing ->
                            Debug.todo "Display error"
                    )

                AddToDownloadList ->
                    ( model, Effect.none )

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

                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mScreenWidthF = Just widthF, renderPlotState = AwaitingRender model.replotTime }, Effect.none )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

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
    { title = "Design Details"
    , attributes =
        [ centerX
        , width
            (fill
                |> minimum (getScreenWidthInt model.mScreenWidthF)
            )
        ]
    , element = details model.mScreenWidthF model.design
    }


details : Maybe Float -> RemoteData Http.Error ProteinDesign -> Element Msg
details mScreenWidthF mDesign =
    row []
        [ column
            [ width fill ]
            [ case mDesign of
                NotAsked ->
                    paragraph
                        (Style.bodyFont
                            ++ [ width fill, Font.center, Font.justify ]
                        )
                        [ text "Error querying the database. Try reloading the page."
                        ]

                Loading ->
                    paragraph
                        (Style.bodyFont
                            ++ [ width fill, Font.center, Font.justify ]
                        )
                        [ text "Loading the design..."
                        ]

                Failure e ->
                    paragraph
                        (Style.bodyFont
                            ++ [ width fill, Font.center, Font.justify ]
                        )
                        [ case e of
                            Http.BadUrl _ ->
                                text "Error loading design: invalid URL."

                            Http.Timeout ->
                                text "Error loading design: it took too long to get a response."

                            Http.NetworkError ->
                                text "Error loading design: please connect to the Internet."

                            Http.BadStatus i ->
                                text ("Error loading design: status code " ++ String.fromInt i)

                            Http.BadBody s ->
                                text ("Error decoding JSON: " ++ s)
                        ]

                Success design ->
                    designDetailsView mScreenWidthF design
            ]
        ]


designDetailsView : Maybe Float -> ProteinDesign -> Element Msg
designDetailsView mScreenWidthF proteinDesign =
    column
        ([ centerX
         , width (fill |> maximum (getScreenWidthInt mScreenWidthF))
         , padding 30
         , height fill
         ]
            ++ Style.bodyFont
        )
        [ designDetailsHeader "Design Details" "/designs/" proteinDesign
        , downloadArea mScreenWidthF
        , designDetailsBody mScreenWidthF proteinDesign
        ]


downloadButton : Length -> List (Attribute msg) -> Maybe msg -> Element msg -> Element msg
downloadButton widthButton buttonAttributes onPressCmd textLabel =
    Input.button
        (buttonAttributes
            ++ [ padding 10
               , width widthButton
               , centerX
               , Font.center
               , Element.mouseOver
                    [ Background.color <| rgb255 220 220 220
                    ]
               ]
        )
        { onPress = onPressCmd
        , label = textLabel
        }


downloadArea : Maybe Float -> Element Msg
downloadArea mScreenWidthF =
    let
        screenWidth =
            getScreenWidthInt mScreenWidthF

        widthButton =
            if screenWidth < 600 then
                Element.fill |> maximum (screenWidth - 10)

            else
                Element.px 200

        buttonAttributes =
            if screenWidth < 600 then
                [ Border.widthEach { bottom = 1, top = 1, left = 0, right = 0 }
                , Border.color <| rgb255 220 220 220
                ]

            else
                [ centerX
                , Font.center
                ]
    in
    if screenWidth < 600 then
        column
            [ width (fill |> maximum (getScreenWidthInt mScreenWidthF))
            , Font.bold
            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
            , Border.color <| rgb255 220 220 220
            ]
            [ downloadButton widthButton buttonAttributes (Just CsvRequested) (text "Download CSV")
            , downloadButton widthButton buttonAttributes (Just JsonRequested) (text "Download JSON")
            , downloadButton widthButton buttonAttributes (Just AddToDownloadList) (text "Add to download list")
            ]

    else
        row
            [ width (fill |> maximum (getScreenWidthInt mScreenWidthF))
            , Font.bold
            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
            , Border.color <| rgb255 220 220 220
            ]
            [ downloadButton widthButton buttonAttributes (Just CsvRequested) (text "Download CSV")
            , downloadButton widthButton buttonAttributes (Just JsonRequested) (text "Download JSON")
            , downloadButton widthButton buttonAttributes (Just AddToDownloadList) (text "Add to download list")
            ]


designDetailsHeader : String -> String -> ProteinDesign -> Element msg
designDetailsHeader title path { previousDesign, nextDesign } =
    row
        [ width fill
        , spaceEvenly
        , paddingXY 0 30
        ]
        [ link
            []
            { url = path ++ previousDesign
            , label =
                el [ centerX ]
                    (html <|
                        FeatherIcons.toHtml [ HAtt.align "center" ] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.arrowLeftCircle
                    )
            }
        , paragraph
            (Style.h2Font ++ [ Font.center ])
            [ text title ]
        , link
            []
            { url = path ++ nextDesign
            , label =
                el [ centerX ]
                    (html <|
                        FeatherIcons.toHtml [ HAtt.align "center" ] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.arrowRightCircle
                    )
            }
        ]


designDetailsBody : Maybe Float -> ProteinDesign -> Element msg
designDetailsBody mScreenWidthF proteinDesign =
    column
        ([ centerX
         , width fill
         , padding 30
         , spacing 30
         , height fill
         ]
            ++ Style.bodyFont
        )
        [ column
            [ height fill
            , width (fill |> maximum (getScreenWidthInt mScreenWidthF))
            , spacing 10
            , Font.justify
            ]
            [ table
                [ padding 2 ]
                { data = designDetailsFromProteinDesign proteinDesign
                , columns =
                    [ { header =
                            paragraph
                                [ Font.bold
                                , paddingXY 5 10
                                , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                                , Border.color <| rgb255 220 220 220
                                ]
                                [ text "Attribute" ]
                      , width = fillPortion 2
                      , view =
                            \category ->
                                paragraph
                                    Style.monospacedFont
                                    [ column
                                        [ width (fill |> maximum 150)
                                        , height fill
                                        , scrollbarX
                                        , paddingXY 5 10
                                        ]
                                        [ text category.header ]
                                    ]
                      }
                    , { header =
                            paragraph
                                [ Font.bold
                                , paddingXY 10 10
                                , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                                , Border.color <| rgb255 220 220 220
                                ]
                                [ text "Value" ]
                      , width = fillPortion 8
                      , view =
                            \detail ->
                                paragraph
                                    Style.monospacedFont
                                    [ column
                                        [ width (fill |> maximum (getScreenWidthInt mScreenWidthF - 200))
                                        , height fill
                                        , scrollbarX
                                        , paddingXY 10 10
                                        ]
                                        [ detail.property ]
                                    ]
                      }
                    ]
                }
            ]
        , column
            [ width (fill |> maximum (getScreenWidthIntNgl mScreenWidthF))
            , spacing 20
            ]
            [ column
                Style.h2Font
                [ text "Structure"
                ]
            ]
        , column
            [ spacing 20
            , centerX
            ]
            [ Keyed.el
                [ width <| px (getScreenWidthIntNgl mScreenWidthF)
                , height <| px 400

                --, Border.width 2
                --, Border.rounded 3
                --, Border.color <| rgb255 220 220 220
                ]
                ( proteinDesign.pdb
                , Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" (getScreenWidthStringNgl mScreenWidthF)
                    , HAtt.style "height" "400px"
                    , HAtt.style "align" "center"
                    , HAtt.alt "3D structure"
                    , HAtt.attribute "pdb-string" proteinDesign.pdb
                    ]
                    []
                    |> html
                )
            ]
        , paragraph
            Style.h2Font
            [ text "Sequence"
            ]
        , table
            [ padding 2 ]
            { data = proteinDesign.chains
            , columns =
                [ { header =
                        paragraph
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Chain ID" ]
                  , width = fillPortion 2
                  , view =
                        \chain ->
                            paragraph
                                Style.monospacedFont
                                [ column
                                    [ width
                                        (fill
                                            |> maximum 150
                                            |> minimum 80
                                        )
                                    , height fill
                                    , scrollbarX
                                    , paddingXY 5 10
                                    ]
                                    [ text chain.chain_id ]
                                ]
                  }
                , { header =
                        paragraph
                            [ width fill
                            , Font.bold
                            , paddingXY 10 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Sequence" ]
                  , width = fillPortion 8
                  , view =
                        \chain ->
                            paragraph
                                Style.monospacedFont
                                [ column
                                    [ width (fill |> maximum (getScreenWidthInt mScreenWidthF - 200))
                                    , height fill
                                    , scrollbarX
                                    , paddingXY 10 10
                                    ]
                                    [ text chain.chain_seq_unnat ]
                                ]
                  }
                ]
            }
        , column
            [ width fill
            , spacing 20
            ]
            [ paragraph
                Style.h2Font
                [ text "Description"
                ]
            , paragraph
                [ Font.justify ]
                [ proteinDesign.abstract
                    |> text
                ]
            ]
        ]
