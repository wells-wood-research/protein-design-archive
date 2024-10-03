module Pages.Designs.DesignId_ exposing (Model, Msg, designDetailsBody, designDetailsHeader, designDetailsView, details, page)

import AppError exposing (AppError(..))
import Browser.Dom
import Browser.Events
import Components.Title
import Effect exposing (Effect, downloadFile)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import Get exposing (..)
import Html
import Html.Attributes as HAtt
import Http
import Json.Decode
import Json.Encode as JsonEncode exposing (Value)
import List exposing (drop)
import Page exposing (Page)
import Plots exposing (RenderPlotState(..))
import ProteinDesign exposing (DownloadFileType, ProteinDesign, csvStringFromProteinDesignDownload, designDetailsFromProteinDesign, downloadDesignDecoder, fileTypeToString)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Set exposing (Set)
import Shared
import Style
import Task
import Time
import Urls
import View exposing (View)


page : Shared.Model -> Route { designId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> init shared.mScreenWidthF shared.mScreenHeightF route.params.designId
        , update = update
        , subscriptions = subscriptions
        , view = view shared >> Components.Title.view shared.mScreenWidthF
        }



-- INIT


type alias Model =
    { designId : String
    , design : RemoteData Http.Error ProteinDesign
    , errors : List AppError
    , mScreenWidthF : Maybe Float
    , mScreenHeightF : Maybe Float
    , replotTime : Int
    , renderPlotState : RenderPlotState
    , dataDownload : RemoteData Http.Error String
    }


init : Maybe Float -> Maybe Float -> String -> ( Model, Effect Msg )
init mSharedScreenWidthF mSharedScreenHeightF designId =
    ( { designId = designId
      , design = Loading
      , errors = []
      , replotTime = 3
      , renderPlotState = WillRender
      , mScreenWidthF = mSharedScreenWidthF
      , mScreenHeightF = mSharedScreenHeightF
      , dataDownload = NotAsked
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
    | RequestSelectedDesignData DownloadFileType
    | ForExportResponse DownloadFileType (Result Http.Error String)
    | AddToDownloadList
    | RemoveFromDownloadList
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
                    ( { model | design = Success design }
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
                            ( { model | mScreenWidthF = Just viewport.viewport.width, mScreenHeightF = Just viewport.viewport.height }, Effect.none )

                        Err _ ->
                            ( model, Effect.none )

                WindowResizes width height ->
                    let
                        widthF =
                            toFloat width

                        heightF =
                            toFloat height
                    in
                    ( { model | mScreenWidthF = Just widthF, mScreenHeightF = Just heightF }, Effect.resetViewport ViewportReset )

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
                RequestSelectedDesignData fileType ->
                    ( { model | dataDownload = Loading }
                    , Http.get
                        { url = Urls.downloadSelectedDesigns [ model.designId ]
                        , expect =
                            Http.expectString (ForExportResponse fileType)
                        }
                        |> Effect.sendCmd
                    )

                ForExportResponse _ (Err err) ->
                    ( { model | dataDownload = Failure err }
                    , Effect.none
                    )

                ForExportResponse fileType (Ok designData) ->
                    let
                        encodedFileContent =
                            case fileType of
                                ProteinDesign.Json ->
                                    designData

                                ProteinDesign.Csv ->
                                    case Json.Decode.decodeString (Json.Decode.list downloadDesignDecoder) designData of
                                        Ok designs ->
                                            csvStringFromProteinDesignDownload designs

                                        Err _ ->
                                            designData
                    in
                    ( { model | dataDownload = NotAsked }
                    , Effect.downloadFile model.designId encodedFileContent fileType
                    )

                AddToDownloadList ->
                    ( model, Effect.addDesignsToDownload [ model.designId ] )

                RemoveFromDownloadList ->
                    ( model, Effect.removeDesignsFromDownload [ model.designId ] )

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


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Design Details"
    , attributes =
        [ centerX
        , width
            (fill
                |> minimum (getScreenWidthInt model.mScreenWidthF)
            )
        ]
    , element = details shared model
    }


details : Shared.Model -> Model -> Element Msg
details shared model =
    let
        mDesign =
            model.design

        screenWidth =
            getScreenWidthInt model.mScreenWidthF

        screenHeight =
            getScreenWidthInt model.mScreenHeightF - 130
    in
    column
        [ width (fill |> maximum screenWidth) ]
        [ case mDesign of
            NotAsked ->
                column
                    (Style.monospacedFont
                        ++ [ width (fill |> maximum screenWidth)
                           , height <| px screenHeight
                           , centerX
                           , spaceEvenly
                           ]
                    )
                    [ el [ centerX, centerY ]
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 106 <|
                                    FeatherIcons.refreshCcw
                        )
                    , paragraph [ Font.center, Font.size 24, padding 50 ] [ text "Error querying the database. Try reloading the page." ]
                    ]

            Loading ->
                column
                    (Style.monospacedFont
                        ++ [ width (fill |> maximum screenWidth)
                           , height <| px screenHeight
                           , centerX
                           , spaceEvenly
                           ]
                    )
                    [ el [ centerX, centerY ]
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 106 <|
                                    FeatherIcons.loader
                        )
                    , paragraph [ Font.center, Font.size 16, padding 50, centerY ] [ text "Loading the design..." ]
                    ]

            Failure e ->
                column
                    (Style.monospacedFont
                        ++ [ width (fill |> maximum screenWidth)
                           , height <| px screenHeight
                           , centerX
                           , spaceEvenly
                           ]
                    )
                    [ el [ centerX, centerY ]
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 106 <|
                                    FeatherIcons.alertOctagon
                        )
                    , paragraph [ Font.center, Font.size 24, padding 50 ]
                        [ case e of
                            Http.BadUrl _ ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text "Error loading design: invalid URL." ]

                            Http.Timeout ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text "Error loading design: it took too long to get a response." ]

                            Http.NetworkError ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text "Error loading design: please connect to the Internet." ]

                            Http.BadStatus i ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text ("Error loading design: status code " ++ String.fromInt i) ]

                            Http.BadBody s ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text ("Error decoding JSON: " ++ s) ]
                        ]
                    ]

            Success design ->
                designDetailsView shared screenWidth design
        ]


designDetailsView : Shared.Model -> Int -> ProteinDesign -> Element Msg
designDetailsView shared screenWidth proteinDesign =
    column
        ([ centerX
         , width (fill |> maximum screenWidth)
         , height fill
         , paddingXY 10 0
         ]
            ++ Style.bodyFont
        )
        [ designDetailsHeader "Design Details" "/designs/" proteinDesign
        , downloadArea shared screenWidth proteinDesign.pdb
        , designDetailsBody screenWidth proteinDesign
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


downloadArea : Shared.Model -> Int -> String -> Element Msg
downloadArea shared screenWidth designId =
    let
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

        elementType =
            if screenWidth < 600 then
                column

            else
                row
    in
    elementType
        [ width (fill |> maximum screenWidth)
        , Font.bold
        , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
        , Border.color <| rgb255 220 220 220
        ]
        [ downloadButton widthButton buttonAttributes (Just <| RequestSelectedDesignData ProteinDesign.Csv) (text "Download CSV")
        , downloadButton widthButton buttonAttributes (Just <| RequestSelectedDesignData ProteinDesign.Json) (text "Download JSON")
        , if Set.member designId shared.designsToDownload then
            downloadButton widthButton buttonAttributes (Just RemoveFromDownloadList) (text "Remove from download")

          else
            downloadButton widthButton buttonAttributes (Just AddToDownloadList) (text "Add to download list")
        ]


designDetailsHeader : String -> String -> ProteinDesign -> Element msg
designDetailsHeader title path { previous_design, next_design } =
    row
        [ width fill
        , spaceEvenly
        , paddingXY 0 30
        ]
        [ link
            []
            { url = path ++ previous_design
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
            { url = path ++ next_design
            , label =
                el [ centerX ]
                    (html <|
                        FeatherIcons.toHtml [ HAtt.align "center" ] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.arrowRightCircle
                    )
            }
        ]


designDetailsBody : Int -> ProteinDesign -> Element msg
designDetailsBody screenWidth proteinDesign =
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
            , width (fill |> maximum screenWidth)
            , spacing 10
            , Font.justify
            ]
            [ table
                [ padding 2
                , width (fill |> maximum (getScreenWidthIntNgl <| Just <| toFloat screenWidth))
                ]
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
                                        [ width (fill |> maximum (screenWidth - 200))
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
            [ width (fill |> maximum (getScreenWidthIntNgl <| Just <| toFloat screenWidth))
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
                [ width <| px (getScreenWidthIntNgl <| Just <| toFloat screenWidth)
                , height <| px 400
                ]
                ( proteinDesign.pdb
                , Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" (getScreenWidthStringNgl <| Just <| toFloat screenWidth)
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
            [ padding 2
            , width (fill |> maximum (getScreenWidthIntNgl <| Just <| toFloat screenWidth))
            ]
            { data = proteinDesign.chains
            , columns =
                [ { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Chain ID" ]
                  , width = fillPortion 1
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum 100)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text chain.chain_id ]
                  }
                , { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Type" ]
                  , width = fillPortion 1
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum 100)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text chain.chain_type ]
                  }
                , { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Source" ]
                  , width = fillPortion 2
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum 100)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text <| String.toLower chain.chain_source ]
                  }
                , { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 10 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Sequence" ]
                  , width = fillPortion 6
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum (screenWidth - 400))
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text chain.chain_seq_unnat ]
                  }
                , { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Length" ]
                  , width = fillPortion 1
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum 100)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text <| String.fromInt chain.chain_length ]
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
                (Style.monospacedFont
                    ++ [ Font.justify
                       , width (fill |> maximum (getScreenWidthIntNgl <| Just <| toFloat screenWidth))
                       ]
                )
                [ proteinDesign.abstract
                    |> text
                ]
            ]
        ]
