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
import Get exposing (..)
import Html
import Html.Attributes as HAtt
import Http
import Json.Decode
import Page exposing (Page)
import Plots exposing (RenderPlotState(..))
import ProteinDesign exposing (DownloadFileType, ProteinDesign, csvStringFromProteinDesignDownload, designDetailsFromProteinDesign, downloadDesignDecoder, jsonStringFromProteinDesignDownload)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Set
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
                                    case Json.Decode.decodeString (Json.Decode.list downloadDesignDecoder) designData of
                                        Ok designs ->
                                            jsonStringFromProteinDesignDownload designs

                                        Err _ ->
                                            "There was an error in generating JSON file. Please try downloading CSV instead and contact us with feedback if the issue persists."

                                ProteinDesign.Csv ->
                                    case Json.Decode.decodeString (Json.Decode.list downloadDesignDecoder) designData of
                                        Ok designs ->
                                            csvStringFromProteinDesignDownload designs

                                        Err _ ->
                                            "There was an error in generating CSV file. Please try downloading JSON instead and contact us with feedback if the issue persists."
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
    { title = "PDA | " ++ model.designId
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
                designDetailsView shared design screenWidth screenHeight
        ]


designDetailsView : Shared.Model -> ProteinDesign -> Int -> Int -> Element Msg
designDetailsView shared proteinDesign screenWidth screenHeight =
    column
        ([ centerX
         , width (fill |> maximum screenWidth)
         , height fill
         , paddingXY 0 0
         ]
            ++ Style.bodyFont
        )
        [ designDetailsHeader "Design Details" "/designs/" proteinDesign screenWidth
        , downloadArea shared proteinDesign.pdb screenWidth
        , designDetailsBody proteinDesign screenWidth screenHeight
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


downloadArea : Shared.Model -> String -> Int -> Element Msg
downloadArea shared designId screenWidth =
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
        , centerX
        ]
        [ downloadButton widthButton buttonAttributes (Just <| RequestSelectedDesignData ProteinDesign.Csv) (text "Download CSV")
        , downloadButton widthButton buttonAttributes (Just <| RequestSelectedDesignData ProteinDesign.Json) (text "Download JSON")
        , if Set.member designId shared.designsToDownload then
            downloadButton widthButton buttonAttributes (Just RemoveFromDownloadList) (text "Remove from download")

          else
            downloadButton widthButton buttonAttributes (Just AddToDownloadList) (text "Add to download list")
        ]


designDetailsHeader : String -> String -> ProteinDesign -> Int -> Element msg
designDetailsHeader title path { previous_design, next_design } screenWidth =
    row
        [ width (fill |> maximum screenWidth) ]
        [ row
            [ centerX
            , spacing 30
            , paddingXY 0 30
            ]
            [ link
                []
                { url = path ++ previous_design
                , label =
                    el []
                        (html <|
                            FeatherIcons.toHtml [ HAtt.align "center" ] <|
                                FeatherIcons.withSize 36 <|
                                    FeatherIcons.withStrokeWidth 1.2 <|
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
                    el []
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 36 <|
                                    FeatherIcons.withStrokeWidth 1.2 <|
                                        FeatherIcons.arrowRightCircle
                        )
                }
            ]
        ]


designDetailsBodyTable : ProteinDesign -> Int -> Element msg
designDetailsBodyTable proteinDesign screenWidth =
    let
        limitingScreenSize =
            900

        elPadding =
            40

        tableWidth =
            if screenWidth < limitingScreenSize then
                screenWidth - elPadding

            else
                (screenWidth * 2) // 3

        pictureWidth =
            if screenWidth < limitingScreenSize then
                screenWidth - elPadding

            else
                screenWidth - tableWidth - elPadding

        elementType =
            if screenWidth < limitingScreenSize then
                column

            else
                row
    in
    elementType
        [ width fill
        , spacing 10
        , Font.justify
        ]
        [ table
            [ width <| fillPortion 2
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
                  , width = px (tableWidth // 5)
                  , view =
                        \category ->
                            paragraph
                                Style.monospacedFont
                                [ column
                                    [ width (px (tableWidth // 5 - 20))
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
                  , width = px (tableWidth * 4 // 5)
                  , view =
                        \detail ->
                            paragraph
                                Style.monospacedFont
                                [ column
                                    [ width (px (tableWidth * 4 // 5))
                                    , height fill
                                    , scrollbarX
                                    , paddingXY 10 10
                                    ]
                                    [ detail.property ]
                                ]
                  }
                ]
            }
        , el
            [ padding 2
            , Border.width 2
            , Border.color <| rgb255 220 220 220
            , Border.rounded 3
            , alignTop
            , centerX
            , width (fillPortion 1 |> maximum pictureWidth)
            ]
            (image
                [ width (fill |> minimum 200) ]
                { src = proteinDesign.picture_path
                , description = "Structure of " ++ proteinDesign.pdb
                }
            )
        ]


designDetailsBodyStructure : ProteinDesign -> Int -> Int -> Element msg
designDetailsBodyStructure proteinDesign screenWidth screenHeight =
    let
        picHeight =
            screenHeight // 2
    in
    column
        [ width fill
        , spacing 20
        ]
        [ column
            [ width (fill |> maximum screenWidth)
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
                [ width (fill |> maximum screenWidth)
                , height <| px picHeight
                ]
                ( proteinDesign.pdb
                , Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" (String.fromInt screenWidth ++ "px")
                    , HAtt.style "height" (String.fromInt picHeight ++ "px")
                    , HAtt.style "align" "center"
                    , HAtt.alt "3D structure"
                    , HAtt.attribute "pdb-string" proteinDesign.pdb
                    ]
                    []
                    |> html
                )
            ]
        ]


designDetailsBodySequence : ProteinDesign -> Int -> Element msg
designDetailsBodySequence proteinDesign screenWidth =
    let
        elPadding =
            10

        narrowColumnWidth =
            screenWidth // 10 - elPadding

        wideColumnWidth =
            screenWidth - 4 * (narrowColumnWidth + elPadding)
    in
    column [ spacing 20 ]
        [ paragraph
            Style.h2Font
            [ text "Sequence"
            ]
        , table
            [ padding 2
            , width fill
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
                  , width = px narrowColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum narrowColumnWidth)
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
                  , width = px narrowColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum narrowColumnWidth)
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
                  , width = px narrowColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum narrowColumnWidth)
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
                  , width = px wideColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum wideColumnWidth)
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
                  , width = px narrowColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum narrowColumnWidth)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text <| String.fromInt chain.chain_length ]
                  }
                ]
            }
        ]


designDetailsBodyParagraphs : ProteinDesign -> Int -> Element msg
designDetailsBodyParagraphs proteinDesign screenWidth =
    column
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
                   , width (fill |> maximum screenWidth)
                   ]
            )
            [ proteinDesign.abstract
                |> text
            ]
        , paragraph
            Style.h2Font
            [ text "Curation comments"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width (fill |> maximum screenWidth)
                   , spacing 10
                   ]
            )
            (if proteinDesign.review_comment == [ "" ] then
                [ text "" ]

             else
                List.map
                    (\comment ->
                        wrappedRow
                            [ width fill
                            ]
                            [ row
                                [ width fill ]
                                [ text <| comment ]
                            ]
                    )
                    proteinDesign.review_comment
            )
        ]


designDetailsBody : ProteinDesign -> Int -> Int -> Element msg
designDetailsBody proteinDesign screenWidth screenHeight =
    column
        (Style.bodyFont
            ++ [ width (fill |> maximum screenWidth)
               , paddingXY 30 20
               , spacing 30
               , centerX
               ]
        )
        [ designDetailsBodyTable proteinDesign screenWidth
        , designDetailsBodyStructure proteinDesign screenWidth screenHeight
        , designDetailsBodySequence proteinDesign screenWidth
        , designDetailsBodyParagraphs proteinDesign screenWidth
        ]
