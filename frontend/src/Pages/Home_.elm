module Pages.Home_ exposing (Model, Msg, page)

import AppError exposing (AppError(..))
import Browser.Dom
import Browser.Events
import Components.Title
import Date
import DesignFilter
    exposing
        ( DesignFilter(..)
        , defaultEndDate
        , defaultKeys
        , defaultStartDate
        , removeHyphenFromIsoDate
        )
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input as Input
import FeatherIcons
import Get exposing (getScreenWidthFloat, getScreenWidthInt)
import Http
import Json.Decode
import Page exposing (Page)
import Plots exposing (RenderPlotState(..))
import ProteinDesign exposing (DownloadFileType(..), ProteinDesignStub, csvStringFromProteinDesignDownload, downloadDesignDecoder, jsonStringFromProteinDesignDownload)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Set
import Shared
import Shared.Msg exposing (Msg(..))
import Style
import Task
import Time
import Urls
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = \() -> init shared.mScreenWidthF shared.mScreenHeightF
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared >> Components.Title.view shared.mScreenWidthF
        }



-- INIT


type alias Model =
    { designStubs : RemoteData Http.Error (Dict String ProteinDesignStub)
    , errors : List AppError
    , designFilters : Dict String DesignFilter
    , designFiltersCached : Dict String DesignFilter
    , mStartDate : Maybe String
    , mEndDate : Maybe String
    , replotTime : Int
    , renderPlotState : RenderPlotState
    , mScreenWidthF : Maybe Float
    , mScreenHeightF : Maybe Float
    , dataDownload : RemoteData Http.Error String
    , searchString : String
    , sliderSimilaritySequenceValue : Float
    , sliderSimilarityStructureValue : Float
    }


init : Maybe Float -> Maybe Float -> ( Model, Effect Msg )
init mSharedScreenWidthF mSharedScreenHeightF =
    ( { designStubs = Loading
      , errors = []
      , designFilters = Dict.empty
      , designFiltersCached = Dict.empty
      , mStartDate = Nothing
      , mEndDate = Nothing
      , replotTime = 3
      , renderPlotState = WillRender
      , mScreenWidthF = mSharedScreenWidthF
      , mScreenHeightF = mSharedScreenHeightF
      , dataDownload = NotAsked
      , searchString = ""
      , sliderSimilaritySequenceValue = 1000.0
      , sliderSimilarityStructureValue = 100.0
      }
    , Effect.batch
        [ Effect.sendCmd (Task.attempt ViewportResult Browser.Dom.getViewport)
        , Effect.resetViewport ViewportReset
        , Effect.sendCmd (getData Urls.allDesignStubs)
        ]
    )


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect =
            Http.expectJson DesignsDataReceived (Json.Decode.list ProteinDesign.rawDesignStubDecoder)
        }



-- UPDATE


type Msg
    = UpdateFilters String DesignFilter
    | UpdateStartDateTextField String
    | UpdateEndDateTextField String
    | UpdateSimilaritySlider String Float
    | SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error (List ProteinDesignStub))
    | AddAllSelected
    | RemoveAllSelected
    | DownloadAllSelectedCsv
    | DownloadAllSelectedJson
    | RequestSelectedDesignData DownloadFileType
    | ForExportResponse DownloadFileType (Result Http.Error String)
    | RenderWhenReady Time.Posix
    | WindowResizes Int Int
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ViewportReset


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case model.designStubs of
        RemoteData.NotAsked ->
            case msg of
                SendDesignsHttpRequest ->
                    ( { model | designStubs = Loading }, Effect.sendCmd (getData Urls.allDesignStubs) )

                _ ->
                    ( model, Effect.none )

        RemoteData.Loading ->
            case msg of
                DesignsDataReceived (Ok rawDesignStubs) ->
                    let
                        designs =
                            rawDesignStubs
                                |> List.map (\d -> ( d.pdb, d ))
                                |> Dict.fromList

                        filteredDesignStubs =
                            designs
                                |> Dict.values

                        plotWidth =
                            getScreenWidthFloat model.mScreenWidthF

                        plotHeight =
                            getScreenWidthFloat model.mScreenHeightF
                    in
                    ( { model | designStubs = Success designs }
                    , Effect.renderVegaPlot (Plots.timelinePlotStubs plotWidth plotHeight filteredDesignStubs)
                    )

                DesignsDataReceived (Err e) ->
                    ( { model
                        | designStubs = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

                WindowResizes width height ->
                    let
                        widthF =
                            toFloat width

                        heightF =
                            toFloat height
                    in
                    ( { model | mScreenWidthF = Just widthF, mScreenHeightF = Just heightF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width, mScreenHeightF = Just viewport.viewport.height }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        RemoteData.Failure e ->
            case msg of
                _ ->
                    ( { model | designStubs = Failure e, errors = DesignRequestFailed :: model.errors }, Effect.none )

        RemoteData.Success loadedDesignStubs ->
            case msg of
                UpdateFilters key newFilter ->
                    let
                        newDesignFilters =
                            Dict.insert key newFilter model.designFiltersCached
                    in
                    case newFilter of
                        ContainsTextParsed string ->
                            ( { model
                                | designFiltersCached = newDesignFilters
                                , renderPlotState = AwaitingRender model.replotTime
                                , searchString = string
                              }
                            , Effect.none
                            )

                        _ ->
                            ( { model
                                | designFiltersCached = newDesignFilters
                                , renderPlotState = AwaitingRender model.replotTime
                              }
                            , Effect.none
                            )

                UpdateStartDateTextField string ->
                    let
                        phrase =
                            removeHyphenFromIsoDate string
                    in
                    case Date.fromIsoString phrase of
                        Err _ ->
                            update shared
                                (UpdateFilters defaultKeys.dateStartKey (DateStart defaultStartDate))
                                { model | mStartDate = ifEmptyOrNot string }

                        Ok date ->
                            update shared
                                (UpdateFilters defaultKeys.dateStartKey (DateStart date))
                                { model | mStartDate = ifEmptyOrNot string }

                UpdateEndDateTextField string ->
                    let
                        phrase =
                            removeHyphenFromIsoDate string
                    in
                    case Date.fromIsoString phrase of
                        Err _ ->
                            update shared
                                (UpdateFilters defaultKeys.dateEndKey (DateEnd defaultEndDate))
                                { model | mEndDate = ifEmptyOrNot string }

                        Ok date ->
                            update shared
                                (UpdateFilters defaultKeys.dateEndKey (DateEnd date))
                                { model | mEndDate = ifEmptyOrNot string }

                UpdateSimilaritySlider key threshold ->
                    case key of
                        "similarity-sequence-bit" ->
                            update shared
                                (UpdateFilters key (SimilaritySequence threshold))
                                { model | sliderSimilaritySequenceValue = threshold, renderPlotState = AwaitingRender model.replotTime }

                        "similarity-structure-lddt" ->
                            update shared
                                (UpdateFilters key (SimilarityStructure threshold))
                                { model | sliderSimilarityStructureValue = threshold, renderPlotState = AwaitingRender model.replotTime }

                        _ ->
                            update shared msg model

                AddAllSelected ->
                    let
                        filteredDesignStubs =
                            loadedDesignStubs
                                |> Dict.values
                                |> List.filterMap (DesignFilter.stubMeetsAllFilters (Dict.values model.designFilters))
                                |> List.map (\x -> x.pdb)
                    in
                    ( model, Effect.addDesignsToDownload filteredDesignStubs )

                RemoveAllSelected ->
                    let
                        filteredDesignStubs =
                            loadedDesignStubs
                                |> Dict.values
                                |> List.map (\x -> x.pdb)
                    in
                    ( model, Effect.removeDesignsFromDownload filteredDesignStubs )

                RequestSelectedDesignData fileType ->
                    ( { model | dataDownload = Loading }
                    , Http.get
                        { url = Urls.downloadSelectedDesigns (shared.designsToDownload |> Set.toList)
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
                                            designData

                                ProteinDesign.Csv ->
                                    case Json.Decode.decodeString (Json.Decode.list downloadDesignDecoder) designData of
                                        Ok designs ->
                                            csvStringFromProteinDesignDownload designs

                                        Err _ ->
                                            designData
                    in
                    ( { model | dataDownload = NotAsked }
                    , Effect.downloadFile "exported_designs" encodedFileContent fileType
                    )

                RenderWhenReady _ ->
                    let
                        filteredDesignStubs =
                            loadedDesignStubs
                                |> Dict.values
                                |> List.filterMap (DesignFilter.stubMeetsAllFilters (Dict.values model.designFiltersCached))

                        plotWidth =
                            getScreenWidthFloat model.mScreenWidthF

                        plotHeight =
                            getScreenWidthFloat model.mScreenHeightF
                    in
                    case model.renderPlotState of
                        AwaitingRender 0 ->
                            ( { model | renderPlotState = Rendered, designFilters = model.designFiltersCached }
                            , Effect.renderVegaPlot (Plots.timelinePlotStubs plotWidth plotHeight filteredDesignStubs)
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
                    ( { model | mScreenWidthF = Just widthF, mScreenHeightF = Just heightF }, Effect.batch [ Effect.resetViewport ViewportReset, Effect.resizeShared (getScreenWidthInt model.mScreenWidthF) (getScreenWidthInt model.mScreenHeightF) ] )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width, mScreenHeightF = Just viewport.viewport.height }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                ViewportReset ->
                    ( { model | renderPlotState = AwaitingRender model.replotTime }, Effect.resizeShared (getScreenWidthInt model.mScreenWidthF) (getScreenWidthInt model.mScreenHeightF) )

                _ ->
                    ( model, Effect.none )


ifEmptyOrNot : String -> Maybe String
ifEmptyOrNot string =
    if String.isEmpty string then
        Nothing

    else
        Just string



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.renderPlotState of
        AwaitingRender _ ->
            Time.every 100 RenderWhenReady

        _ ->
            Browser.Events.onResize (\width height -> WindowResizes width height)



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Protein Design Archive"
    , attributes =
        [ centerX
        , width (fill |> minimum (getScreenWidthInt model.mScreenWidthF))
        ]
    , element = homeView shared model
    }


homeView : Shared.Model -> Model -> Element Msg
homeView shared model =
    let
        screenWidth =
            getScreenWidthInt model.mScreenWidthF

        screenHeight =
            getScreenWidthInt model.mScreenHeightF - 130
    in
    case model.designStubs of
        RemoteData.NotAsked ->
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
                            FeatherIcons.withSize 100 <|
                                FeatherIcons.loader
                    )
                , paragraph [ center, Font.size 24, padding 50 ] [ text "Not asked for data..." ]
                ]

        RemoteData.Loading ->
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
                , paragraph [ center, Font.size 24, padding 50, centerY ] [ text "Loading..." ]
                ]

        RemoteData.Failure _ ->
            text "Failed to load data, probably couldn't connect to server."

        RemoteData.Success designStubs ->
            let
                designsToDisplay =
                    designStubs
                        |> Dict.values
                        |> List.filterMap
                            (DesignFilter.stubMeetsAllFilters (Dict.values model.designFilters))

                widthDesignCard =
                    if screenWidth < 800 then
                        Element.fill |> maximum (screenWidth - 10)

                    else
                        Element.px 390
            in
            column [ centerX ]
                [ Plots.timelinePlotView model.mScreenWidthF
                , column
                    [ paddingXY 20 0, spacing 15, width (fill |> maximum screenWidth) ]
                    [ downloadArea model
                    , searchArea model
                    , dateSearchArea model
                    , similarityFilteringArea model
                    , numberArea model.mScreenWidthF designsToDisplay shared.designsToDownload
                    , designList widthDesignCard designsToDisplay
                    ]
                ]


designList : Length -> List ProteinDesignStub -> Element msg
designList widthDesignCard designs =
    wrappedRow
        [ spaceEvenly
        , centerX
        ]
        (List.map (ProteinDesign.designCard widthDesignCard) designs)


downloadArea : Model -> Element Msg
downloadArea model =
    let
        screenWidth =
            getScreenWidthInt model.mScreenWidthF

        widthButton =
            if screenWidth < 1200 then
                Element.fill |> maximum (screenWidth - 10)

            else
                Element.px 300

        buttonAttributes =
            if screenWidth < 1200 then
                [ Border.widthEach { bottom = 1, top = 1, left = 0, right = 0 }
                , Border.color <| rgb255 220 220 220
                ]

            else
                [ centerX
                , Font.center
                ]

        elementType =
            if screenWidth < 1200 then
                column

            else
                row
    in
    elementType
        (Style.bodyFont
            ++ [ width (fill |> maximum screenWidth)
               , Font.bold
               , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
               , Border.color <| rgb255 220 220 220
               ]
        )
        [ downloadButton widthButton buttonAttributes (Just AddAllSelected) (text "Add displayed")
        , downloadButton widthButton buttonAttributes (Just <| RequestSelectedDesignData ProteinDesign.Csv) (text "Download as CSV")
        , downloadButton widthButton buttonAttributes (Just <| RequestSelectedDesignData ProteinDesign.Json) (text "Download as JSON")
        , downloadButton widthButton buttonAttributes (Just RemoveAllSelected) (text "Clear download list")
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


searchArea : Model -> Element Msg
searchArea model =
    row
        [ width <|
            Element.px (getScreenWidthInt model.mScreenWidthF - 50)
        ]
        [ searchIcon
        , searchInput model
        ]


searchIcon : Element Msg
searchIcon =
    el [ centerX, alignTop, padding 10 ]
        (html <|
            FeatherIcons.toHtml [] <|
                FeatherIcons.withSize 24 <|
                    FeatherIcons.search
        )


searchInput : Model -> Element Msg
searchInput model =
    Input.text
        (Style.monospacedFont
            ++ [ width <| fillPortion 6 ]
        )
        { onChange = \string -> UpdateFilters defaultKeys.searchTextParsedKey (ContainsTextParsed string)
        , text = model.searchString
        , placeholder =
            Just <|
                Input.placeholder []
                    (text "Enter search phrase here e.g. Woolfson && coiled-coil || coiled coil &&!! 4-helix")
        , label = Input.labelHidden "Filter Designs Search Box"
        }


dateSearchArea : Model -> Element Msg
dateSearchArea model =
    let
        screenWidth =
            getScreenWidthInt model.mScreenWidthF

        elementType =
            if screenWidth < 1200 then
                column

            else
                row
    in
    elementType
        (Style.monospacedFont ++ [ centerX, spacing 10, alignLeft ])
        [ row []
            [ el [ centerX, paddingXY 10 0 ]
                (html <|
                    FeatherIcons.toHtml [] <|
                        FeatherIcons.withSize 24 <|
                            FeatherIcons.calendar
                )
            , text "Type to show designs released"
            ]
        , dateStartField model
        , dateEndField model
        ]


similarityFilteringArea : Model -> Element Msg
similarityFilteringArea model =
    let
        screenWidth =
            getScreenWidthInt model.mScreenWidthF

        elementType =
            if screenWidth < 1000 then
                column

            else
                row
    in
    column
        (Style.monospacedFont ++ [ centerX, spacing 10, alignLeft ])
        [ row []
            [ el [ centerX, paddingXY 10 0 ]
                (html <|
                    FeatherIcons.toHtml [] <|
                        FeatherIcons.withSize 24 <|
                            FeatherIcons.crosshair
                )
            , text "Slide to set similarity threshold: "
            ]
        , elementType []
            [ sequenceSimilarityField model
            , similarityExclusionButton model defaultKeys.similaritySequenceExclusionKey DesignFilter.SimilaritySequenceExclusion
            ]
        , elementType []
            [ structureSimilarityField model
            , similarityExclusionButton model defaultKeys.similarityStructureExclusionKey DesignFilter.SimilarityStructureExclusion
            ]
        ]


similarityExclusionButton : Model -> String -> (Bool -> DesignFilter) -> Element Msg
similarityExclusionButton model dictKey filter =
    Input.checkbox [ paddingXY 3 10, alignTop ]
        { onChange =
            \checkboxStatus ->
                UpdateFilters dictKey (filter checkboxStatus)
        , icon = Input.defaultCheckbox
        , checked =
            case Dict.get dictKey model.designFiltersCached of
                Just (SimilaritySequenceExclusion value) ->
                    value

                Just (SimilarityStructureExclusion value) ->
                    value

                _ ->
                    False
        , label =
            let
                label =
                    case Dict.get dictKey model.designFiltersCached of
                        Just (SimilaritySequenceExclusion value) ->
                            if value then
                                "uncomputed excluded"

                            else
                                "exclude uncomputed"

                        Just (SimilarityStructureExclusion value) ->
                            if value then
                                "uncomputed excluded"

                            else
                                "exclude uncomputed"

                        _ ->
                            "exclude uncomputed"
            in
            Input.labelRight
                [ centerY
                , width fill
                , paddingXY 5 0
                , Font.italic
                ]
                (paragraph
                    Style.monospacedFont
                    [ text <| label ]
                )
        }


numberArea : Maybe Float -> List ProteinDesignStub -> Set.Set String -> Element Msg
numberArea mScreenWidthF designsToDisplay designsToDownload =
    let
        screenWidth =
            getScreenWidthInt mScreenWidthF

        widthButton =
            if screenWidth < 900 then
                Element.fill |> maximum (screenWidth - 10)

            else
                Element.px 300

        elementType =
            if screenWidth < 900 then
                column

            else
                row
    in
    elementType
        (Style.bodyFont
            ++ [ width widthButton
               , Border.color <| rgb255 220 220 220
               , spacing 10
               ]
        )
        [ numberDisplayedArea designsToDisplay
        , numberSavedToDownloadArea designsToDownload
        ]


numberDisplayedArea :
    List ProteinDesignStub
    -> Element Msg
numberDisplayedArea designs =
    let
        numberOfDesigns =
            List.length designs
    in
    row (Style.monospacedFont ++ [ alignLeft ])
        [ el [ centerX, paddingXY 10 0 ]
            (html <|
                FeatherIcons.toHtml [] <|
                    FeatherIcons.withSize 24 <|
                        FeatherIcons.hash
             -- or barChart
            )
        , text ("Number of designs displayed: " ++ String.fromInt numberOfDesigns)
        ]


numberSavedToDownloadArea :
    Set.Set String
    -> Element Msg
numberSavedToDownloadArea designs =
    let
        numberOfDesigns =
            Set.size designs
    in
    row (Style.monospacedFont ++ [ alignLeft ])
        [ el [ centerX, paddingXY 10 0 ]
            (html <|
                FeatherIcons.toHtml [] <|
                    FeatherIcons.withSize 24 <|
                        FeatherIcons.download
             -- or barChart
            )
        , text ("Number of designs saved for download: " ++ String.fromInt numberOfDesigns)
        ]


dateStartField : Model -> Element Msg
dateStartField model =
    row [ alignLeft, width fill ]
        [ text " after: "
        , el [ paddingXY 5 0 ]
            (Input.text
                [ width <| px 150
                , Background.color <|
                    case model.mStartDate of
                        Nothing ->
                            rgb255 255 255 255

                        Just "" ->
                            rgb255 255 255 255

                        Just string ->
                            case Date.fromIsoString <| string of
                                Ok startDate ->
                                    case Date.fromIsoString <| Maybe.withDefault "" model.mEndDate of
                                        Ok endDate ->
                                            if Date.compare endDate startDate == GT then
                                                rgb255 223 255 214

                                            else
                                                rgb255 255 215 213

                                        Err _ ->
                                            rgb255 223 255 214

                                Err _ ->
                                    rgb255 255 215 213
                ]
                { onChange =
                    \string ->
                        UpdateStartDateTextField string
                , text = Maybe.withDefault "" model.mStartDate
                , placeholder = Just <| Input.placeholder [] (text "YYYY-MM-DD")
                , label = Input.labelHidden "Filter Designs by Date - start"
                }
            )
        ]


dateEndField : Model -> Element Msg
dateEndField model =
    row [ alignLeft, width fill ]
        [ text " before: "
        , el [ paddingXY 5 0 ]
            (Input.text
                [ width <| px 150
                , Background.color <|
                    case model.mEndDate of
                        Nothing ->
                            rgb255 255 255 255

                        Just "" ->
                            rgb255 255 255 255

                        Just string ->
                            case Date.fromIsoString <| string of
                                Ok endDate ->
                                    case Date.fromIsoString <| Maybe.withDefault "" model.mStartDate of
                                        Ok startDate ->
                                            if Date.compare endDate startDate == GT then
                                                rgb255 223 255 214

                                            else
                                                rgb255 255 215 213

                                        Err _ ->
                                            rgb255 223 255 214

                                Err _ ->
                                    rgb255 255 215 213
                ]
                { onChange =
                    \string ->
                        UpdateEndDateTextField string
                , text = Maybe.withDefault "" model.mEndDate
                , placeholder = Just <| Input.placeholder [] (text "YYYY-MM-DD")
                , label = Input.labelHidden "Filter Designs by Date - end"
                }
            )
        ]


sequenceSimilarityField : Model -> Element Msg
sequenceSimilarityField model =
    let
        stringScore =
            String.fromInt <| round model.sliderSimilaritySequenceValue

        scoreToShow =
            if String.length stringScore < 4 then
                " " ++ stringScore

            else
                stringScore
    in
    row
        [ paddingXY 5 0
        , alignLeft
        , width fill
        ]
        [ Input.slider
            [ paddingXY 5 0
            , width <| px 200
            , Border.rounded 5
            , Border.widthEach { bottom = 1, top = 1, left = 1, right = 1 }
            , Border.color <| rgb255 220 220 220
            , Background.gradient
                { angle = pi / 2
                , steps =
                    [ rgb255 255 255 255
                    , rgb255 68 129 187
                    ]
                }
            ]
            { onChange =
                \threshold ->
                    UpdateSimilaritySlider defaultKeys.similaritySequenceKey threshold
            , label = Input.labelHidden "Filter Designs by Similarity - sequence"
            , min = 0.0
            , max = 1000.0
            , value = model.sliderSimilaritySequenceValue
            , thumb = Input.defaultThumb
            , step = Just 5.0
            }
        , text <| " " ++ scoreToShow
        , text " bit score (sequence)"
        ]


structureSimilarityField : Model -> Element Msg
structureSimilarityField model =
    let
        stringScore =
            String.fromInt <| round model.sliderSimilarityStructureValue

        scoreToShow =
            if String.length stringScore < 3 then
                " " ++ stringScore

            else
                stringScore
    in
    row
        [ paddingXY 5 0
        , alignLeft
        , width fill
        ]
        [ Input.slider
            [ width <| px 200
            , Border.rounded 5
            , Border.widthEach { bottom = 1, top = 1, left = 1, right = 1 }
            , Border.color <| rgb255 200 200 200
            , Background.gradient
                { angle = pi / 2
                , steps =
                    [ rgb255 255 255 255
                    , rgb255 67 162 87
                    ]
                }
            ]
            { onChange =
                \threshold ->
                    UpdateSimilaritySlider defaultKeys.similarityStructureKey threshold
            , label = Input.labelHidden "Filter Designs by Similarity - structure"
            , min = 0.0
            , max = 100.0
            , value = model.sliderSimilarityStructureValue
            , thumb = Input.defaultThumb
            , step = Nothing
            }
        , text <| " " ++ scoreToShow
        , text "% LDDT (structure)"
        ]
