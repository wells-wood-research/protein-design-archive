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
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Get exposing (getScreenWidthFloat, getScreenWidthInt, getScreenWidthString)
import Http
import Json.Decode
import Page exposing (Page)
import Plots exposing (RenderPlotState(..))
import ProteinDesign exposing (ProteinDesignStub)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
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
        { init = \() -> init shared.mScreenWidthF
        , update = update
        , subscriptions = subscriptions
        , view = view >> Components.Title.view
        }



-- INIT


type alias Model =
    { designStubs : RemoteData Http.Error (Dict String ProteinDesignStub)
    , errors : List AppError
    , designFilters : Dict String DesignFilter
    , mStartDate : Maybe String
    , mEndDate : Maybe String
    , replotTime : Int
    , renderPlotState : RenderPlotState
    , mScreenWidthF : Maybe Float
    , searchString : String
    }


init : Maybe Float -> ( Model, Effect Msg )
init mSharedScreenWidthF =
    ( { designStubs = Loading
      , errors = []
      , designFilters = Dict.empty
      , mStartDate = Nothing
      , mEndDate = Nothing
      , replotTime = 3
      , renderPlotState = WillRender
      , mScreenWidthF = mSharedScreenWidthF
      , searchString = ""
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
    | SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error (List ProteinDesignStub))
    | RenderWhenReady Time.Posix
    | WindowResizes Int Int
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ViewportReset


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
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
                    in
                    ( { model | designStubs = Success designs }
                    , Effect.renderVegaPlot (Plots.timelinePlotStubs plotWidth filteredDesignStubs)
                    )

                DesignsDataReceived (Err e) ->
                    ( { model
                        | designStubs = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mScreenWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

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
                            Dict.insert key newFilter model.designFilters
                    in
                    case newFilter of
                        ContainsTextParsed string ->
                            ( { model
                                | designFilters = newDesignFilters
                                , renderPlotState = AwaitingRender model.replotTime
                                , searchString = string
                              }
                            , Effect.none
                            )

                        _ ->
                            ( { model
                                | designFilters = newDesignFilters
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
                            update
                                (UpdateFilters defaultKeys.dateStartKey (DateStart defaultStartDate))
                                { model | mStartDate = ifEmptyOrNot string }

                        Ok date ->
                            update
                                (UpdateFilters defaultKeys.dateStartKey (DateStart date))
                                { model | mStartDate = ifEmptyOrNot string }

                UpdateEndDateTextField string ->
                    let
                        phrase =
                            removeHyphenFromIsoDate string
                    in
                    case Date.fromIsoString phrase of
                        Err _ ->
                            update
                                (UpdateFilters defaultKeys.dateEndKey (DateEnd defaultEndDate))
                                { model | mEndDate = ifEmptyOrNot string }

                        Ok date ->
                            update
                                (UpdateFilters defaultKeys.dateEndKey (DateEnd date))
                                { model | mEndDate = ifEmptyOrNot string }

                RenderWhenReady _ ->
                    let
                        filteredDesignStubs =
                            loadedDesignStubs
                                |> Dict.values
                                |> List.filterMap (DesignFilter.stubMeetsAllFilters (Dict.values model.designFilters))

                        plotWidth =
                            getScreenWidthFloat model.mScreenWidthF
                    in
                    case model.renderPlotState of
                        AwaitingRender 0 ->
                            ( { model | renderPlotState = Rendered }
                            , Effect.renderVegaPlot (Plots.timelinePlotStubs plotWidth filteredDesignStubs)
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
                    ( { model | mScreenWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                ViewportReset ->
                    ( { model | renderPlotState = AwaitingRender model.replotTime }, Effect.none )

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


view : Model -> View Msg
view model =
    { title = "Protein Design Archive"
    , attributes =
        [ centerX
        , width (fill |> minimum (getScreenWidthInt model.mScreenWidthF))
        ]
    , element = homeView model
    }


homeView : Model -> Element Msg
homeView model =
    case model.designStubs of
        RemoteData.NotAsked ->
            text "Not asked for data..."

        RemoteData.Loading ->
            text "Requested data..."

        RemoteData.Failure _ ->
            text "Failed to load data, probably couldn't connect to server."

        RemoteData.Success designStubs ->
            let
                designsToDisplay =
                    designStubs
                        |> Dict.values
                        |> List.filterMap
                            (DesignFilter.stubMeetsAllFilters (Dict.values model.designFilters))

                screenWidth =
                    getScreenWidthInt model.mScreenWidthF

                screenWidthS =
                    getScreenWidthString model.mScreenWidthF

                widthDesignCard =
                    if screenWidth < 800 then
                        Element.fill |> maximum (screenWidth - 10)

                    else
                        Element.px 390
            in
            column [ centerX ]
                [ Plots.timelinePlotView (px screenWidth) screenWidthS
                , column
                    [ paddingXY 20 0, spacing 15, width (fill |> maximum screenWidth) ]
                    [ searchArea model
                    , dateSearchArea model
                    , designList widthDesignCard designsToDisplay
                    ]
                ]


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
        [ width <| fillPortion 6 ]
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
    case model.mScreenWidthF of
        Just widthF ->
            if widthF <= 900 then
                column (Style.monospacedFont ++ [ centerX, spacing 10, width fill ])
                    [ row []
                        [ el [ centerX, paddingXY 10 0 ]
                            (html <|
                                FeatherIcons.toHtml [] <|
                                    FeatherIcons.withSize 24 <|
                                        FeatherIcons.calendar
                            )
                        , text "Type to show designs released "
                        ]
                    , dateStartField model
                    , dateEndField model
                    ]

            else
                row (Style.monospacedFont ++ [ alignLeft, spaceEvenly ])
                    [ el [ centerX, paddingXY 10 0 ]
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 24 <|
                                    FeatherIcons.calendar
                        )
                    , text "Type to show designs released "
                    , dateStartField model
                    , dateEndField model
                    ]

        _ ->
            row (Style.monospacedFont ++ [ alignLeft ])
                [ el [ centerX, paddingXY 10 0 ]
                    (html <|
                        FeatherIcons.toHtml [] <|
                            FeatherIcons.withSize 24 <|
                                FeatherIcons.calendar
                    )
                , text "Type to show designs released "
                , dateStartField model
                , dateEndField model
                ]


dateStartField : Model -> Element Msg
dateStartField model =
    row [ alignLeft, width fill ]
        [ text "after "
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
        [ text "before"
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


designList : Length -> List ProteinDesignStub -> Element msg
designList widthDesignCard designs =
    wrappedRow
        [ spaceEvenly
        , centerX
        ]
        (List.map (ProteinDesign.designCard widthDesignCard) designs)
