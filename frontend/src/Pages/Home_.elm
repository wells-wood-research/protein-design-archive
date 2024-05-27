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
import Element.Font exposing (center)
import Element.Input as Input
import FeatherIcons
import Get exposing (getScreenWidthFloat, getScreenWidthInt, getScreenWidthString)
import Html.Attributes as HAtt
import Http
import Json.Decode
import Page exposing (Page)
import Plots
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
        { init = \() -> init shared.mScreenWidth
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
    , mWidthF : Maybe Float
    , widthS : String
    , widthI : Int
    , widthF : Float
    }


init : Maybe Float -> ( Model, Effect Msg )
init mScreenWidthF =
    ( { designStubs = Loading
      , errors = []
      , designFilters = Dict.empty
      , mStartDate = Nothing
      , mEndDate = Nothing
      , mWidthF = mScreenWidthF
      , widthS = "800"
      , widthI = 800
      , widthF = 800.0
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
    | CheckForData Time.Posix
    | SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error (List ProteinDesignStub))
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

                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                ViewportReset ->
                    ( { model
                        | widthS = getScreenWidthString model.mWidthF
                        , widthI = getScreenWidthInt model.mWidthF
                        , widthF = getScreenWidthFloat model.mWidthF
                      }
                    , Effect.none
                    )

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
                    in
                    ( { model | designStubs = Success designs }
                    , Effect.renderVegaPlot (Plots.timelinePlotStubs model.widthF filteredDesignStubs)
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
                    ( { model | mWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                ViewportReset ->
                    ( { model
                        | widthS = getScreenWidthString model.mWidthF
                        , widthI = getScreenWidthInt model.mWidthF
                        , widthF = getScreenWidthFloat model.mWidthF
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        RemoteData.Failure e ->
            case msg of
                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                ViewportReset ->
                    ( { model
                        | widthS = getScreenWidthString model.mWidthF
                        , widthI = getScreenWidthInt model.mWidthF
                        , widthF = getScreenWidthFloat model.mWidthF
                      }
                    , Effect.none
                    )

                _ ->
                    ( { model
                        | designStubs = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

        RemoteData.Success loadedDesignStubs ->
            case msg of
                UpdateFilters key newFilter ->
                    let
                        newDesignFilters =
                            Dict.insert key newFilter model.designFilters

                        filteredDesignStubs =
                            loadedDesignStubs
                                |> Dict.values
                                |> List.filterMap (DesignFilter.stubMeetsAllFilters (Dict.values newDesignFilters))
                    in
                    ( { model | designFilters = newDesignFilters }
                    , Effect.renderVegaPlot (Plots.timelinePlotStubs model.widthF filteredDesignStubs)
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

                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                ViewportReset ->
                    let
                        filteredDesignStubs =
                            loadedDesignStubs
                                |> Dict.values
                                |> List.filterMap (DesignFilter.stubMeetsAllFilters (Dict.values model.designFilters))
                    in
                    ( { model
                        | widthS = getScreenWidthString model.mWidthF
                        , widthI = getScreenWidthInt model.mWidthF
                        , widthF = getScreenWidthFloat model.mWidthF
                      }
                    , Effect.renderVegaPlot (Plots.timelinePlotStubs model.widthF filteredDesignStubs)
                    )

                CheckForData _ ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )


ifEmptyOrNot : String -> Maybe String
ifEmptyOrNot string =
    if String.isEmpty string then
        Nothing

    else
        Just string



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\width height -> WindowResizes width height)



-- VIEW


view : Model -> View Msg
view model =
    { title = "Protein Design Archive"
    , attributes = [ centerX, width (fill |> minimum model.widthI) ]
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

                widthDesignCard =
                    if model.widthI < 800 then
                        Element.fill |> maximum (model.widthI - 10)

                    else
                        Element.px 390
            in
            column [ centerX ]
                [ Plots.timelinePlotView (px model.widthI) model.widthS
                , column
                    [ paddingXY 20 0, spacing 10, width (fill |> maximum model.widthI) ]
                    [ searchArea model
                    , dateSearchArea model
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


searchArea : Model -> Element Msg
searchArea model =
    row [ width fill, spaceEvenly ]
        [ el [ centerX, paddingXY 10 0 ]
            (html <|
                FeatherIcons.toHtml [] <|
                    FeatherIcons.withSize 24 <|
                        FeatherIcons.search
            )
        , Input.text
            (Style.monospacedFont ++ [ width fill, centerX ])
            { onChange = \string -> UpdateFilters defaultKeys.searchTextKey (ContainsText string)
            , text =
                Dict.get defaultKeys.searchTextKey model.designFilters
                    |> Maybe.map DesignFilter.toString
                    |> Maybe.withDefault ""
            , placeholder = Just <| Input.placeholder [] (text "Enter search phrase here")
            , label = Input.labelHidden "Filter Designs Search Box"
            }
        ]



-- calendar
-- chevronsRight
-- fastForward


dateSearchArea : Model -> Element Msg
dateSearchArea model =
    case model.mWidthF of
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
