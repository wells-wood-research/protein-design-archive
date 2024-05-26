module Pages.Home_ exposing (Model, Msg, page)

import AppError exposing (AppError(..))
import Browser
import Browser.Dom
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
import Http
import Json.Decode
import Page exposing (Page)
import Plots
import ProteinDesign exposing (ProteinDesignStub)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Shared.Msg exposing (Msg(..))
import Task
import Time
import Urls
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
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
    , screenWidth : Maybe Float
    , screenHeight : Maybe Float
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { designStubs = Loading
      , errors = []
      , designFilters = Dict.empty
      , mStartDate = Nothing
      , mEndDate = Nothing
      , screenWidth = Nothing
      , screenHeight = Nothing
      }
    , Effect.batch
        [ Effect.resetViewport ViewportReset
        , Effect.sendCmd (Task.attempt ViewportResult Browser.Dom.getViewport)
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

                        width =
                            getScreenWidthFloat model
                    in
                    ( { model | designStubs = Success designs }
                    , Effect.renderVegaPlot (Plots.timelinePlotStubs width filteredDesignStubs)
                    )

                DesignsDataReceived (Err e) ->
                    ( { model
                        | designStubs = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            let
                                width =
                                    viewport.viewport.width

                                height =
                                    viewport.viewport.height
                            in
                            ( { model | screenWidth = Just width, screenHeight = Just height }, Effect.none )

                        Err _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        RemoteData.Failure e ->
            case msg of
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

                        width =
                            getScreenWidthFloat model
                    in
                    ( { model | designFilters = newDesignFilters }
                    , Effect.renderVegaPlot (Plots.timelinePlotStubs width filteredDesignStubs)
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Protein Design Archive"
    , attributes = [ padding 10, centerX ]
    , element = homeView model
    }


getScreenWidthInt : Model -> Int
getScreenWidthInt model =
    Maybe.withDefault 800 <| String.toInt <| String.fromFloat <| Maybe.withDefault 800.0 model.screenWidth


getScreenWidthFloat : Model -> Float
getScreenWidthFloat model =
    Maybe.withDefault 800.0 model.screenWidth


getScreenWidthString : Model -> String
getScreenWidthString model =
    (String.fromFloat <| Maybe.withDefault 800.0 model.screenWidth) ++ "px"


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
            column
                [ spacing 10, width fill ]
                [ --growthCurve
                  Plots.timelinePlotView (px (getScreenWidthInt model)) (getScreenWidthString model)
                , row [ width fill ]
                    [ searchArea model
                    , dateSearchArea model
                    ]
                , designStubs
                    |> Dict.values
                    |> List.filterMap
                        (DesignFilter.stubMeetsAllFilters (Dict.values model.designFilters))
                    |> designList
                ]


designList : List ProteinDesignStub -> Element msg
designList designs =
    wrappedRow
        [ spacing 5
        , width fill
        ]
        (List.map ProteinDesign.designCard designs)


searchArea : Model -> Element Msg
searchArea model =
    Input.text
        [ width <| fillPortion 6 ]
        { onChange = \string -> UpdateFilters defaultKeys.searchTextKey (ContainsText string)
        , text =
            Dict.get defaultKeys.searchTextKey model.designFilters
                |> Maybe.map DesignFilter.toString
                |> Maybe.withDefault ""
        , placeholder = Just <| Input.placeholder [] (text "Enter search phrase here")
        , label = Input.labelHidden "Filter Designs Search Box"
        }


dateSearchArea : Model -> Element Msg
dateSearchArea model =
    row [ width <| fillPortion 4 ]
        [ dateStartField model
        , dateEndField model
        ]


dateStartField : Model -> Element Msg
dateStartField model =
    row [ width fill ]
        [ paragraph [ width fill, center, padding 5 ] [ text "from" ]
        , Input.text
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
        ]


dateEndField : Model -> Element Msg
dateEndField model =
    row [ width fill, spaceEvenly ]
        [ paragraph [ width fill, center, padding 5 ] [ text "to" ]
        , Input.text
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
        ]
