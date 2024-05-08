module Pages.Home_ exposing (Model, Msg, page)

import Components.Title
import Date
import DesignFilter exposing (DesignFilter(..), dateToPosition, defaultEndDate, defaultKeys, defaultStartDate, getFirstAndLastDate, isValidIsoDate, removeHyphenFromIsoDate)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Page exposing (Page)
import Plots
import ProteinDesign exposing (ProteinDesign)
import Route exposing (Route)
import Shared
import Shared.Msg exposing (Msg(..))
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared >> Components.Title.view
        }



-- INIT


type alias Model =
    { waitingForData : Bool
    , designFilters : Dict String DesignFilter
    , mStartDate : Maybe String
    , mEndDate : Maybe String
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    if Dict.isEmpty shared.designs then
        ( { waitingForData = True
          , designFilters = Dict.empty
          , mStartDate = Nothing
          , mEndDate = Nothing
          }
        , Effect.batch
            [ Effect.resetViewport NoOp
            ]
        )

    else
        ( { waitingForData = False
          , designFilters = Dict.empty
          , mStartDate = Just ""
          , mEndDate = Just ""
          }
        , Effect.batch
            [ Effect.resetViewport NoOp
            , shared.designs
                |> Dict.values
                |> Plots.timelinePlotData
                |> Effect.renderVegaPlot
            ]
        )



-- UPDATE


type Msg
    = UpdateFilters String DesignFilter
    | UpdateStartDateTextField String
    | UpdateEndDateTextField String
    | CheckForData Time.Posix
    | NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        UpdateFilters key newFilter ->
            let
                newDesignFilters =
                    Dict.insert key newFilter model.designFilters
            in
            ( { model | designFilters = newDesignFilters }
            , shared.designs
                |> Dict.values
                |> List.filterMap (DesignFilter.meetsAllFilters (Dict.values newDesignFilters))
                |> Plots.timelinePlotData
                |> Effect.renderVegaPlot
            )

        UpdateStartDateTextField string ->
            let
                phrase =
                    removeHyphenFromIsoDate string
            in
            case Date.fromIsoString phrase of
                Err _ ->
                    update
                        shared
                        (UpdateFilters defaultKeys.dateStartKey (DateStart defaultStartDate))
                        { model | mStartDate = ifEmptyOrNot string }

                Ok date ->
                    update
                        shared
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
                        shared
                        (UpdateFilters defaultKeys.dateEndKey (DateEnd defaultEndDate))
                        { model | mEndDate = ifEmptyOrNot string }

                Ok date ->
                    update
                        shared
                        (UpdateFilters defaultKeys.dateEndKey (DateEnd date))
                        { model | mEndDate = ifEmptyOrNot string }

        CheckForData _ ->
            if Dict.isEmpty shared.designs then
                ( model, Effect.none )

            else
                ( { model | waitingForData = False }
                , shared.designs
                    |> Dict.values
                    |> List.filterMap (DesignFilter.meetsAllFilters (Dict.values model.designFilters))
                    |> Plots.timelinePlotData
                    |> Effect.renderVegaPlot
                )

        NoOp ->
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
    if model.waitingForData then
        Time.every 200 CheckForData

    else
        Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Protein Design Archive"
    , attributes = [ padding 10, width fill ]
    , element =
        Dict.values shared.designs
            |> homeView model
    }


growthCurve : Element Msg
growthCurve =
    image
        [ width (fill |> maximum 780), centerX ]
        { src = "/growth_curve.png"
        , description = "Histogram showing increase in number of de novo protein designs published from 1990 to 2026"
        }


homeView : Model -> List ProteinDesign -> Element Msg
homeView model designs =
    column
        [ spacing 10, width fill ]
        [ --growthCurve
          Plots.timelinePlotView
        , row [ spacing 5, width fill ]
            [ searchArea model
            , dateStartField model
            , dateEndField model
            ]
        , designs
            |> List.filterMap (DesignFilter.meetsAllFilters (Dict.values model.designFilters))
            |> designList
        ]


designList : List ProteinDesign -> Element msg
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


dateStartField : Model -> Element Msg
dateStartField model =
    row [ width <| fillPortion 2 ]
        [ paragraph [ width <| fillPortion 1, padding 3 ] [ text "from" ]
        , Input.text
            [ width <| fillPortion 9
            , Background.color <|
                case model.mStartDate of
                    Nothing ->
                        rgb255 255 255 255

                    Just "" ->
                        rgb255 255 255 255

                    Just string ->
                        if isValidIsoDate string then
                            rgb255 223 255 214

                        else
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
    row [ width <| fillPortion 2 ]
        [ paragraph [ width <| fillPortion 1, padding 3 ] [ text "to" ]
        , Input.text
            [ width <| fillPortion 9
            , Background.color <|
                case model.mEndDate of
                    Nothing ->
                        rgb255 255 255 255

                    Just "" ->
                        rgb255 255 255 255

                    Just string ->
                        if isValidIsoDate string then
                            rgb255 223 255 214

                        else
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
