module Pages.Home_ exposing (Model, Msg, page)

import Components.Title
import DesignFilter exposing (DesignFilter(..), defaultKeys)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
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
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    if Dict.isEmpty shared.designs then
        ( { waitingForData = True
          , designFilters = Dict.empty
          }
        , Effect.batch
            [ Effect.resetViewport NoOp
            ]
        )

    else
        ( { waitingForData = False
          , designFilters = Dict.empty
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
    = UpdateSearchString String
    | CheckForData Time.Posix
    | NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        UpdateSearchString s ->
            let
                newDesignFilters =
                    Dict.insert defaultKeys.searchTextKey (ContainsText s) model.designFilters
            in
            ( { model
                | designFilters = newDesignFilters
              }
            , shared.designs
                |> Dict.values
                |> List.filterMap (DesignFilter.meetsAllFilters (Dict.values newDesignFilters))
                |> Plots.timelinePlotData
                |> Effect.renderVegaPlot
            )

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
        , Input.text []
            { onChange = UpdateSearchString
            , text =
                Dict.get defaultKeys.searchTextKey model.designFilters
                    |> Maybe.map DesignFilter.toString
                    |> Maybe.withDefault ""
            , placeholder = Just <| Input.placeholder [] (text "Enter search term...")
            , label = Input.labelHidden "Search box"
            }
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
