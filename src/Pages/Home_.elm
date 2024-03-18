module Pages.Home_ exposing (Model, Msg, page)

import Components.Title
import DesignFilter exposing (DesignFilter(..), defaultKeys)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Page exposing (Page)
import ProteinDesign exposing (ProteinDesign)
import Route exposing (Route)
import Shared
import Shared.Msg exposing (Msg(..))
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared >> Components.Title.view
        }



-- INIT


type alias Model =
    { designFilters : Dict String DesignFilter }


init : () -> ( Model, Effect Msg )
init _ =
    ( { designFilters = Dict.empty }
    , Effect.resetViewport NoOp
    )



-- UPDATE


type Msg
    = UpdateSearchString String
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateSearchString s ->
            case s of
                "" ->
                    ( { model
                        | designFilters =
                            Dict.remove defaultKeys.searchTextKey model.designFilters
                      }
                    , Effect.none
                    )

                _ ->
                    ( { model
                        | designFilters =
                            Dict.insert defaultKeys.searchTextKey (ContainsText s) model.designFilters
                      }
                    , Effect.none
                    )

        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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


homeView : Model -> List ProteinDesign -> Element Msg
homeView model designs =
    column
        [ spacing 10, width fill ]
        [ Input.text []
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
    column
        [ spacing 5
        , width fill
        ]
        (List.map ProteinDesign.designCard designs)
