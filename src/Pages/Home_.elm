module Pages.Home_ exposing (Model, Msg, page)

import Components.Title
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Http
import Page exposing (Page)
import ProteinDesign exposing (ProteinDesign)
import RawDesignData exposing (RawDesignData)
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
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.resetViewport NoOp
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    { title = "Protein Design Archive"
    , attributes = [ padding 10, width fill ]
    , element = designList <| Dict.values shared.designs
    }


designList : List ProteinDesign -> Element msg
designList designs =
    column
        [ spacing 5
        , width fill
        ]
        (List.map ProteinDesign.designCard designs)
