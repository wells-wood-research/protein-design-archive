module Pages.Home_ exposing (Model, Msg, page)

import AppError exposing (AppError(..))
import Components.Title
import Effect exposing (Effect)
import Element exposing (..)
import Http
import Json.Decode
import Page exposing (Page)
import ProteinDesign exposing (ProteinDesign)
import RawDesignData exposing (RawDesignData)
import Route exposing (Route)
import Shared
import Shared.Msg exposing (Msg(..))
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
    { designs : List ProteinDesign
    , errors : List AppError
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { designs = [], errors = [] }
    , Effect.sendCmd getData
    )



-- UPDATE


type Msg
    = DesignsDataReceived (Result Http.Error (List RawDesignData))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        DesignsDataReceived (Ok rawDesigns) ->
            let
                designs =
                    List.filterMap RawDesignData.toProteinDesign rawDesigns
            in
            ( { model | designs = designs }
            , Effect.none
            )

        DesignsDataReceived (Err _) ->
            ( { model | errors = DesignRequestFailed :: model.errors }
            , Effect.none
            )


getData : Cmd Msg
getData =
    Http.get
        { url = "/designs.json"
        , expect = Http.expectJson DesignsDataReceived (Json.Decode.list RawDesignData.rawDesignDecoder)
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Protein Design Archive"
    , attributes = [ padding 10, width fill ]
    , element = designList model.designs
    }


designList : List ProteinDesign -> Element msg
designList designs =
    column
        [ spacing 5
        , width fill
        ]
        (List.map ProteinDesign.designCard designs)
