module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import AppError exposing (AppError(..))
import Dict
import Effect exposing (Effect)
import Http
import Json.Decode
import ProteinDesign
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared.Model
import Shared.Msg as Msg exposing (Msg(..))



-- FLAGS


type alias Flags =
    {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed {}



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init _ _ =
    ( { designs = NotAsked, errors = [] }
    , Effect.sendCmd getData
    )



-- UPDATE


type alias Msg =
    Msg.Msg


getData : Cmd Msg
getData =
    Http.get
        { url = "http://localhost:5000/all-design-stubs"
        , expect =
            Http.expectJson DesignsDataReceived
                (Json.Decode.list ProteinDesign.rawDesignDecoder)
        }


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        DesignsDataReceived (Ok rawDesigns) ->
            let
                designs =
                    List.filterMap ProteinDesign.toProteinDesign rawDesigns
                        |> List.map (\d -> ( d.pdb, d ))
                        |> Dict.fromList
            in
            ( { model | designs = Success designs }
            , Effect.none
            )

        DesignsDataReceived (Err e) ->
            let
                _ =
                    Debug.log "error" e
            in
            ( { model
                | designs = Failure e
                , errors = DesignRequestFailed :: model.errors
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
