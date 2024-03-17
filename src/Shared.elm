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

import Dict
import Effect exposing (Effect)
import Http
import Json.Decode
import RawDesignData exposing (RawDesignData)
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg exposing (Msg(..))



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
    ( { designs = Dict.empty
      , errors = []
      }
    , Effect.sendCmd <| getData
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.DesignsDataReceived (Ok rawDesigns) ->
            let
                designs =
                    List.filterMap RawDesignData.toProteinDesign rawDesigns
                        |> List.map (\d -> ( d.pdbCode, d ))
                        |> Dict.fromList
            in
            ( { model | designs = designs }
            , Effect.none
            )

        Shared.Msg.DesignsDataReceived (Err err) ->
            let
                _ =
                    Debug.log "Error" err
            in
            ( model
            , Effect.none
            )


getData : Cmd Msg
getData =
    Http.get
        { url = "/designs.json"
        , expect = Http.expectJson DesignsDataReceived (Json.Decode.list RawDesignData.rawDesignDecoder)
        }



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
