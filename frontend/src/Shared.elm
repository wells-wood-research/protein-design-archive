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
import Browser.Dom
import Dict
import Effect exposing (Effect)
import Json.Decode
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Set
import Shared.Model
import Shared.Msg as Msg exposing (Msg(..))
import Task



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
    ( { designs = NotAsked, errors = [], designsToDownload = Set.empty, mScreenWidthF = Nothing, mScreenHeightF = Nothing }
    , Effect.sendCmd (Task.attempt ViewportResult Browser.Dom.getViewport)
    )



-- UPDATE


type alias Msg =
    Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        DesignsDataReceived (Ok rawDesigns) ->
            let
                designs =
                    rawDesigns
                        |> List.map (\d -> ( d.pdb, d ))
                        |> Dict.fromList
            in
            ( { model | designs = Success designs }
            , Effect.none
            )

        DesignsDataReceived (Err e) ->
            ( { model
                | designs = Failure e
                , errors = DesignRequestFailed :: model.errors
              }
            , Effect.none
            )

        AddDesignsToDownload designIds ->
            let
                updatedDesignsToDownload =
                    Set.union model.designsToDownload (Set.fromList designIds)
            in
            ( { model | designsToDownload = updatedDesignsToDownload }
            , Effect.none
            )

        RemoveDesignsFromDownload designIds ->
            let
                updatedDesignsToDownload =
                    Set.diff model.designsToDownload (Set.fromList designIds)
            in
            ( { model | designsToDownload = updatedDesignsToDownload }
            , Effect.none
            )

        ViewportResult result ->
            case result of
                Ok viewport ->
                    ( { model | mScreenWidthF = Just viewport.viewport.width, mScreenHeightF = Just viewport.viewport.height }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        WindowResizes width height ->
            let
                widthF =
                    toFloat width

                heightF =
                    toFloat height
            in
            ( { model | mScreenWidthF = Just widthF, mScreenHeightF = Just heightF }, Effect.resetViewport ViewportReset )

        ViewportReset ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
