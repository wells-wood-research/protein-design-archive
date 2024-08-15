port module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute
    , pushRoutePath, replaceRoutePath
    , loadExternalUrl, back
    , map, toCmd
    , addDesignsToDownload, downloadFile, removeDesignsFromDownload, renderVegaPlot, resetViewport
    )

{-|

@docs Effect

@docs none, batch
@docs sendCmd, sendMsg

@docs pushRoute, replaceRoute
@docs pushRoutePath, replaceRoutePath
@docs loadExternalUrl, back

@docs map, toCmd

-}

import Browser.Dom as Dom
import Browser.Navigation
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import Plots exposing (PlotData)
import ProteinDesign exposing (DownloadFile)
import Route
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Url exposing (Url)



-- Ports


port vegaPlotCmd : PlotData -> Cmd msg


type Effect msg
    = -- BASICS
      None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
    | LoadExternalUrl String
    | Back
      -- DOM
    | ResetViewport msg
      -- DOWNLOAD
    | AddDesignsToDownload (List String)
    | RemoveDesignsFromDownload (List String)
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg
      -- PORTS
    | RenderVegaPlot PlotData



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send multiple effects at once.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd


{-| Send a message as an effect. Useful when emitting events from UI components.
-}
sendMsg : msg -> Effect msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
        |> SendCmd



-- ROUTING


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Same as `Effect.pushRoute`, but without `query` or `hash` support
-}
pushRoutePath : Route.Path.Path -> Effect msg
pushRoutePath path =
    PushUrl (Route.Path.toString path)


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)


{-| Same as `Effect.replaceRoute`, but without `query` or `hash` support
-}
replaceRoutePath : Route.Path.Path -> Effect msg
replaceRoutePath path =
    ReplaceUrl (Route.Path.toString path)


{-| Redirect users to a new URL, somewhere external your web application.
-}
loadExternalUrl : String -> Effect msg
loadExternalUrl =
    LoadExternalUrl


{-| Navigate back one page
-}
back : Effect msg
back =
    Back



-- DOM


resetViewport : msg -> Effect msg
resetViewport msg =
    ResetViewport msg



-- DOWNLOAD


addDesignsToDownload : List String -> Effect msg
addDesignsToDownload designIds =
    AddDesignsToDownload designIds


removeDesignsFromDownload : List String -> Effect msg
removeDesignsFromDownload designIds =
    RemoveDesignsFromDownload designIds


downloadFile : String -> String -> DownloadFile -> Effect msg
downloadFile fileName fileContent fileType =
    case fileType of
        ProteinDesign.Json ->
            sendCmd (Download.string ("pda_" ++ fileName ++ ".json") "application/json" fileContent)

        ProteinDesign.Csv ->
            sendCmd (Download.string ("pda_" ++ fileName ++ ".csv") "text/csv" fileContent)



-- Ports


renderVegaPlot : PlotData -> Effect msg
renderVegaPlot plotData =
    RenderVegaPlot plotData



-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        Back ->
            Back

        ResetViewport msg ->
            ResetViewport <| fn msg

        AddDesignsToDownload designIds ->
            AddDesignsToDownload designIds

        RemoveDesignsFromDownload designIds ->
            RemoveDesignsFromDownload designIds

        LoadExternalUrl url ->
            LoadExternalUrl url

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg

        RenderVegaPlot plotData ->
            RenderVegaPlot plotData


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        Back ->
            Browser.Navigation.back options.key 1

        LoadExternalUrl url ->
            Browser.Navigation.load url

        ResetViewport msg ->
            Task.perform (\_ -> msg) (Dom.setViewport 0 0)

        AddDesignsToDownload designIds ->
            Task.succeed (Shared.Msg.AddDesignsToDownload designIds)
                |> Task.perform options.fromSharedMsg

        RemoveDesignsFromDownload designIds ->
            Task.succeed (Shared.Msg.RemoveDesignsFromDownload designIds)
                |> Task.perform options.fromSharedMsg

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg

        RenderVegaPlot plotData ->
            vegaPlotCmd plotData
