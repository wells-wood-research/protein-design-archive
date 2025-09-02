module Shared.Msg exposing (Msg(..))

import Browser.Dom
import Dict exposing (Dict)
import Http
import ProteinDesign exposing (ProteinDesign, ProteinDesignStub)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = DesignsDataReceived (Result Http.Error (List ProteinDesign))
    | SaveDesignStubs (Dict String ProteinDesignStub)
    | AddDesignsToDownload (List String)
    | RemoveDesignsFromDownload (List String)
    | WindowResizes Int Int
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ViewportReset
