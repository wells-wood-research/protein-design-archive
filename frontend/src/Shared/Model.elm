module Shared.Model exposing (Model)

{-| -}

import AppError exposing (AppError)
import Dict exposing (Dict)
import Element exposing (..)
import Http
import ProteinDesign exposing (ProteinDesign)
import RemoteData exposing (RemoteData)
import Set exposing (Set)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { designs : RemoteData Http.Error (Dict String ProteinDesign)
    , errors : List AppError
    , designsToDownload : Set String
    , mScreenWidthF : Maybe Float
    , mScreenHeightF : Maybe Float
    }
