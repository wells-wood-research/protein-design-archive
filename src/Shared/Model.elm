module Shared.Model exposing (Model)

{-| -}

import Dict exposing (Dict)
import Element exposing (..)
import ProteinDesign exposing (ProteinDesign)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { designs : Dict String ProteinDesign
    , errors : List AppError
    }


type AppError
    = DesignRequestFailed


appErrorToAdvice : AppError -> Element msg
appErrorToAdvice appError =
    case appError of
        DesignRequestFailed ->
            paragraph []
                [ """Failed to download designs from server. Try refreshing page."""
                    |> text
                ]
