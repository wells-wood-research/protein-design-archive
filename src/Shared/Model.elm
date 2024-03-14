module Shared.Model exposing (Model)

{-| -}

import Dict exposing (Dict)
import ProteinDesign exposing (ProteinDesign)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { designs : Dict String ProteinDesign }
