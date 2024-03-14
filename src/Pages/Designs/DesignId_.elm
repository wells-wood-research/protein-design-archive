module Pages.Designs.DesignId_ exposing (page)

import Html exposing (Html)
import View exposing (View)


page : { designId : String } -> View msg
page params =
    View.fromString <| "Pages.Designs.DesignId_" ++ params.designId
