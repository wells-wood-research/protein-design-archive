module Components.Title exposing (view)

import Element exposing (..)
import Element.Background as Background
import Style
import View exposing (View)


view : View msg -> View msg
view props =
    { title = props.title
    , attributes = []
    , element =
        column
            [ centerX, width (fill |> maximum 960) ]
            [ title
            , el props.attributes props.element
            ]
    }


title : Element msg
title =
    el
        (Style.titleFont
            ++ [ width fill
               , padding 20
               , Background.color <| rgb255 143 192 169
               ]
        )
    <|
        link [ centerX ]
            { label = paragraph [] [ text "Protein Design Archive" ]
            , url = "/"
            }
