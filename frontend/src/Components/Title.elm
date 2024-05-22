module Components.Title exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Style
import View exposing (View)


view : View msg -> View msg
view props =
    { title = props.title
    , attributes = [ Background.color (Element.rgb255 250 250 250) ]
    , element =
        column
            [ centerX
            , width (fill |> maximum 960)
            ]
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
               , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
               , Border.color <| rgb255 220 220 220
               ]
        )
    <|
        link [ centerX ]
            { label = paragraph [] [ text "Protein Design Archive" ]
            , url = "/pda/"
            }
