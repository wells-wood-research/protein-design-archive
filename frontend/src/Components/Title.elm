module Components.Title exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import FeatherIcons
import Style
import View exposing (View)


view : View msg -> View msg
view props =
    { title = props.title
    , attributes =
        [ Background.color <|
            Element.rgb255 250 250 250
        ]
            ++ props.attributes
    , element =
        column
            props.attributes
            [ title
            , el props.attributes props.element
            , footerArea
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
            , url = "/"
            }


footerArea : Element msg
footerArea =
    row
        (Style.monospacedFont
            ++ [ width fill
               , spaceEvenly
               , Background.color <| rgb255 220 220 220
               ]
        )
        [ row [ width <| fillPortion 1, centerX ]
            [ newTabLink [ width fill, centerX ]
                { url = "mailto:chris.wood@ed.ac.uk"
                , label =
                    row []
                        [ el [ centerX, alignTop, padding 10 ]
                            (html <|
                                FeatherIcons.toHtml [] <|
                                    FeatherIcons.withSize 24 <|
                                        FeatherIcons.send
                            )
                        , text "Email us feedback!"
                        ]
                }
            ]
        , row [ width <| fillPortion 1, centerX ]
            [ newTabLink [ width fill ]
                { url = "https://www.wellswoodresearchgroup.com/"
                , label =
                    row []
                        [ el [ centerX, alignTop, padding 10 ]
                            (Element.image
                                [ width <| px 30, padding 5, Border.color <| rgb255 64 64 64, Border.width 2, Border.roundEach { bottomLeft = 100, bottomRight = 50, topLeft = 100, topRight = 50 } ]
                                { src = "/wells-wood-logo.svg"
                                , description = "Wells Wood Research Group icon"
                                }
                            )
                        , text "Find out more about us"
                        ]
                }
            ]
        , row [ width <| fillPortion 1, centerX ] [ text <| "Date od last update: " ++ "2024-09-04" ]
        ]
