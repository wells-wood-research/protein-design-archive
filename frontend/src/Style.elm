module Style exposing (..)

import Element exposing (..)
import Element.Font as Font


titleFont : List (Attribute msg)
titleFont =
    [ Font.family
        [ Font.typeface "Barlow"
        , Font.sansSerif
        ]
    , Font.size 28
    ]


h1Font : List (Attribute msg)
h1Font =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 24
    ]


h2Font : List (Attribute msg)
h2Font =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 24
    ]


h3Font : List (Attribute msg)
h3Font =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 20
    ]


bodyFont : List (Attribute msg)
bodyFont =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 16
    , Font.alignLeft
    ]


monospacedFont : List (Attribute msg)
monospacedFont =
    [ Font.family
        [ Font.typeface "Fira Code"
        , Font.sansSerif
        ]
    , Font.size 16
    ]
