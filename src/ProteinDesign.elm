module ProteinDesign exposing (..)

import Date exposing (Date, Unit(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


type alias ProteinDesign =
    { pdbCode : String
    , structuralKeywords : Keyword
    , depositionDate : Date
    , picturePath : String
    , doiLink : String
    , sequences : List String
    , classification : Classification
    , authors : String
    , pubmedID : Int
    , abstract : String
    }


searchableText : ProteinDesign -> String
searchableText design =
    [ design.pdbCode
    , keywordToString design.structuralKeywords
    , design.doiLink
    , String.join " " design.sequences
    , classificationToString design.classification
    , design.authors
    , design.abstract
    ]
        |> String.join "\n"
        |> String.toLower


type Classification
    = OriginalDeNovo
    | RelativeDeNovo
    | Small
    | Engineered
    | Unknown


stringToClassfication : String -> Classification
stringToClassfication string =
    case string of
        "original de novo design" ->
            OriginalDeNovo

        "relative of another de novo design" ->
            RelativeDeNovo

        "small, non-systematic, and other" ->
            Small

        "engineered" ->
            Engineered

        _ ->
            Unknown


classificationToString : Classification -> String
classificationToString classification =
    case classification of
        OriginalDeNovo ->
            "Original De Novo"

        RelativeDeNovo ->
            "Relative De Novo"

        Small ->
            "Small, Non-Systematic, Other"

        Engineered ->
            "Engineered"

        Unknown ->
            "Unknown"


classificationToColour : Classification -> String
classificationToColour classification =
    case classification of
        OriginalDeNovo ->
            "#ff0000"

        RelativeDeNovo ->
            "#00ff00"

        Small ->
            "#ffffff"

        Engineered ->
            "#0000ff"

        Unknown ->
            "#333333"


type Keyword
    = Synthetic
    | DeNovo
    | Novel
    | Designed
    | ProteinBinding
    | MetalBinding
    | Transcription
    | Growth
    | Structural
    | AlphaHelicalBundle
    | BetaBetaAlpha
    | CoiledCoil
    | UnknownFunction


stringToKeyword : String -> Keyword
stringToKeyword string =
    case string of
        "synthetic protein model" ->
            Synthetic

        "de novo protein" ->
            DeNovo

        "de novo protein design" ->
            DeNovo

        "novel sequence" ->
            Novel

        "designed peptide" ->
            Designed

        "protein binding" ->
            ProteinBinding

        "metal binding protein" ->
            MetalBinding

        "transcription" ->
            Transcription

        "growth response protein" ->
            Growth

        "structural protein" ->
            Structural

        "unknown function" ->
            UnknownFunction

        "alpha-helical bundle" ->
            AlphaHelicalBundle

        "beta beta alpha motif" ->
            BetaBetaAlpha

        "coiled coil" ->
            CoiledCoil

        _ ->
            UnknownFunction


keywordToString : Keyword -> String
keywordToString keyword =
    case keyword of
        Synthetic ->
            "synthetic"

        DeNovo ->
            "de novo"

        Novel ->
            "novel sequence"

        Designed ->
            "designed peptide"

        ProteinBinding ->
            "protein binding"

        MetalBinding ->
            "metal binding"

        Transcription ->
            "transcription"

        Growth ->
            "growth response"

        Structural ->
            "structural"

        UnknownFunction ->
            "unknown"

        AlphaHelicalBundle ->
            "alpha-helical bundle"

        BetaBetaAlpha ->
            "beta-beta-alpha motif"

        CoiledCoil ->
            "coiled-coil"



-- {{{ Views


{-| A simple view that shows basic data about a design. Used for lists etc.
-}
designCard : ProteinDesign -> Element msg
designCard design =
    link
        [ width <| px 460
        , clip
        ]
        { url = "/designs/" ++ design.pdbCode
        , label =
            row
                [ width fill
                , spacing 4
                , padding 4
                , mouseOver [ Background.color <| rgb255 235 235 235 ]
                ]
                [ el
                    [ Border.width 2
                    , Border.color <| rgb255 220 220 220
                    ]
                    (image
                        [ width <| px 50
                        ]
                        { src = design.picturePath, description = "Image of design " ++ design.pdbCode }
                    )
                , column
                    [ padding 2, spacing 2, width (fill |> minimum 200), alignTop ]
                    [ paragraph [ Font.size 16 ] [ text <| String.toUpper <| design.pdbCode ]
                    , paragraph [ Font.color <| rgb255 130 130 130, Font.size 11 ] [ text design.authors ]
                    ]
                ]
        }



-- }}}
