module ProteinDesign exposing (..)

import Date exposing (Date, Unit(..))


type alias ProteinDesign =
    { pdbCode : String
    , structuralKeywords : String
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
    , design.structuralKeywords
    , design.doiLink
    , String.join " " design.sequences
    , classificationToString design.classification
    , design.authors
    , design.abstract
    ]
        |> String.join "\n"


type Classification
    = OriginalDeNovo
    | RelativeDeNovo
    | Small
    | Engineered
    | Unknown


rawStringToClassfication : String -> Classification
rawStringToClassfication string =
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
