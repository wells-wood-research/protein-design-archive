module DesignFilter exposing (..)

import Date
import Dict exposing (Dict)
import List exposing (filter)
import ProteinDesign exposing (Classification(..), ProteinDesign, Tag(..), classificationToString, searchableText, stringToClassification, stringToTag, tagToString)


type DesignFilter
    = ContainsText String
    | DateStart Date.Date
    | DateEnd Date.Date
    | DesignClass Classification
    | KeyTag Tag


defaultKeys :
    { dateStartKey : String
    , dateEndKey : String
    , searchTextKey : String
    , classificationOriginalDeNovoKey : String
    , classificationRelativeDeNovoKey : String
    , classificationSmallKey : String
    , classificationEngineeredKey : String
    , classificationUnknownKey : String
    , keywordSyntheticKey : String
    , keywordDeNovoKey : String
    , keywordNovelKey : String
    , keywordDesignedKey : String
    , keywordProteinBindingKey : String
    , keywordMetalBindingKey : String
    , keywordTranscriptionKey : String
    , keywordGrowthKey : String
    , keywordStructuralKey : String
    , keywordAlphaHelicalBundleKey : String
    , keywordBetaBetaAlphaKey : String
    , keywordCoiledCoilKey : String
    , keywordUnknownFunctionKey : String
    }
defaultKeys =
    { dateStartKey = "deposition-date-start"
    , dateEndKey = "deposition-date-end"
    , searchTextKey = "search-text-string"
    , classificationOriginalDeNovoKey = "design-classification-original"
    , classificationRelativeDeNovoKey = "design-classification-relative"
    , classificationSmallKey = "design-classification-small"
    , classificationEngineeredKey = "design-classification-engineered"
    , classificationUnknownKey = "design-classification-unknown"
    , keywordSyntheticKey = "design-keyword-synthetic"
    , keywordDeNovoKey = "design-keyword-de-novo"
    , keywordNovelKey = "design-keyword-novel"
    , keywordDesignedKey = "design-keyword-designed"
    , keywordProteinBindingKey = "design-keyword-protein-binding"
    , keywordMetalBindingKey = "design-keyword-metal-binding"
    , keywordTranscriptionKey = "design-keyword-transcription"
    , keywordGrowthKey = "design-keyword-growth"
    , keywordStructuralKey = "design-keyword-structural"
    , keywordAlphaHelicalBundleKey = "design-keyword-alpha-helical-bundle"
    , keywordBetaBetaAlphaKey = "design-keyword-beta-beta-alpha"
    , keywordCoiledCoilKey = "design-keyword-coiled-coil"
    , keywordUnknownFunctionKey = "design-keyword-unknown"
    }


checkboxDict : Dict String Bool
checkboxDict =
    Dict.fromList
        [ ( defaultKeys.classificationOriginalDeNovoKey, False )
        , ( defaultKeys.classificationRelativeDeNovoKey, False )
        , ( defaultKeys.classificationSmallKey, False )
        , ( defaultKeys.classificationEngineeredKey, False )
        , ( defaultKeys.classificationUnknownKey, False )
        , ( defaultKeys.keywordSyntheticKey, False )
        , ( defaultKeys.keywordDeNovoKey, False )
        , ( defaultKeys.keywordNovelKey, False )
        , ( defaultKeys.keywordDesignedKey, False )
        , ( defaultKeys.keywordProteinBindingKey, False )
        , ( defaultKeys.keywordMetalBindingKey, False )
        , ( defaultKeys.keywordTranscriptionKey, False )
        , ( defaultKeys.keywordGrowthKey, False )
        , ( defaultKeys.keywordStructuralKey, False )
        , ( defaultKeys.keywordAlphaHelicalBundleKey, False )
        , ( defaultKeys.keywordBetaBetaAlphaKey, False )
        , ( defaultKeys.keywordCoiledCoilKey, False )
        , ( defaultKeys.keywordUnknownFunctionKey, False )
        ]


toString : DesignFilter -> String
toString filter =
    case filter of
        ContainsText string ->
            string

        DateStart startDate ->
            Date.toIsoString startDate

        DateEnd endDate ->
            Date.toIsoString endDate

        DesignClass classification ->
            classificationToString classification

        KeyTag tag ->
            tagToString tag


toDesignFilter : String -> DesignFilter
toDesignFilter key =
    case key of
        "design-classification-original" ->
            DesignClass OriginalDeNovo

        "design-classification-relative" ->
            DesignClass RelativeDeNovo

        "design-classification-small" ->
            DesignClass Small

        "design-classification-engineered" ->
            DesignClass Engineered

        "design-classification-unknown" ->
            DesignClass Unknown

        "design-keyword-synthetic" ->
            KeyTag Synthetic

        "design-keyword-de-novo" ->
            KeyTag DeNovo

        "design-keyword-novel" ->
            KeyTag Novel

        "design-keyword-designed" ->
            KeyTag Designed

        "design-keyword-protein-binding" ->
            KeyTag ProteinBinding

        "design-keyword-metal-binding" ->
            KeyTag MetalBinding

        "design-keyword-transcription" ->
            KeyTag Transcription

        "design-keyword-growth" ->
            KeyTag Growth

        "design-keyword-structural" ->
            KeyTag Structural

        "design-keyword-alpha-helical-bundle" ->
            KeyTag AlphaHelicalBundle

        "design-keyword-beta-beta-alpha" ->
            KeyTag BetaBetaAlpha

        "design-keyword-coiled-coil" ->
            KeyTag CoiledCoil

        "design-keyword-unknown" ->
            KeyTag UnknownFunction

        _ ->
            ContainsText ""


keyToLabel : String -> String
keyToLabel label =
    toString <| toDesignFilter label


meetsAllFilters : List DesignFilter -> ProteinDesign -> Maybe ProteinDesign
meetsAllFilters filters design =
    List.all (\f -> meetsOneFilter design f) filters
        |> (\allFiltersMet ->
                if allFiltersMet then
                    Just design

                else
                    Nothing
           )


meetsOneFilter : ProteinDesign -> DesignFilter -> Bool
meetsOneFilter design filter =
    case filter of
        ContainsText searchString ->
            design
                |> searchableText
                |> String.contains (String.toLower searchString)

        DateStart startDate ->
            if Date.compare startDate design.release_date == LT then
                True

            else
                False

        DateEnd endDate ->
            if Date.compare endDate design.release_date == GT then
                True

            else
                False

        DesignClass classification ->
            classification == stringToClassification design.classification

        KeyTag tag ->
            List.member tag (List.map stringToTag design.tags)
