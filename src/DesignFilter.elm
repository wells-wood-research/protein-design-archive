module DesignFilter exposing (..)

import Date
import Dict exposing (Dict)
import List exposing (filter)
import ProteinDesign exposing (Classification(..), ProteinDesign, Tag(..), classificationToString, searchableText, tagToString)


type DesignFilter
    = ContainsText String
    | DateStart Date.Date
    | DateEnd Date.Date
    | DesignClass Classification



--| DesignTag Tag


defaultKeys :
    { dateStartKey : String
    , dateEndKey : String
    , searchTextKey : String
    , classificationMinimalKey : String
    , classificationRationalKey : String
    , classificationEngineeredKey : String
    , classificationCompPhysKey : String
    , classificationCompDLKey : String
    , classificationConsensusKey : String
    , classificationOtherKey : String
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
    , classificationMinimalKey = "design-classification-minimal"
    , classificationRationalKey = "design-classification-rational"
    , classificationEngineeredKey = "design-classification-engineered"
    , classificationCompPhysKey = "design-classification-comp-phys"
    , classificationCompDLKey = "design-classification-comp-dl"
    , classificationConsensusKey = "design-classification-consensus"
    , classificationOtherKey = "design-classification-other"
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
        [ ( defaultKeys.classificationMinimalKey, False )
        , ( defaultKeys.classificationRationalKey, False )
        , ( defaultKeys.classificationEngineeredKey, False )
        , ( defaultKeys.classificationCompPhysKey, False )
        , ( defaultKeys.classificationCompDLKey, False )
        , ( defaultKeys.classificationConsensusKey, False )
        , ( defaultKeys.classificationOtherKey, False )
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



--DesignTag tag ->
--    tagToString tag


toDesignFilter : String -> DesignFilter
toDesignFilter key =
    case key of
        "design-classification-minimal" ->
            DesignClass Minimal

        "design-classification-rational" ->
            DesignClass Rational

        "design-classification-engineered" ->
            DesignClass Engineered

        "design-classification-comp-phys" ->
            DesignClass CompPhys

        "design-classification-comp-dl" ->
            DesignClass CompDL

        "design-classification-consensus" ->
            DesignClass Consensus

        "design-classification-other" ->
            DesignClass Other

        {---
        "design-keyword-synthetic" ->
            DesignTag Synthetic

        "design-keyword-de-novo" ->
            DesignTag DeNovo

        "design-keyword-novel" ->
            DesignTag Novel

        "design-keyword-designed" ->
            DesignTag Designed

        "design-keyword-protein-binding" ->
            DesignTag ProteinBinding

        "design-keyword-metal-binding" ->
            DesignTag MetalBinding

        "design-keyword-transcription" ->
            DesignTag Transcription

        "design-keyword-growth" ->
            DesignTag Growth

        "design-keyword-structural" ->
            DesignTag Structural

        "design-keyword-alpha-helical-bundle" ->
            DesignTag AlphaHelicalBundle

        "design-keyword-beta-beta-alpha" ->
            DesignTag BetaBetaAlpha

        "design-keyword-coiled-coil" ->
            DesignTag CoiledCoil

        "design-keyword-unknown" ->
            DesignTag UnknownFunction
--}
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
            classification == design.classification



--DesignTag tag ->
--    List.member tag design.tags
