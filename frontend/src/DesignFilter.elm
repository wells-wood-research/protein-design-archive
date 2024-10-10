module DesignFilter exposing (..)

import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Json.Decode exposing (string)
import List exposing (filter)
import ProteinDesign
    exposing
        ( Classification(..)
        , ProteinDesign
        , ProteinDesignStub
        , Tag(..)
        , classificationToString
        , stubSearchableText
        )
import Time exposing (Month(..))


type DesignFilter
    = ContainsTextParsed String
    | DateStart Date.Date
    | DateEnd Date.Date
    | SimilaritySequence Float
    | SimilarityStructure Float
    | DesignClass Classification
    | Vote Bool



--| DesignTag Tag


defaultKeys :
    { dateStartKey : String
    , dateEndKey : String
    , searchTextKey : String
    , searchTextParsedKey : String
    , similaritySequenceKey : String
    , similarityStructureKey : String
    , classificationMinimalKey : String
    , classificationRationalKey : String
    , classificationEngineeredKey : String
    , classificationPhysKey : String
    , classificationDeepLearningKey : String
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
    , voteKeep : String
    , voteRemove : String
    }
defaultKeys =
    { dateStartKey = "deposition-date-start"
    , dateEndKey = "deposition-date-end"
    , searchTextKey = "search-text-string"
    , searchTextParsedKey = "search-text-parsed"
    , similaritySequenceKey = "similarity-sequence-bit"
    , similarityStructureKey = "similarity-structure-lddt"
    , classificationMinimalKey = "design-classification-minimal"
    , classificationRationalKey = "design-classification-rational"
    , classificationEngineeredKey = "design-classification-engineered"
    , classificationPhysKey = "design-classification-comp-phys"
    , classificationDeepLearningKey = "design-classification-comp-dl"
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
    , voteKeep = "vote-keep"
    , voteRemove = "vote-remove"
    }


checkboxDict : Dict String Bool
checkboxDict =
    Dict.fromList
        [ ( defaultKeys.classificationMinimalKey, False )
        , ( defaultKeys.classificationRationalKey, False )
        , ( defaultKeys.classificationEngineeredKey, False )
        , ( defaultKeys.classificationPhysKey, False )
        , ( defaultKeys.classificationDeepLearningKey, False )
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
        , ( defaultKeys.voteKeep, False )
        , ( defaultKeys.voteRemove, False )
        ]


toString : DesignFilter -> String
toString filter =
    case filter of
        ContainsTextParsed _ ->
            "complicated_search_string"

        DateStart startDate ->
            Date.toIsoString startDate

        DateEnd endDate ->
            Date.toIsoString endDate

        DesignClass classification ->
            classificationToString classification

        Vote vote ->
            if vote then
                "keep"

            else
                "remove"

        SimilaritySequence threshold ->
            "sequence similarity below " ++ String.fromFloat threshold

        SimilarityStructure threshold ->
            "structure similarity below " ++ String.fromFloat threshold


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
            DesignClass Phys

        "design-classification-comp-dl" ->
            DesignClass DeepLearning

        "design-classification-consensus" ->
            DesignClass Consensus

        "design-classification-other" ->
            DesignClass Other

        "vote-keep" ->
            Vote True

        "vote-remove" ->
            Vote False

        _ ->
            ContainsTextParsed ""


keyToLabel : String -> String
keyToLabel label =
    toString <| toDesignFilter label


defaultStartDate : Date
defaultStartDate =
    Date.fromCalendarDate 1900 Jan 1


defaultEndDate : Date
defaultEndDate =
    Date.fromCalendarDate 2100 Dec 31


removeHyphenFromIsoDate : String -> String
removeHyphenFromIsoDate string =
    if String.right 1 string /= "-" then
        string

    else
        String.dropRight 1 string


isValidIsoDate : String -> Bool
isValidIsoDate string =
    let
        phrase =
            removeHyphenFromIsoDate string
    in
    case Date.fromIsoString phrase of
        Err _ ->
            False

        Ok _ ->
            True


getFirstAndLastDate : List ProteinDesign -> { firstDate : Date, lastDate : Date }
getFirstAndLastDate proteinDesigns =
    let
        sortedDesigns =
            List.sortWith
                (\a b -> Date.compare a.release_date b.release_date)
                proteinDesigns

        firstDesignDate =
            List.head sortedDesigns
                |> Maybe.map .release_date
                |> Maybe.withDefault defaultStartDate

        lastDesignDate =
            List.reverse sortedDesigns
                |> List.head
                |> Maybe.map .release_date
                |> Maybe.withDefault defaultEndDate
    in
    { firstDate = firstDesignDate, lastDate = lastDesignDate }


dateToPosition :
    { firstDate : Date
    , lastDate : Date
    , date : Date
    , height : Int
    , radius : Int
    }
    -> Int
dateToPosition { firstDate, lastDate, date, height, radius } =
    let
        dateRange =
            Date.diff Days lastDate firstDate
                |> toFloat

        dateDelta =
            Date.diff Days date firstDate
                |> toFloat

        fraction =
            dateDelta / dateRange
    in
    fraction
        * toFloat (height - (2 * radius))
        |> round
        |> (+) 3


parseStringToConditions : String -> Dict String (List (List String))
parseStringToConditions searchString =
    let
        conditionsList =
            List.map String.trim <| String.split "&&" searchString

        updateDict condition dict =
            if String.contains "!!" condition then
                let
                    splitConditions =
                        if String.contains "||" condition then
                            List.map String.trim (String.split "||" condition)

                        else
                            [ String.trim <| String.replace "!!" "" condition ]

                    updatedList =
                        case Dict.get "!!" dict of
                            Just list ->
                                list ++ [ splitConditions ]

                            Nothing ->
                                [ splitConditions ]
                in
                Dict.insert "!!" updatedList dict

            else if String.contains "||" condition then
                let
                    splitConditions =
                        String.split "||" condition
                            |> List.map String.trim

                    updatedList =
                        case Dict.get "||" dict of
                            Just list ->
                                list ++ [ splitConditions ]

                            Nothing ->
                                [ splitConditions ]
                in
                Dict.insert "||" updatedList dict

            else
                let
                    updatedList =
                        case Dict.get "&&" dict of
                            Just list ->
                                list ++ [ [ condition ] ]

                            Nothing ->
                                [ [ condition ] ]
                in
                Dict.insert "&&" updatedList dict
    in
    List.foldl updateDict (Dict.fromList [ ( "&&", [] ), ( "||", [] ), ( "!!", [] ) ]) conditionsList


stubMeetsAllFilters : List DesignFilter -> ProteinDesignStub -> Maybe ProteinDesignStub
stubMeetsAllFilters filters design =
    List.all (\f -> stubMeetsOneFilter design f) filters
        |> (\allFiltersMet ->
                if allFiltersMet then
                    Just design

                else
                    Nothing
           )


stubMeetsOneFilter : ProteinDesignStub -> DesignFilter -> Bool
stubMeetsOneFilter design filter =
    case filter of
        ContainsTextParsed string ->
            let
                searchDict =
                    parseStringToConditions string

                andConditions =
                    case Dict.get "&&" searchDict of
                        Just listOfListsOfConditions ->
                            List.concatMap identity listOfListsOfConditions

                        Nothing ->
                            []

                notConditions =
                    case Dict.get "!!" searchDict of
                        Just listOfListsOfConditions ->
                            List.concatMap identity listOfListsOfConditions

                        Nothing ->
                            []

                orConditions =
                    Maybe.withDefault [] (Dict.get "||" searchDict)

                searchableText =
                    stubSearchableText design
            in
            if
                List.all (\searchString -> String.contains (String.toLower searchString) searchableText) andConditions
                    && (not <| List.any (\searchString -> String.contains (String.toLower searchString) searchableText) notConditions)
                    && List.all (\eachOrSet -> List.any (\searchString -> String.contains (String.toLower searchString) searchableText) eachOrSet) orConditions
            then
                True

            else
                False

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

        SimilaritySequence sim ->
            if sim == 1000.0 || design.seq_max_sim_natural.similarity <= sim then
                True

            else
                False

        SimilarityStructure sim ->
            let
                scaled_similarity =
                    sim / 100.0
            in
            if scaled_similarity == 1.0 || design.struct_max_sim_natural.similarity < scaled_similarity then
                True

            else
                False

        _ ->
            True
