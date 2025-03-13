module DesignFilter exposing (..)

import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import File.Download exposing (url)
import Json.Decode exposing (string)
import List exposing (filter, filterMap)
import ProteinDesign
    exposing
        ( Classification(..)
        , ProteinDesign
        , ProteinDesignStub
        , Tag(..)
        , stubSearchableText
        )
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, s)
import Url.Parser.Query as Query
import Vega exposing (icThresholds)


type DesignFilter
    = ContainsTextParsed String
    | DateStart String
    | DateEnd String
    | SimilaritySequence Float
    | SimilarityStructure Float
    | SimilaritySequenceExclusion Bool
    | SimilarityStructureExclusion Bool
    | DesignClass Classification
    | Vote Bool



--| DesignTag Tag


defaultKeys :
    { dateStartKey : String
    , dateEndKey : String
    , searchTextKey : String
    , similaritySequenceKey : String
    , similarityStructureKey : String
    , similaritySequenceExclusionKey : String
    , similarityStructureExclusionKey : String
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
    }
defaultKeys =
    { dateStartKey = "deposition-date-after"
    , dateEndKey = "deposition-date-before"
    , searchTextKey = "search-text"
    , similaritySequenceKey = "sim-seq-bit-lt"
    , similarityStructureKey = "sim-struct-lddt-lt"
    , similaritySequenceExclusionKey = "sim-excl-uncomp-seq"
    , similarityStructureExclusionKey = "sim-excl-uncomp-struct"
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
        ]


encodeFiltersToUrl : Dict String DesignFilter -> String
encodeFiltersToUrl filters =
    filters
        |> Dict.map (\key value -> Url.Builder.string key (valueToString value))
        |> Dict.values
        |> Url.Builder.toQuery


queryStringToPairs : String -> List ( String, String )
queryStringToPairs queryString =
    if String.startsWith "?" queryString then
        String.dropLeft 1 queryString
            |> String.split "&"
            |> List.filterMap
                (\pair ->
                    case String.split "=" pair of
                        [ key, value ] ->
                            Just ( key, value )

                        _ ->
                            Nothing
                )

    else
        []


queryParser : Query.Parser (Dict String DesignFilter)
queryParser =
    Query.map7
        (\dateAfter dateBefore text simSeq simStruct exclSeq exclStruct ->
            Dict.fromList
                (List.filterMap identity
                    [ Maybe.map (\v -> ( "deposition-date-after", DateStart v )) dateAfter
                    , Maybe.map (\v -> ( "deposition-date-before", DateEnd v )) dateBefore
                    , Maybe.map (\v -> ( "search-text", ContainsTextParsed v )) text
                    , Maybe.map (\v -> ( "sim-seq-bit-lt", SimilaritySequence v )) simSeq
                    , Maybe.map (\v -> ( "sim-struct-lddt-lt", SimilarityStructure v )) simStruct
                    , Maybe.map (\v -> ( "sim-excl-uncomp-seq", SimilaritySequenceExclusion v )) exclSeq
                    , Maybe.map (\v -> ( "sim-excl-uncomp-struct", SimilarityStructureExclusion v )) exclStruct
                    ]
                )
        )
        (Query.string "deposition-date-after")
        (Query.string "deposition-date-before")
        (Query.string "search-text")
        (Query.custom "sim-seq-bit-lt" (List.head << List.filterMap String.toFloat))
        (Query.custom "sim-struct-lddt-lt" (List.head << List.filterMap String.toFloat))
        (Query.custom "sim-excl-uncomp-seq" (Just << List.any (\s -> stringToBool s == Just True)))
        (Query.custom "sim-excl-uncomp-struct" (Just << List.any (\s -> stringToBool s == Just True)))


urlParser : Parser (Dict String DesignFilter -> a) a
urlParser =
    Url.Parser.top <?> queryParser


decodeUrlToFilters : Url -> Dict String DesignFilter
decodeUrlToFilters url =
    Url.Parser.parse urlParser url
        |> Maybe.withDefault (Dict.singleton "broken-url" (Vote True))


valueToString : DesignFilter -> String
valueToString filter =
    case filter of
        ContainsTextParsed string ->
            string

        DateStart startDate ->
            startDate

        DateEnd endDate ->
            endDate

        SimilaritySequence threshold ->
            String.fromFloat threshold

        SimilarityStructure threshold ->
            String.fromFloat <| (toFloat (round threshold) / 100.0)

        Vote vote ->
            boolToString vote

        SimilaritySequenceExclusion isTicked ->
            boolToString isTicked

        SimilarityStructureExclusion isTicked ->
            boolToString isTicked

        _ ->
            "ERROR_INVALID_FILTER"


stringToValue : String -> String -> DesignFilter
stringToValue key value =
    case key of
        "search-text" ->
            ContainsTextParsed value

        "deposition-date-after" ->
            DateStart value

        "deposition-date-before" ->
            DateEnd value

        "sim-seq-bit-lt" ->
            case String.toFloat value of
                Just threshold ->
                    SimilaritySequence threshold

                _ ->
                    SimilaritySequence 1000.0

        "sim-struct-lddt-lt" ->
            case String.toFloat value of
                Just threshold ->
                    SimilarityStructure <| threshold * 100.0

                _ ->
                    SimilarityStructure 100.0

        "vote-keep" ->
            case stringToBool value of
                Just bool ->
                    Vote bool

                _ ->
                    Vote False

        "sim-excl-uncomp-seq" ->
            case stringToBool value of
                Just bool ->
                    SimilaritySequenceExclusion bool

                _ ->
                    SimilaritySequenceExclusion False

        "sim-excl-uncomp-struct" ->
            case stringToBool value of
                Just bool ->
                    SimilarityStructureExclusion bool

                _ ->
                    SimilarityStructureExclusion False

        _ ->
            ContainsTextParsed value


boolToString : Bool -> String
boolToString value =
    if value then
        "True"

    else
        "False"


stringToBool : String -> Maybe Bool
stringToBool value =
    case String.toLower value of
        "true" ->
            Just True

        "false" ->
            Just False

        _ ->
            Nothing


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
    valueToString <| toDesignFilter label


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

        DateStart startDateString ->
            case Date.fromIsoString startDateString of
                Err _ ->
                    True

                Ok startDate ->
                    if Date.compare startDate design.release_date == LT then
                        True

                    else
                        False

        DateEnd endDateString ->
            case Date.fromIsoString endDateString of
                Err _ ->
                    True

                Ok endDate ->
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

        SimilaritySequenceExclusion isTicked ->
            if isTicked then
                if design.seq_max_sim_natural.partner == "" then
                    False

                else
                    True

            else
                True

        SimilarityStructureExclusion isTicked ->
            if isTicked then
                if design.struct_max_sim_natural.partner == "" then
                    False

                else
                    True

            else
                True

        _ ->
            True
