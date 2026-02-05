module DesignFilter exposing (..)

import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import List
import ProteinDesign
    exposing
        ( ProteinDesign
        , ProteinDesignStub
        , stubSearchableText
        )
import Time exposing (Month(..))
import Url
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type DesignFilter
    = ContainsTextParsed String
    | DateStart String
    | DateEnd String
    | SimilaritySequence Float
    | SimilarityStructure Float
    | SimilaritySequenceExclusion Bool
    | SimilarityStructureExclusion Bool
    | CathArch String Bool
    | CathUnassigned Bool


defaultKeys :
    { dateStartKey : String
    , dateEndKey : String
    , searchTextKey : String
    , similaritySequenceKey : String
    , similarityStructureKey : String
    , similaritySequenceExclusionKey : String
    , similarityStructureExclusionKey : String
    , cathArchKey : String
    , cathUnassignedKey : String
    }
defaultKeys =
    { dateStartKey = "deposition-date-after"
    , dateEndKey = "deposition-date-before"
    , searchTextKey = "search-text"
    , similaritySequenceKey = "sim-seq-bit-lt"
    , similarityStructureKey = "sim-struct-lddt-lt"
    , similaritySequenceExclusionKey = "sim-excl-uncomp-seq"
    , similarityStructureExclusionKey = "sim-excl-uncomp-struct"
    , cathArchKey = "cath-arch"
    , cathUnassignedKey = "cath-unassigned"
    }


encodeFiltersToUrl : Dict String DesignFilter -> Dict String String
encodeFiltersToUrl filters =
    Dict.map (\_ value -> valueToString value) filters


parseCathArchList : String -> List DesignFilter
parseCathArchList value =
    value
        |> String.split ","
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map (\code -> CathArch code True)


queryParser : Query.Parser (Dict String DesignFilter)
queryParser =
    Query.map8
        (\dateAfter dateBefore searchText simSeq simStruct exclSeq exclStruct cathUnassigned ->
            \cathArchRaw ->
                let
                    base =
                        Dict.fromList <|
                            List.filterMap identity
                                [ Maybe.map (\v -> ( defaultKeys.dateStartKey, DateStart v )) dateAfter
                                , Maybe.map (\v -> ( defaultKeys.dateEndKey, DateEnd v )) dateBefore
                                , Maybe.map
                                    (\v ->
                                        let
                                            decoded =
                                                Maybe.withDefault v (Url.percentDecode v)
                                        in
                                        ( defaultKeys.searchTextKey, ContainsTextParsed decoded )
                                    )
                                    searchText
                                , Maybe.map (\v -> ( defaultKeys.similaritySequenceKey, SimilaritySequence v )) simSeq
                                , Maybe.map (\v -> ( defaultKeys.similarityStructureKey, SimilarityStructure v )) simStruct
                                , Maybe.map (\v -> ( defaultKeys.similaritySequenceExclusionKey, SimilaritySequenceExclusion v )) exclSeq
                                , Maybe.map (\v -> ( defaultKeys.similarityStructureExclusionKey, SimilarityStructureExclusion v )) exclStruct
                                , Maybe.map (\v -> ( defaultKeys.cathUnassignedKey, CathUnassigned v )) cathUnassigned
                                ]

                    cathArchDict =
                        cathArchRaw
                            |> Maybe.withDefault ""
                            |> parseCathArchList
                            |> List.indexedMap
                                (\i filter ->
                                    ( defaultKeys.cathArchKey ++ "-" ++ String.fromInt i
                                    , filter
                                    )
                                )
                            |> Dict.fromList
                in
                Dict.union base cathArchDict
        )
        (Query.string defaultKeys.dateStartKey)
        (Query.string defaultKeys.dateEndKey)
        (Query.string defaultKeys.searchTextKey)
        (Query.custom defaultKeys.similaritySequenceKey (List.head << List.filterMap String.toFloat))
        (Query.custom defaultKeys.similarityStructureKey (List.head << List.filterMap String.toFloat))
        (Query.custom defaultKeys.similaritySequenceExclusionKey (List.head << List.filterMap stringToBool))
        (Query.custom defaultKeys.similarityStructureExclusionKey (List.head << List.filterMap stringToBool))
        (Query.custom defaultKeys.cathUnassignedKey (List.head << List.filterMap stringToBool))
        |> apply (Query.string defaultKeys.cathArchKey)


apply : Query.Parser a -> Query.Parser (a -> b) -> Query.Parser b
apply argParser funcParser =
    Query.map2 (<|) funcParser argParser


urlParser : Parser (Dict String DesignFilter -> a) a
urlParser =
    Url.Parser.top <?> queryParser


decodeUrlToFilters : Url.Url -> Dict String DesignFilter
decodeUrlToFilters url =
    Url.Parser.parse urlParser url
        |> Maybe.withDefault Dict.empty


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
            String.fromFloat <| toFloat (round threshold)

        SimilaritySequenceExclusion isTicked ->
            boolToString isTicked

        SimilarityStructureExclusion isTicked ->
            boolToString isTicked

        CathArch name isTicked ->
            name ++ boolToString isTicked

        CathUnassigned isTicked ->
            boolToString isTicked


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

        "cath-arch" ->
            CathArch value True

        "cath-unassigned" ->
            case stringToBool value of
                Just bool ->
                    CathUnassigned bool

                _ ->
                    CathUnassigned False

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


defaultStartDate : Date
defaultStartDate =
    Date.fromCalendarDate 1900 Jan 1


defaultEndDate : Date
defaultEndDate =
    Date.fromCalendarDate 2100 Dec 31


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
            List.map String.trim <| String.split "AND" searchString

        updateDict condition dict =
            if String.contains "NOT" condition then
                let
                    splitConditions =
                        if String.contains "OR" condition then
                            List.map String.trim (String.split "OR" condition)

                        else
                            [ String.trim <| String.replace "NOT" "" condition ]

                    updatedList =
                        case Dict.get "NOT" dict of
                            Just list ->
                                list ++ [ splitConditions ]

                            Nothing ->
                                [ splitConditions ]
                in
                Dict.insert "NOT" updatedList dict

            else if String.contains "OR" condition then
                let
                    splitConditions =
                        String.split "OR" condition
                            |> List.map String.trim

                    updatedList =
                        case Dict.get "OR" dict of
                            Just list ->
                                list ++ [ splitConditions ]

                            Nothing ->
                                [ splitConditions ]
                in
                Dict.insert "OR" updatedList dict

            else
                let
                    updatedList =
                        case Dict.get "AND" dict of
                            Just list ->
                                list ++ [ [ condition ] ]

                            Nothing ->
                                [ [ condition ] ]
                in
                Dict.insert "AND" updatedList dict
    in
    List.foldl updateDict (Dict.fromList [ ( "AND", [] ), ( "OR", [] ), ( "NOT", [] ) ]) conditionsList


meetsOtherFilters : ProteinDesignStub -> List DesignFilter -> Bool
meetsOtherFilters design filters =
    List.all (stubMeetsOneFilter design) filters


meetsCathArchFilters : ProteinDesignStub -> List DesignFilter -> Bool
meetsCathArchFilters design cathArchFilters =
    let
        activeCathCodes =
            cathArchFilters
                |> List.filterMap
                    (\filter ->
                        case filter of
                            CathArch code True ->
                                Just code

                            _ ->
                                Nothing
                    )
    in
    case activeCathCodes of
        [] ->
            True

        codes ->
            List.any
                (\cath ->
                    List.member cath.code codes
                )
                design.cath_arch


isCathArch : DesignFilter -> Bool
isCathArch filter =
    case filter of
        CathArch _ _ ->
            True

        _ ->
            False


stubMeetsAllFilters : List DesignFilter -> ProteinDesignStub -> Maybe ProteinDesignStub
stubMeetsAllFilters filters design =
    let
        cathArchFilters =
            List.filter isCathArch filters

        otherFilters =
            List.filter (not << isCathArch) filters
    in
    meetsOtherFilters design otherFilters
        && meetsCathArchFilters design cathArchFilters
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
                    case Dict.get "AND" searchDict of
                        Just listOfListsOfConditions ->
                            List.concatMap identity listOfListsOfConditions

                        Nothing ->
                            []

                notConditions =
                    case Dict.get "NOT" searchDict of
                        Just listOfListsOfConditions ->
                            List.concatMap identity listOfListsOfConditions

                        Nothing ->
                            []

                orConditions =
                    Maybe.withDefault [] (Dict.get "OR" searchDict)

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
                if design.seq_max_sim_designed.partner == "" then
                    False

                else
                    True

            else
                True

        SimilarityStructureExclusion isTicked ->
            if isTicked then
                if design.struct_max_sim_designed.partner == "" then
                    False

                else
                    True

            else
                True

        CathArch cathCode isTicked ->
            if isTicked then
                List.any (\c -> c.code == cathCode) design.cath_arch

            else
                True

        CathUnassigned isTicked ->
            if isTicked then
                List.isEmpty design.cath_arch

            else
                True
