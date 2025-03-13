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
import Url.Builder
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
    , voteKeep = "vote-keep"
    }


encodeFiltersToUrl : Dict String DesignFilter -> String
encodeFiltersToUrl filters =
    filters
        |> Dict.map (\key value -> Url.Builder.string key (valueToString value))
        |> Dict.values
        |> Url.Builder.toQuery


decodeUrlTextTwice : String -> String
decodeUrlTextTwice input =
    input
        |> Url.percentDecode
        |> Maybe.andThen Url.percentDecode
        |> Maybe.withDefault input


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
                    , Maybe.map
                        (\v ->
                            let
                                decodedText =
                                    String.replace "NOT" "&&!!"
                                        << String.replace "AND" "&&"
                                        << String.replace "OR" "||"
                                        << decodeUrlTextTwice
                            in
                            ( "search-text", ContainsTextParsed (decodedText v) )
                        )
                        text
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


decodeUrlToFilters : Url.Url -> Dict String DesignFilter
decodeUrlToFilters url =
    Url.Parser.parse urlParser url
        |> Maybe.withDefault Dict.empty


valueToString : DesignFilter -> String
valueToString filter =
    case filter of
        ContainsTextParsed string ->
            string
                |> String.replace "&&!!" "NOT"
                |> String.replace "&&" "AND"
                |> String.replace "||" "OR"
                |> Url.percentEncode

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
