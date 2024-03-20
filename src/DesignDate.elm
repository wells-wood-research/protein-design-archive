module DesignDate exposing (..)

import Date exposing (Date, Unit(..))
import ProteinDesign exposing (..)
import Time exposing (Month(..))


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
