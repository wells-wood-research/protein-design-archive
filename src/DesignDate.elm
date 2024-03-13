module DesignDate exposing (..)

import Date exposing (Date, Unit(..))
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
