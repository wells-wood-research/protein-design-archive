module DesignFilter exposing (..)

import List exposing (filter)
import ProteinDesign exposing (ProteinDesign, searchableText)


type DesignFilter
    = ContainsText String
    | DateRange Int Int -- Temporary


toString : DesignFilter -> String
toString filter =
    case filter of
        ContainsText string ->
            string

        DateRange _ _ ->
            "Date Range"


meetsAllFilters : List DesignFilter -> ProteinDesign -> Maybe ProteinDesign
meetsAllFilters filters design =
    List.all (\f -> designMeetsFilter design f) filters
        |> (\allFiltersMet ->
                if allFiltersMet then
                    Just design

                else
                    Nothing
           )


designMeetsFilter : ProteinDesign -> DesignFilter -> Bool
designMeetsFilter design filter =
    case filter of
        ContainsText searchString ->
            design
                |> searchableText
                |> String.contains (String.toLower searchString)

        DateRange _ _ ->
            Debug.todo "do this"
