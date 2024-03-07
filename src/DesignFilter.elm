module DesignFilter exposing (..)

import Date
import List exposing (filter)
import ProteinDesign exposing (Classification(..), ProteinDesign, classificationToString, searchableText)


type DesignFilter
    = ContainsText String
    | DateStart Date.Date
    | DateEnd Date.Date
    | DesignClass Classification


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

        _ ->
            ContainsText ""


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
            if Date.compare startDate design.depositionDate == LT then
                True

            else
                False

        DateEnd endDate ->
            if Date.compare endDate design.depositionDate == GT then
                True

            else
                False

        DesignClass classification ->
            classification == design.classification
