module ProteinDesign exposing (..)

import Date exposing (Date, Unit(..))
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Month(..))


type alias ProteinDesign =
    { pdb : String
    , picture_path : String
    , chains : List Chain
    , authors : List Author
    , classification : String
    , keyword : List String
    , tags : List String
    , release_date : Date
    , publication_title : String
    , publication_journal_abbrev : String
    , publication_journal_volume : String
    , publication_page_first : String
    , publication_page_last : String
    , publication_id_astm : String
    , publication_id_issn : String
    , publication_id_csd : String
    , publication_id_doi : String
    , publication_id_pubmed : String
    , publication_country : String
    , abstract : String
    , related_pdb : String
    , crystal_structure : Xtal
    , exptl_method : List String
    , formula_weight : Float
    , synthesis_comment : String
    }


type alias Author =
    { forename : String
    , surname : String
    }


type alias Chain =
    { chain_id : String
    , chain_seq : String
    }


type alias Xtal =
    { length_a : String
    , length_b : String
    , length_c : String
    , angle_a : String
    , angle_b : String
    , angle_g : String
    }


proteinDesignDecoder : Decoder ProteinDesign
proteinDesignDecoder =
    Decode.succeed ProteinDesign
        |> required "pdb" string
        |> required "picture_path" string
        |> required "chains" (list chainDecoder)
        |> required "authors" (list authorDecoder)
        |> required "classification" string
        |> required "keyword" (list string)
        |> required "tags" (list string)
        |> required "release_date" dateDecoder
        |> required "publication_title" string
        |> required "publication_journal_abbrev" string
        |> required "publication_journal_volume" string
        |> required "publication_page_first" string
        |> required "publication_page_last" string
        |> required "publication_id_astm" string
        |> required "publication_id_issn" string
        |> required "publication_id_csd" string
        |> required "publication_id_doi" string
        |> required "publication_id_pubmed" string
        |> required "publication_country" string
        |> required "abstract" string
        |> required "related_pdb" string
        |> required "crystal_structure" xtalDecoder
        |> required "exptl_method" (list string)
        |> required "formula_weight" float
        |> required "synthesis_comment" string


authorDecoder : Decoder Author
authorDecoder =
    Decode.succeed Author
        |> required "forename" string
        |> required "surname" string


chainDecoder : Decoder Chain
chainDecoder =
    Decode.succeed Chain
        |> required "chain_id" string
        |> required "chain_seq" string


xtalDecoder : Decoder Xtal
xtalDecoder =
    Decode.succeed Xtal
        |> required "length_a" string
        |> required "length_b" string
        |> required "length_c" string
        |> required "angle_a" string
        |> required "angle_b" string
        |> required "angle_g" string


dateDecoder : Decoder Date
dateDecoder =
    string
        |> Decode.andThen
            (\isoString ->
                case Date.fromIsoString isoString of
                    Ok date ->
                        Decode.succeed date

                    Err _ ->
                        Decode.succeed (Date.fromCalendarDate 1900 Jan 1)
            )


searchableText : ProteinDesign -> String
searchableText proteinDesign =
    [ proteinDesign.pdb
    , String.join " " proteinDesign.tags
    , String.join " " <| List.map chainToString proteinDesign.chains
    , String.join " " <| List.map authorToString proteinDesign.authors
    ]
        |> String.join "\n"
        |> String.toLower


chainToString : Chain -> String
chainToString chain =
    chain.chain_id ++ " " ++ chain.chain_seq


authorToString : Author -> String
authorToString author =
    author.forename ++ " " ++ author.surname


type Classification
    = OriginalDeNovo
    | RelativeDeNovo
    | Small
    | Engineered
    | Unknown


stringToClassification : String -> Classification
stringToClassification string =
    case string of
        "original de novo design" ->
            OriginalDeNovo

        "relative of another de novo design" ->
            RelativeDeNovo

        "small, non-systematic, and other" ->
            Small

        "engineered" ->
            Engineered

        _ ->
            Unknown


classificationToString : Classification -> String
classificationToString classification =
    case classification of
        OriginalDeNovo ->
            "Original De Novo"

        RelativeDeNovo ->
            "Relative De Novo"

        Small ->
            "Small, Non-Systematic, Other"

        Engineered ->
            "Engineered"

        Unknown ->
            "Unknown"


classificationToColour : Classification -> String
classificationToColour classification =
    case classification of
        OriginalDeNovo ->
            "#ff0000"

        RelativeDeNovo ->
            "#00ff00"

        Small ->
            "#ffffff"

        Engineered ->
            "#0000ff"

        Unknown ->
            "#333333"


type Tag
    = Synthetic
    | DeNovo
    | Novel
    | Designed
    | ProteinBinding
    | MetalBinding
    | Transcription
    | Growth
    | Structural
    | AlphaHelicalBundle
    | BetaBetaAlpha
    | CoiledCoil
    | UnknownFunction


stringToTag : String -> Tag
stringToTag string =
    case string of
        "synthetic protein model" ->
            Synthetic

        "de novo protein" ->
            DeNovo

        "de novo protein design" ->
            DeNovo

        "novel sequence" ->
            Novel

        "designed peptide" ->
            Designed

        "protein binding" ->
            ProteinBinding

        "metal binding protein" ->
            MetalBinding

        "transcription" ->
            Transcription

        "growth response protein" ->
            Growth

        "structural protein" ->
            Structural

        "unknown function" ->
            UnknownFunction

        "alpha-helical bundle" ->
            AlphaHelicalBundle

        "beta beta alpha motif" ->
            BetaBetaAlpha

        "coiled coil" ->
            CoiledCoil

        _ ->
            UnknownFunction


tagToString : Tag -> String
tagToString tag =
    case tag of
        Synthetic ->
            "synthetic"

        DeNovo ->
            "de novo"

        Novel ->
            "novel sequence"

        Designed ->
            "designed peptide"

        ProteinBinding ->
            "protein binding"

        MetalBinding ->
            "metal binding"

        Transcription ->
            "transcription"

        Growth ->
            "growth response"

        Structural ->
            "structural"

        UnknownFunction ->
            "unknown"

        AlphaHelicalBundle ->
            "alpha-helical bundle"

        BetaBetaAlpha ->
            "beta-beta-alpha motif"

        CoiledCoil ->
            "coiled-coil"
