module ProteinDesign exposing (..)

import Csv.Encode as CsvEncode
import Date exposing (Date, Unit(..), fromIsoString)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (header)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as JsonEncode exposing (..)
import String exposing (fromFloat)
import Time exposing (Month(..))


type alias ProteinDesign =
    { pdb : String
    , picture_path : String
    , chains : List Chain
    , authors : List Author
    , classification : Classification
    , classification_suggested : List Classification
    , classification_suggested_reason : List String
    , subtitle : String
    , tags : List String
    , keywords : List String
    , release_date : Date
    , publication : String
    , publication_ref : Reference
    , publication_country : String
    , abstract : String
    , related_pdb : List String
    , crystal_structure : Xtal
    , exptl_method : List String
    , formula_weight : Float
    , synthesis_comment : String
    , review : Bool
    , previousDesign : String
    , nextDesign : String
    }


type alias ProteinDesignStub =
    { pdb : String
    , picture_path : String
    , authors : List Author
    , subtitle : String
    , tags : List String
    , keywords : List String
    , release_date : Date
    , publication : String
    }


type Classification
    = Minimal
    | Rational
    | Engineered
    | Computational
    | Phys
    | DeepLearning
    | Consensus
    | Other


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


type alias Author =
    { forename : String
    , surname : String
    }


type alias Reference =
    { doi : String
    , pubmed : String
    , csd : String
    , issn : String
    , astm : String
    }


type alias Chain =
    { chain_id : String
    , chain_seq_unnat : String
    , chain_seq_nat : String
    }


type alias Xtal =
    { length_a : String
    , length_b : String
    , length_c : String
    , angle_a : String
    , angle_b : String
    , angle_g : String
    }


type alias DesignDetails msg =
    { header : String
    , property : Element msg
    }


csvListFromProteinDesign : ProteinDesign -> List ( String, String )
csvListFromProteinDesign proteinDesign =
    [ ( "pdb_code", proteinDesign.pdb )
    , ( "release_date", Date.toIsoString proteinDesign.release_date )
    , ( "classification", classificationToString proteinDesign.classification )
    , ( "chains", String.join ";" (List.map chainToString proteinDesign.chains) )
    , ( "formula_weight", fromFloat proteinDesign.formula_weight )
    , ( "crystal_structure (a;b;c;alpha;beta;gamma)", xtalToString proteinDesign.crystal_structure )
    , ( "exptl_method", String.join ";" proteinDesign.exptl_method )
    , ( "synthesis_comment", proteinDesign.synthesis_comment )
    , ( "authors", authorsToString proteinDesign.authors )
    , ( "publication", proteinDesign.publication )
    , ( "publication_ref", refToString proteinDesign.publication_ref )
    , ( "publication_country", proteinDesign.publication_country )
    , ( "subtitle", proteinDesign.subtitle )
    , ( "tags", String.join ";" proteinDesign.tags )
    , ( "keywords", String.join ";" proteinDesign.keywords )
    , ( "abstract", proteinDesign.abstract )
    , ( "related_pdb", String.join ";" proteinDesign.related_pdb )
    ]


csvStringFromProteinDesign : List ProteinDesign -> String
csvStringFromProteinDesign proteinDesigns =
    proteinDesigns
        |> CsvEncode.encode
            { encoder =
                CsvEncode.withFieldNames
                    csvListFromProteinDesign
            , fieldSeparator = ','
            }


jsonValueFromProteinDesign : ProteinDesign -> JsonEncode.Value
jsonValueFromProteinDesign proteinDesign =
    JsonEncode.object
        [ ( proteinDesign.pdb
          , JsonEncode.object <|
                [ ( "pdb", JsonEncode.string <| proteinDesign.pdb )
                , ( "release_date", JsonEncode.string <| Date.toIsoString proteinDesign.release_date )
                , ( "classification", JsonEncode.string <| classificationToString proteinDesign.classification )
                , ( "chains", JsonEncode.list chainEncoder proteinDesign.chains )
                , ( "formula_weight", JsonEncode.string <| fromFloat proteinDesign.formula_weight )
                , ( "exptl_method", JsonEncode.string <| String.join ";" proteinDesign.exptl_method )
                , ( "crystal_structure", xtalEncoder proteinDesign.crystal_structure )
                , ( "synthesis_comment", JsonEncode.string <| proteinDesign.synthesis_comment )
                , ( "authors", JsonEncode.list authorEncoder proteinDesign.authors )
                , ( "publication", JsonEncode.string <| proteinDesign.publication )
                , ( "publication_ref", JsonEncode.string <| refToString proteinDesign.publication_ref )
                , ( "publication_country", JsonEncode.string <| proteinDesign.publication_country )
                , ( "subtitle", JsonEncode.string <| proteinDesign.subtitle )
                , ( "tags", JsonEncode.string <| String.join ";" proteinDesign.tags )
                , ( "keywords", JsonEncode.string <| String.join ";" proteinDesign.keywords )
                , ( "abstract", JsonEncode.string <| proteinDesign.abstract )
                , ( "related_pdb", JsonEncode.string <| String.join ";" proteinDesign.related_pdb )
                ]
          )
        ]


chainEncoder : Chain -> JsonEncode.Value
chainEncoder chain =
    JsonEncode.object
        [ ( "id", JsonEncode.string chain.chain_id )
        , ( "sequence", JsonEncode.string chain.chain_seq_unnat )
        ]


xtalEncoder : Xtal -> JsonEncode.Value
xtalEncoder xtal =
    JsonEncode.object
        [ ( "length_a", JsonEncode.string xtal.length_a )
        , ( "length_b", JsonEncode.string xtal.length_b )
        , ( "length_c", JsonEncode.string xtal.length_c )
        , ( "angle_alpha", JsonEncode.string xtal.angle_a )
        , ( "angle_beta", JsonEncode.string xtal.angle_b )
        , ( "angle_gamma", JsonEncode.string xtal.angle_g )
        ]


authorEncoder : Author -> JsonEncode.Value
authorEncoder author =
    JsonEncode.object
        [ ( "forename", JsonEncode.string author.forename )
        , ( "surname", JsonEncode.string author.surname )
        ]


jsonStringFromProteinDesign : ProteinDesign -> String
jsonStringFromProteinDesign proteinDesign =
    JsonEncode.encode 4 (jsonValueFromProteinDesign proteinDesign)


designDetailsFromProteinDesign : ProteinDesign -> List (DesignDetails msg)
designDetailsFromProteinDesign proteinDesign =
    [ { header = "PDB code"
      , property =
            newTabLink
                [ Font.color <| rgb255 104 176 171
                , Font.underline
                ]
                { url =
                    "https://www.rcsb.org/structure/"
                        ++ proteinDesign.pdb
                , label =
                    proteinDesign.pdb
                        |> text
                }
      }
    , { header = "Subtitle"
      , property =
            el
                [ padding 2
                , Border.width 2
                , Border.color <| rgb255 220 220 220
                , Border.rounded 3
                , alignTop
                , width (fill |> maximum 350)
                ]
                (image
                    [ width fill ]
                    { src = proteinDesign.picture_path
                    , description = "Structure of " ++ proteinDesign.pdb
                    }
                )
      }
    , { header = "Subtitle"
      , property =
            if String.isEmpty proteinDesign.subtitle then
                text "-"

            else
                text <| proteinDesign.subtitle
      }
    , { header = "Classification"
      , property =
            if String.isEmpty <| classificationToString proteinDesign.classification then
                text "-"

            else
                text <| classificationToString proteinDesign.classification
      }
    , { header = "Tags"
      , property =
            if List.isEmpty proteinDesign.tags then
                text "-"

            else
                text <| String.join ", " proteinDesign.tags
      }
    , { header = "Release date"
      , property =
            if String.isEmpty <| Date.toIsoString proteinDesign.release_date then
                text "-"

            else
                text <| Date.toIsoString proteinDesign.release_date
      }
    , { header = "Publication"
      , property =
            if String.isEmpty proteinDesign.publication then
                text "-"

            else
                text <| proteinDesign.publication
      }
    , { header = "Reference link"
      , property =
            newTabLink
                [ Font.color <| rgb255 104 176 171
                , Font.underline
                ]
                { url =
                    if String.isEmpty proteinDesign.publication_ref.doi then
                        if String.isEmpty proteinDesign.publication_ref.pubmed then
                            "https://www.rcsb.org/structure/"
                                ++ proteinDesign.pdb

                        else
                            "https://pubmed.ncbi.nlm.nih.gov/"
                                ++ proteinDesign.publication_ref.pubmed

                    else
                        "https://doi.org/"
                            ++ proteinDesign.publication_ref.doi
                , label =
                    (if String.isEmpty proteinDesign.publication_ref.doi then
                        if String.isEmpty proteinDesign.publication_ref.pubmed then
                            "-"

                        else
                            proteinDesign.publication_ref.pubmed

                     else
                        proteinDesign.publication_ref.doi
                    )
                        |> text
                }
      }
    , { header = "Authors"
      , property =
            if List.isEmpty proteinDesign.authors then
                text "-"

            else
                text <| authorsToString proteinDesign.authors
      }
    , { header = "Related entries"
      , property =
            if List.isEmpty proteinDesign.related_pdb then
                text "-"

            else
                text <| String.join ", " proteinDesign.related_pdb
      }
    , { header = "Formula weight"
      , property =
            if String.isEmpty <| String.fromFloat proteinDesign.formula_weight then
                text "-"

            else
                text <| String.fromFloat proteinDesign.formula_weight ++ " Da"
      }
    , { header = "Synthesis comment"
      , property =
            if String.isEmpty proteinDesign.synthesis_comment then
                text "-"

            else
                text <| proteinDesign.synthesis_comment
      }
    ]


rawDesignDecoder : Decoder ProteinDesign
rawDesignDecoder =
    Decode.succeed ProteinDesign
        |> required "pdb" Decode.string
        |> required "picture_path" Decode.string
        |> required "chains" (Decode.list chainDecoder)
        |> required "authors" (Decode.list authorDecoder)
        |> required "classification" classificationDecoder
        |> required "classification_suggested" (Decode.list classificationDecoder)
        |> required "classification_suggested_reason" (Decode.list Decode.string)
        |> required "subtitle" Decode.string
        |> required "tags" (Decode.list Decode.string)
        |> required "keywords" (Decode.list Decode.string)
        |> required "release_date" dateDecoder
        |> required "publication" Decode.string
        |> required "publication_ref" referenceDecoder
        |> required "publication_country" Decode.string
        |> required "abstract" Decode.string
        |> required "related_pdb" (Decode.list Decode.string)
        |> required "crystal_structure" xtalDecoder
        |> required "exptl_method" (Decode.list Decode.string)
        |> required "formula_weight" Decode.float
        |> required "synthesis_comment" Decode.string
        |> required "review" Decode.bool
        |> optional "previous_design" Decode.string "/"
        |> optional "next_design" Decode.string "/"


rawDesignStubDecoder : Decoder ProteinDesignStub
rawDesignStubDecoder =
    Decode.succeed ProteinDesignStub
        |> required "pdb" Decode.string
        |> required "picture_path" Decode.string
        |> required "authors" (Decode.list authorDecoder)
        |> required "subtitle" Decode.string
        |> required "tags" (Decode.list Decode.string)
        |> required "keywords" (Decode.list Decode.string)
        |> required "release_date" dateDecoder
        |> required "publication" Decode.string


classificationDecoder : Decoder Classification
classificationDecoder =
    Decode.map stringToClassification Decode.string


referenceDecoder : Decoder Reference
referenceDecoder =
    Decode.succeed Reference
        |> required "DOI" Decode.string
        |> required "PubMed" Decode.string
        |> required "CSD" Decode.string
        |> required "ISSN" Decode.string
        |> required "ASTM" Decode.string


dateDecoder : Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\dateString ->
                case Date.fromIsoString dateString of
                    Ok date ->
                        Decode.succeed date

                    Err _ ->
                        Decode.succeed (Date.fromCalendarDate 1900 Jan 1)
            )


authorDecoder : Decoder Author
authorDecoder =
    Decode.succeed Author
        |> required "forename" Decode.string
        |> required "surname" Decode.string


chainDecoder : Decoder Chain
chainDecoder =
    Decode.succeed Chain
        |> required "chain_id" Decode.string
        |> required "chain_seq_unnat" Decode.string
        |> required "chain_seq_nat" Decode.string


xtalDecoder : Decoder Xtal
xtalDecoder =
    Decode.succeed Xtal
        |> required "length_a" Decode.string
        |> required "length_b" Decode.string
        |> required "length_c" Decode.string
        |> required "angle_a" Decode.string
        |> required "angle_b" Decode.string
        |> required "angle_g" Decode.string


designSearchableText : ProteinDesign -> String
designSearchableText proteinDesign =
    [ proteinDesign.pdb
    , String.join " " <| List.map chainToString proteinDesign.chains
    , authorsToString proteinDesign.authors
    , classificationToString proteinDesign.classification
    , String.join " " <| List.map classificationToString proteinDesign.classification_suggested
    , proteinDesign.subtitle
    , String.join " " proteinDesign.tags
    , String.join " " proteinDesign.keywords
    , Date.toIsoString proteinDesign.release_date
    , proteinDesign.publication
    , proteinDesign.publication_ref.doi
    , proteinDesign.publication_ref.pubmed
    , proteinDesign.publication_ref.csd
    , proteinDesign.publication_ref.issn
    , proteinDesign.publication_ref.astm
    , proteinDesign.publication_country
    , proteinDesign.abstract
    , String.join " " proteinDesign.related_pdb
    , String.join " " proteinDesign.exptl_method
    , proteinDesign.synthesis_comment
    , xtalToString proteinDesign.crystal_structure
    ]
        |> String.join "\n"
        |> String.toLower


stubSearchableText : ProteinDesignStub -> String
stubSearchableText proteinDesign =
    [ proteinDesign.pdb
    , authorsToString proteinDesign.authors
    , proteinDesign.subtitle
    , String.join " " proteinDesign.tags
    , String.join " " proteinDesign.keywords
    , Date.toIsoString proteinDesign.release_date
    , proteinDesign.publication
    ]
        |> String.join "\n"
        |> String.toLower


refToString : Reference -> String
refToString reference =
    String.join " " <|
        [ reference.doi
        , reference.pubmed
        , reference.csd
        , reference.issn
        , reference.astm
        ]


chainToString : Chain -> String
chainToString chain =
    "id:[" ++ chain.chain_id ++ "]sequence:" ++ chain.chain_seq_unnat


authorToString : Author -> String
authorToString author =
    author.forename ++ " " ++ author.surname


authorsToString : List Author -> String
authorsToString authors =
    String.join ";" <| List.map authorToString authors


xtalToString : Xtal -> String
xtalToString xtal =
    String.join ";" [ xtal.length_a, xtal.length_b, xtal.length_c, xtal.angle_a, xtal.angle_b, xtal.angle_g ]


stringToClassification : String -> Classification
stringToClassification string =
    case string of
        "minimal" ->
            Minimal

        "rational" ->
            Rational

        "engineered" ->
            Engineered

        "computational" ->
            Computational

        "physics-based" ->
            Phys

        "deep learning-based" ->
            DeepLearning

        "consensus" ->
            Consensus

        _ ->
            Other


classificationToString : Classification -> String
classificationToString classification =
    case classification of
        Minimal ->
            "minimal"

        Rational ->
            "rational"

        Engineered ->
            "engineered"

        Computational ->
            "computational"

        Phys ->
            "physics-based"

        DeepLearning ->
            "deep learning-based"

        Consensus ->
            "consensus"

        Other ->
            "other"


classificationToColour : Classification -> String
classificationToColour classification =
    case classification of
        Minimal ->
            "#ff0000"

        Rational ->
            "#ffff00"

        Engineered ->
            "#0000ff"

        Computational ->
            "#000000"

        Phys ->
            "#800080"

        DeepLearning ->
            "008000"

        Consensus ->
            "ff00ff"

        Other ->
            "#333333"


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


tagsToString : List Tag -> String
tagsToString tags =
    String.join ", " <| List.map tagToString tags


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



-- {{{ Views


{-| A simple view that shows basic data about a design. Used for lists etc.
-}
designCard : Element.Length -> ProteinDesignStub -> Element msg
designCard widthDesignCard design =
    link
        [ width <| widthDesignCard
        , clip
        ]
        { url = "/designs/" ++ design.pdb
        , label =
            row
                [ width <| widthDesignCard
                , spacing 4
                , padding 4
                , mouseOver [ Background.color <| rgb255 235 235 235 ]
                ]
                [ el
                    [ Border.width 2
                    , Border.color <| rgb255 220 220 220
                    ]
                    (image
                        [ width <| px 50
                        ]
                        { src = design.picture_path, description = "Image of design " ++ design.pdb }
                    )
                , column
                    [ padding 2
                    , spacing 2
                    , width fill
                    , alignTop
                    ]
                    [ paragraph [ Font.size 16 ] [ text <| String.toUpper <| design.pdb ]
                    , paragraph [ Font.size 11, Font.color <| rgb255 130 130 130 ] [ wrappedRow [] [ text design.subtitle ] ]
                    , paragraph [ Font.size 11 ] [ wrappedRow [] [ text (authorsToString design.authors) ] ]
                    ]
                ]
        }
