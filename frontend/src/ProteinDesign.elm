module ProteinDesign exposing (..)

import Date exposing (Date, Unit(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Month(..))


type alias ProteinDesign =
    { pdb : String
    , picture_path : String
    , chains : List Chain
    , pdb_string : String
    , authors : List Author
    , classification : Classification
    , classification_suggested : List Classification
    , classification_suggested_reason : List String
    , subtitle : String
    , tags : List String -- List Tag
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


rawDesignDecoder : Decoder ProteinDesign
rawDesignDecoder =
    Decode.succeed ProteinDesign
        |> required "pdb" string
        |> required "picture_path" string
        |> required "chains" (list chainDecoder)
        |> required "pdb_string" string
        |> required "authors" (list authorDecoder)
        |> required "classification" classificationDecoder
        |> required "classification_suggested" (list classificationDecoder)
        |> required "classification_suggested_reason" (list string)
        |> required "subtitle" string
        |> required "tags" (list string)
        |> required "keywords" (list string)
        |> required "release_date" dateDecoder
        |> required "publication" string
        |> required "publication_ref" referenceDecoder
        |> required "publication_country" string
        |> required "abstract" string
        |> required "related_pdb" (list string)
        |> required "crystal_structure" xtalDecoder
        |> required "exptl_method" (list string)
        |> required "formula_weight" float
        |> required "synthesis_comment" string
        |> required "review" bool
        |> optional "previous_design" string "/"
        |> optional "next_design" string "/"


rawDesignStubDecoder : Decoder ProteinDesignStub
rawDesignStubDecoder =
    Decode.succeed ProteinDesignStub
        |> required "pdb" string
        |> required "picture_path" string
        |> required "authors" (list authorDecoder)
        |> required "subtitle" string
        |> required "tags" (list string)
        |> required "keywords" (list string)
        |> required "release_date" dateDecoder


classificationDecoder : Decoder Classification
classificationDecoder =
    Decode.map stringToClassification string


referenceDecoder : Decoder Reference
referenceDecoder =
    Decode.succeed Reference
        |> required "DOI" string
        |> required "PubMed" string
        |> required "CSD" string
        |> required "ISSN" string
        |> required "ASTM" string


type alias Reference =
    { doi : String
    , pubmed : String
    , csd : String
    , issn : String
    , astm : String
    }


dateDecoder : Decoder Date
dateDecoder =
    string
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


designSearchableText : ProteinDesign -> String
designSearchableText proteinDesign =
    [ proteinDesign.pdb
    , String.join " " <| List.map chainToString proteinDesign.chains
    , authorsToString proteinDesign.authors
    , classificationToString proteinDesign.classification
    , proteinDesign.subtitle
    , String.join " " proteinDesign.tags
    , String.join " " proteinDesign.keywords
    , Date.toIsoString proteinDesign.release_date
    , proteinDesign.publication
    , proteinDesign.publication
    , proteinDesign.abstract
    , String.join " " proteinDesign.related_pdb
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
    chain.chain_id ++ " " ++ chain.chain_seq


authorToString : Author -> String
authorToString author =
    author.forename ++ " " ++ author.surname


authorsToString : List Author -> String
authorsToString authors =
    String.join ", " <| List.map authorToString authors


xtalToString : Xtal -> String
xtalToString xtal =
    String.join ", " [ xtal.length_a, xtal.length_b, xtal.length_c, xtal.angle_a, xtal.angle_b, xtal.angle_g ]


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
designCard : ProteinDesignStub -> Element msg
designCard design =
    link
        [ width <| px 460
        , clip
        ]
        { url = "/designs/" ++ design.pdb
        , label =
            row
                [ width fill
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
                    [ padding 2, spacing 2, width (fill |> minimum 200), alignTop ]
                    [ paragraph [ Font.size 16 ] [ text <| String.toUpper <| design.pdb ]
                    , paragraph [ Font.color <| rgb255 130 130 130, Font.size 11 ] [ text (authorsToString design.authors) ]
                    ]
                ]
        }
