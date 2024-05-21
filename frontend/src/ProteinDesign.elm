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
    , authors : List Author
    , classification : Classification
    , keyword : List String
    , tags : List String -- List Tag
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
    , previousDesign : String
    , nextDesign : String
    }


type alias ProteinDesignStub =
    { pdb : String
    , picture_path : String
    , authors : List Author
    , release_date : Date
    }


type Classification
    = Minimal
    | Rational
    | Engineered
    | CompPhys
    | CompDL
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
        |> required "authors" (list authorDecoder)
        |> required "classification" classificationDecoder
        |> required "keyword" (list string)
        |> required "tags" (list string)
        |> required "release_date" dateDecoder
        |> required "publication_title" titleDecoder
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
        |> optional "previous_design" string "/"
        |> optional "next_design" string "/"


rawDesignStubDecoder : Decoder ProteinDesignStub
rawDesignStubDecoder =
    Decode.succeed ProteinDesignStub
        |> required "pdb" string
        |> required "picture_path" string
        |> required "authors" (list authorDecoder)
        |> required "release_date" dateDecoder


classificationDecoder : Decoder Classification
classificationDecoder =
    Decode.map stringToClassification string


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


titleDecoder : Decoder String
titleDecoder =
    Decode.map
        (\titleString ->
            if String.endsWith "." titleString then
                String.dropRight 1 titleString

            else
                titleString
        )
        string


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
    , String.join " " proteinDesign.keyword
    , String.join " " proteinDesign.tags -- tagsToString proteinDesign.tags
    , Date.toIsoString proteinDesign.release_date
    , designToCitation proteinDesign
    , proteinDesign.abstract
    , proteinDesign.related_pdb
    , xtalToString proteinDesign.crystal_structure
    ]
        |> String.join "\n"
        |> String.toLower


stubSearchableText : ProteinDesignStub -> String
stubSearchableText proteinDesign =
    [ proteinDesign.pdb
    , authorsToString proteinDesign.authors
    , Date.toIsoString proteinDesign.release_date
    ]
        |> String.join "\n"
        |> String.toLower


designToPageRange : ProteinDesign -> String
designToPageRange proteinDesign =
    if String.isEmpty proteinDesign.publication_page_first || String.isEmpty proteinDesign.publication_page_last then
        ""

    else
        proteinDesign.publication_page_first ++ "-" ++ proteinDesign.publication_page_last


designToCitation : ProteinDesign -> String
designToCitation proteinDesign =
    String.join ", " <|
        [ proteinDesign.publication_title
        , proteinDesign.publication_journal_abbrev
        , proteinDesign.publication_journal_volume
        , designToPageRange proteinDesign
        , proteinDesign.publication_id_astm
        , proteinDesign.publication_id_issn
        , proteinDesign.publication_id_csd
        , proteinDesign.publication_id_doi
        , proteinDesign.publication_id_pubmed
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

        "computational physics-based" ->
            CompPhys

        "computational, Deep Learning" ->
            CompDL

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

        CompPhys ->
            "computational physics-based"

        CompDL ->
            "computational, Deep Learning"

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

        CompPhys ->
            "#800080"

        CompDL ->
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
