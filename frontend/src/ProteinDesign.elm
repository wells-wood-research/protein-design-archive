module ProteinDesign exposing (..)

import Csv.Encode as CsvEncode
import Date exposing (Date, Unit(..))
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as JsonEncode exposing (..)
import String exposing (fromFloat)
import Style
import Time exposing (Month(..))
import Urls exposing (..)



-- HELPERS --


threeToOne : Dict.Dict String String
threeToOne =
    Dict.fromList
        [ ( "ALA", "A" )
        , ( "ARG", "R" )
        , ( "ASN", "N" )
        , ( "ASP", "D" )
        , ( "CYS", "C" )
        , ( "GLN", "Q" )
        , ( "GLU", "E" )
        , ( "GLY", "G" )
        , ( "HIS", "H" )
        , ( "ILE", "I" )
        , ( "LEU", "L" )
        , ( "LYS", "K" )
        , ( "MET", "M" )
        , ( "PHE", "F" )
        , ( "PRO", "P" )
        , ( "SER", "S" )
        , ( "THR", "T" )
        , ( "TRP", "W" )
        , ( "TYR", "Y" )
        , ( "VAL", "V" )
        , ( "UNK", "X" )
        ]


maybeFloatToJson : Maybe Float -> JsonEncode.Value
maybeFloatToJson mf =
    case mf of
        Nothing ->
            JsonEncode.null

        Just f ->
            JsonEncode.float f


listToObject : List ( String, Float ) -> JsonEncode.Value
listToObject kvs =
    JsonEncode.object (List.map (\( k, v ) -> ( k, JsonEncode.float v )) kvs)


dictNullableToListDecoder : Decoder (List ( String, Float ))
dictNullableToListDecoder =
    Decode.dict (Decode.nullable Decode.float)
        |> Decode.map
            (Dict.toList
                >> List.filterMap
                    (\( k, mv ) -> Maybe.map (Tuple.pair k) mv)
            )



-- TYPE DEFINITIONS --


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
    , symmetry : String
    , crystal_structure : Xtal
    , exptl_method : List String
    , formula_weight : Float
    , synthesis_comment : String
    , previous_design : String
    , next_design : String
    , seq_thr_sim_designed : List Related
    , seq_thr_sim_natural : List Related
    , struct_thr_sim_designed : List Related
    , struct_thr_sim_natural : List Related
    , seq_max_sim_designed : Related
    , seq_max_sim_natural : Related
    , struct_max_sim_designed : Related
    , struct_max_sim_natural : Related
    , cath_full : List Cath
    , cath_class : List Cath
    , cath_arch : List Cath
    , physicochem : Physicochemical
    , review_comment : List String
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
    , seq_max_sim_natural : Related
    , struct_max_sim_natural : Related
    , seq_max_sim_designed : Related
    , struct_max_sim_designed : Related
    , cath_full : List Cath
    , cath_class : List Cath
    , cath_arch : List Cath
    }


type alias ProteinDesignDownload =
    { pdb : String
    , chains : List Chain
    , authors : List Author
    , classification : Classification
    , subtitle : String
    , tags : List String
    , release_date : Date
    , publication : String
    , publication_ref : Reference
    , publication_country : String
    , symmetry : String
    , crystal_structure : Xtal
    , exptl_method : List String
    , formula_weight : Float
    , synthesis_comment : String
    , physicochem : Physicochemical
    , seq_thr_sim_designed : List Related
    , seq_thr_sim_natural : List Related
    , struct_thr_sim_designed : List Related
    , struct_thr_sim_natural : List Related
    , seq_max_sim_designed : Related
    , seq_max_sim_natural : Related
    , struct_max_sim_designed : Related
    , struct_max_sim_natural : Related
    , cath_full : List Cath
    , review_comment : List String
    }


type alias Related =
    { similarity : Float
    , partner : String
    }


type alias Cath =
    { code : String
    , name : String
    }


type alias CathClassGroup =
    { classCode : String
    , className : String
    , archs : List Cath
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
    , chain_source : String
    , chain_type : String
    , chain_seq_unnat : String
    , chain_seq_nat : String
    , chain_length : Int
    }


type alias Xtal =
    { length_a : String
    , length_b : String
    , length_c : String
    , angle_a : String
    , angle_b : String
    , angle_g : String
    }


type alias Solubility =
    { total : Float
    , avg : Float
    , min : Float
    , max : Float
    }


type alias Dssp =
    { full_seq : String
    , dssp_seq : String
    }



-- Physicochemical properties


type alias Bude =
    { budeff_total : Maybe Float
    , budeff_steric : Maybe Float
    , budeff_desolvation : Maybe Float
    , budeff_charge : Maybe Float
    }


type alias Dfire2 =
    { dfire2_total : Maybe Float
    }


type alias Evoef2 =
    { evoef2_total : Maybe Float
    , evoef2_ref_total : Maybe Float
    , evoef2_intraR_total : Maybe Float
    , evoef2_interS_total : Maybe Float
    , evoef2_interD_total : Maybe Float
    }


type alias RosettaEnergy =
    { rosetta_total : Maybe Float
    , rosetta_vdw_atr : Maybe Float
    , rosetta_vdw_rep : Maybe Float
    , rosetta_vdw_intra_rep : Maybe Float
    , rosetta_electrostatics : Maybe Float
    , rosetta_solvation_isotropic : Maybe Float
    , rosetta_solvation_anisotropic_polar_atoms : Maybe Float
    , rosetta_solvation_isotropic_iR : Maybe Float
    , rosetta_hbond_lr_bb : Maybe Float
    , rosetta_hbond_sr_bb : Maybe Float
    , rosetta_hbond_bb_sc : Maybe Float
    , rosetta_hbond_sc : Maybe Float
    , rosetta_disulfides : Maybe Float
    , rosetta_backbone_torsion_preference : Maybe Float
    , rosetta_aa_propensity : Maybe Float
    , rosetta_dunbrack_rotamer : Maybe Float
    , rosetta_omega : Maybe Float
    , rosetta_pro_close : Maybe Float
    , rosetta_yhh_planarity : Maybe Float
    }


type alias Energy =
    { bude : Maybe Bude
    , dfire2 : Maybe Dfire2
    , evoef2 : Maybe Evoef2
    , rosetta : Maybe RosettaEnergy
    }


type alias Physicochemical =
    { num_residues : Maybe Float
    , mass : Maybe Float
    , charge : Maybe Float
    , isoelectric_point : Maybe Float
    , packing_density : Maybe Float
    , hydrophobic_fitness : Maybe Float
    , solubility : Maybe Solubility
    , dssp : Maybe Dssp
    , aa_composition : List ( String, Float )
    , ss_composition : List ( String, Float )
    , energy : Energy
    }


type alias DesignDetails msg =
    { header : String
    , property : Element msg
    }



-- cath type --


uniqueCathClasses : List ProteinDesignStub -> List Cath
uniqueCathClasses designs =
    designs
        |> List.concatMap .cath_class
        |> List.foldl
            (\c acc ->
                if List.any (\a -> a.code == c.code) acc then
                    acc

                else
                    c :: acc
            )
            []
        |> List.sortBy .code


uniqueCathArchs : List ProteinDesignStub -> List Cath
uniqueCathArchs designs =
    designs
        |> List.concatMap .cath_arch
        |> List.foldl
            (\arch acc ->
                if List.any (\a -> a.code == arch.code) acc then
                    acc

                else
                    arch :: acc
            )
            []
        |> List.sortBy .code


groupCathArchsByClass :
    List Cath
    -> List Cath
    -> List CathClassGroup
groupCathArchsByClass classes archs =
    classes
        |> List.map
            (\cls ->
                { classCode = cls.code
                , className = cls.name
                , archs =
                    archs
                        |> List.filter
                            (\arch ->
                                cathClassCode arch == cls.code
                            )
                }
            )


defaultCath : Cath
defaultCath =
    { code = "0"
    , name = "unknown"
    }


emptyArrayAsDefaultCath : Decoder Cath
emptyArrayAsDefaultCath =
    Decode.oneOf
        [ Decode.list Decode.value
            |> Decode.andThen
                (\val ->
                    if List.isEmpty val then
                        Decode.succeed defaultCath

                    else
                        Decode.fail "Expected an empty array."
                )
        , Decode.null defaultCath
        ]



-- related type --


defaultRelated : Related
defaultRelated =
    { similarity = 0.0
    , partner = ""
    }


emptyArrayAsDefaultRelated : Decoder Related
emptyArrayAsDefaultRelated =
    Decode.oneOf
        [ Decode.list Decode.value
            |> Decode.andThen
                (\val ->
                    if List.isEmpty val then
                        Decode.succeed defaultRelated

                    else
                        Decode.fail "Expected an empty array."
                )
        , Decode.null defaultRelated
        ]


relatedDetail : Related -> String -> Element msg
relatedDetail related baseUrl =
    row []
        [ newTabLink
            [ Font.color <| rgb255 104 176 171
            , Font.underline
            ]
            { url = baseUrl ++ related.partner
            , label = text <| related.partner
            }
        , text <| "(" ++ (String.left 4 <| String.fromFloat related.similarity) ++ ")"
        ]



-- DONWLOAD AREA --


type DownloadFileType
    = Csv
    | Json


fileTypeToString : DownloadFileType -> String
fileTypeToString fileType =
    case fileType of
        Csv ->
            "csv"

        Json ->
            "json"


csvListFromProteinDesignDownload : ProteinDesignDownload -> List ( String, String )
csvListFromProteinDesignDownload proteinDesign =
    [ ( "pdb_code", proteinDesign.pdb )
    , ( "release_date", Date.toIsoString proteinDesign.release_date )
    , ( "classification", classificationToString proteinDesign.classification )
    , ( "chains", String.join "|" (List.map chainToString proteinDesign.chains) )
    , ( "formula_weight", fromFloat proteinDesign.formula_weight )
    , ( "physicochemical_properties", JsonEncode.encode 0 (physicochemicalEncoder proteinDesign.physicochem) )
    , ( "symmetry", proteinDesign.symmetry )
    , ( "crystal_structure(a|b|c|alpha|beta|gamma)", xtalToString proteinDesign.crystal_structure )
    , ( "exptl_method", String.join "|" proteinDesign.exptl_method )
    , ( "synthesis_comment", proteinDesign.synthesis_comment )
    , ( "authors", authorsToString proteinDesign.authors )
    , ( "publication", proteinDesign.publication )
    , ( "publication_ref(doi|pubmed|csd|issn|astm)", refToString proteinDesign.publication_ref )
    , ( "publication_country", proteinDesign.publication_country )
    , ( "subtitle", proteinDesign.subtitle )
    , ( "tags", String.join ";" proteinDesign.tags )
    , ( "sequence_related_designs_above_50_bits", String.join ";" <| List.map (\related -> relatedToString related) proteinDesign.seq_thr_sim_designed )
    , ( "sequence_related_natural_proteins_above_50_bits", String.join ";" <| List.map (\related -> relatedToString related) proteinDesign.seq_thr_sim_natural )
    , ( "structure_related_designs_above_95_lddt", String.join ";" <| List.map (\related -> relatedToString related) proteinDesign.struct_thr_sim_designed )
    , ( "structure_related_natural_above_95_lddt", String.join ";" <| List.map (\related -> relatedToString related) proteinDesign.struct_thr_sim_natural )
    , ( "highest_sequence_related_design", relatedToString proteinDesign.seq_max_sim_designed )
    , ( "highest_sequence_related_natural_protein", relatedToString proteinDesign.seq_max_sim_natural )
    , ( "highest_structure_related_design", relatedToString proteinDesign.struct_max_sim_designed )
    , ( "highest_structure_related_natural_protein", relatedToString proteinDesign.struct_max_sim_natural )
    , ( "cath_full", String.join ";" <| List.map (\cath -> cathCodeToString cath) proteinDesign.cath_full )
    , ( "data_curation_comments", String.join ";" proteinDesign.review_comment )
    ]


csvStringFromProteinDesignDownload : List ProteinDesignDownload -> String
csvStringFromProteinDesignDownload proteinDesigns =
    proteinDesigns
        |> CsvEncode.encode
            { encoder =
                CsvEncode.withFieldNames
                    csvListFromProteinDesignDownload
            , fieldSeparator = ','
            }


jsonValueFromProteinDesignDownloadList : List ProteinDesignDownload -> JsonEncode.Value
jsonValueFromProteinDesignDownloadList proteinDesigns =
    JsonEncode.list jsonValueFromProteinDesignDownload proteinDesigns


jsonValueFromProteinDesignDownload : ProteinDesignDownload -> JsonEncode.Value
jsonValueFromProteinDesignDownload proteinDesign =
    JsonEncode.object
        [ ( "pdb", JsonEncode.string proteinDesign.pdb )
        , ( "release_date", JsonEncode.string <| Date.toIsoString proteinDesign.release_date )
        , ( "classification", JsonEncode.string <| classificationToString proteinDesign.classification )
        , ( "chains", JsonEncode.list chainEncoder proteinDesign.chains )
        , ( "physicochemical_properties", physicochemicalEncoder proteinDesign.physicochem )
        , ( "formula_weight", JsonEncode.float proteinDesign.formula_weight )
        , ( "exptl_method", JsonEncode.string <| String.join "" proteinDesign.exptl_method )
        , ( "symmetry", JsonEncode.string proteinDesign.symmetry )
        , ( "crystal_structure", xtalEncoder proteinDesign.crystal_structure )
        , ( "synthesis_comment", JsonEncode.string proteinDesign.synthesis_comment )
        , ( "authors", JsonEncode.list authorEncoder proteinDesign.authors )
        , ( "publication", JsonEncode.string proteinDesign.publication )
        , ( "publication_ref", referenceEncoder proteinDesign.publication_ref )
        , ( "publication_country", JsonEncode.string proteinDesign.publication_country )
        , ( "subtitle", JsonEncode.string proteinDesign.subtitle )
        , ( "tags", JsonEncode.list JsonEncode.string proteinDesign.tags )
        , ( "sequence_related_designs_above_50_bits", JsonEncode.list relatedEncoder proteinDesign.seq_thr_sim_designed )
        , ( "sequence_related_natural_proteins_above_50_bits", JsonEncode.list relatedEncoder proteinDesign.seq_thr_sim_natural )
        , ( "structure_related_designs_above_95_lddt", JsonEncode.list relatedEncoder proteinDesign.struct_thr_sim_designed )
        , ( "structure_related_natural_above_95_lddt", JsonEncode.list relatedEncoder proteinDesign.struct_thr_sim_natural )
        , ( "highest_sequence_related_design", relatedEncoder proteinDesign.seq_max_sim_designed )
        , ( "highest_sequence_related_natural_protein", relatedEncoder proteinDesign.seq_max_sim_natural )
        , ( "highest_structure_related_design", relatedEncoder proteinDesign.struct_max_sim_designed )
        , ( "highest_structure_related_natural_protein", relatedEncoder proteinDesign.struct_max_sim_natural )
        , ( "cath_full", JsonEncode.list cathEncoder proteinDesign.cath_full )
        , ( "data_curation_comments", JsonEncode.list JsonEncode.string proteinDesign.review_comment )
        ]


jsonStringFromProteinDesignDownload : List ProteinDesignDownload -> String
jsonStringFromProteinDesignDownload proteinDesigns =
    JsonEncode.encode 4 (jsonValueFromProteinDesignDownloadList proteinDesigns)



-- DECODER AREA --


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
        |> required "symmetry" Decode.string
        |> required "crystal_structure" xtalDecoder
        |> required "exptl_method" (Decode.list Decode.string)
        |> required "formula_weight" Decode.float
        |> required "synthesis_comment" Decode.string
        |> optional "previous_design" Decode.string "/"
        |> optional "next_design" Decode.string "/"
        |> required "seq_thr_sim_designed" (Decode.list relatedDecoder)
        |> required "seq_thr_sim_natural" (Decode.list relatedDecoder)
        |> required "struct_thr_sim_designed" (Decode.list relatedDecoder)
        |> required "struct_thr_sim_natural" (Decode.list relatedDecoder)
        |> required "seq_max_sim_designed" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "seq_max_sim_natural" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "struct_max_sim_designed" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "struct_max_sim_natural" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "cath_full" (Decode.list (Decode.oneOf [ cathDecoder, emptyArrayAsDefaultCath ]))
        |> required "cath_class" (Decode.list (Decode.oneOf [ cathDecoder, emptyArrayAsDefaultCath ]))
        |> required "cath_arch" (Decode.list (Decode.oneOf [ cathDecoder, emptyArrayAsDefaultCath ]))
        |> required "physicochemical_properties" physicochemicalDecoder
        |> required "review_comment" (Decode.list Decode.string)


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
        |> required "seq_max_sim_natural" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "struct_max_sim_natural" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "seq_max_sim_designed" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "struct_max_sim_designed" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "cath_full" (Decode.list (Decode.oneOf [ cathDecoder, emptyArrayAsDefaultCath ]))
        |> required "cath_class" (Decode.list (Decode.oneOf [ cathDecoder, emptyArrayAsDefaultCath ]))
        |> required "cath_arch" (Decode.list (Decode.oneOf [ cathDecoder, emptyArrayAsDefaultCath ]))


downloadDesignDecoder : Decoder ProteinDesignDownload
downloadDesignDecoder =
    Decode.succeed ProteinDesignDownload
        |> required "pdb" Decode.string
        |> required "chains" (Decode.list chainDecoder)
        |> required "authors" (Decode.list authorDecoder)
        |> required "classification" classificationDecoder
        |> required "subtitle" Decode.string
        |> required "tags" (Decode.list Decode.string)
        |> required "release_date" dateDecoder
        |> required "publication" Decode.string
        |> required "publication_ref" referenceDecoder
        |> required "publication_country" Decode.string
        |> required "symmetry" Decode.string
        |> required "crystal_structure" xtalDecoder
        |> required "exptl_method" (Decode.list Decode.string)
        |> required "formula_weight" Decode.float
        |> required "synthesis_comment" Decode.string
        |> required "physicochemical_properties" physicochemicalDecoder
        |> required "seq_thr_sim_designed" (Decode.list relatedDecoder)
        |> required "seq_thr_sim_natural" (Decode.list relatedDecoder)
        |> required "struct_thr_sim_designed" (Decode.list relatedDecoder)
        |> required "struct_thr_sim_natural" (Decode.list relatedDecoder)
        |> required "seq_max_sim_designed" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "seq_max_sim_natural" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "struct_max_sim_designed" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "struct_max_sim_natural" (Decode.oneOf [ relatedDecoder, emptyArrayAsDefaultRelated ])
        |> required "cath_full" (Decode.list cathDecoder)
        |> required "review_comment" (Decode.list Decode.string)


relatedDecoder : Decoder Related
relatedDecoder =
    Decode.succeed Related
        |> required "sim" Decode.float
        |> required "partner" Decode.string


cathDecoder : Decoder Cath
cathDecoder =
    Decode.succeed Cath
        |> required "code" Decode.string
        |> required "name" Decode.string


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
        |> required "chain_source" Decode.string
        |> required "chain_type" Decode.string
        |> required "chain_seq_unnat" Decode.string
        |> required "chain_seq_nat" Decode.string
        |> required "chain_length" Decode.int


xtalDecoder : Decoder Xtal
xtalDecoder =
    Decode.succeed Xtal
        |> required "length_a" Decode.string
        |> required "length_b" Decode.string
        |> required "length_c" Decode.string
        |> required "angle_a" Decode.string
        |> required "angle_b" Decode.string
        |> required "angle_g" Decode.string


solubilityDecoder : Decoder Solubility
solubilityDecoder =
    Decode.succeed Solubility
        |> required "total_value" Decode.float
        |> required "avg_value" Decode.float
        |> required "min_value" Decode.float
        |> required "max_value" Decode.float


dsspDecoder : Decoder Dssp
dsspDecoder =
    Decode.succeed Dssp
        |> required "full_sequence" Decode.string
        |> required "dssp_sequence" Decode.string


aaCompositionDecoder : Decoder (List ( String, Float ))
aaCompositionDecoder =
    Decode.dict (Decode.nullable Decode.float)
        |> Decode.map
            (Dict.toList
                >> List.filterMap
                    (\( k, mv ) ->
                        Maybe.map
                            (\v ->
                                let
                                    -- strip prefix
                                    suffix =
                                        String.split "_" k
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.withDefault k

                                    code =
                                        Dict.get (String.toUpper suffix) threeToOne
                                            |> Maybe.withDefault (String.toUpper suffix)
                                in
                                ( code, v )
                            )
                            mv
                    )
            )


ssCompositionDecoder : Decoder (List ( String, Float ))
ssCompositionDecoder =
    Decode.dict (Decode.nullable Decode.float)
        |> Decode.map
            (Dict.toList
                >> List.filterMap
                    (\( k, mv ) ->
                        Maybe.map
                            (\v ->
                                ( if String.startsWith "ss_prop_" k then
                                    String.dropLeft (String.length "ss_prop_") k

                                  else
                                    k
                                , v
                                )
                            )
                            mv
                    )
            )


budeDecoder : Decoder Bude
budeDecoder =
    Decode.succeed Bude
        |> optional "budeff_total" (Decode.maybe Decode.float) Nothing
        |> optional "budeff_steric" (Decode.maybe Decode.float) Nothing
        |> optional "budeff_desolvation" (Decode.maybe Decode.float) Nothing
        |> optional "budeff_charge" (Decode.maybe Decode.float) Nothing


dfire2Decoder : Decoder Dfire2
dfire2Decoder =
    Decode.succeed Dfire2
        |> optional "dfire2_total" (Decode.maybe Decode.float) Nothing


evoef2Decoder : Decoder Evoef2
evoef2Decoder =
    Decode.succeed Evoef2
        |> optional "evoef2_total" (Decode.maybe Decode.float) Nothing
        |> optional "evoef2_ref_total" (Decode.maybe Decode.float) Nothing
        |> optional "evoef2_intraR_total" (Decode.maybe Decode.float) Nothing
        |> optional "evoef2_interS_total" (Decode.maybe Decode.float) Nothing
        |> optional "evoef2_interD_total" (Decode.maybe Decode.float) Nothing


rosettaDecoder : Decoder RosettaEnergy
rosettaDecoder =
    Decode.succeed RosettaEnergy
        |> optional "rosetta_total" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_vdw_atr" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_vdw_rep" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_vdw_intra_rep" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_electrostatics" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_solvation_isotropic" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_solvation_anisotropic_polar_atoms" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_solvation_isotropic_iR" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_hbond_lr_bb" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_hbond_sr_bb" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_hbond_bb_sc" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_hbond_sc" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_disulfides" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_backbone_torsion_preference" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_aa_propensity" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_dunbrack_rotamer" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_omega" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_pro_close" (Decode.maybe Decode.float) Nothing
        |> optional "rosetta_yhh_planarity" (Decode.maybe Decode.float) Nothing


energyDecoder : Decoder Energy
energyDecoder =
    Decode.succeed Energy
        |> optional "budeff" (Decode.maybe budeDecoder) Nothing
        |> optional "dfire2" (Decode.maybe dfire2Decoder) Nothing
        |> optional "evoef2" (Decode.maybe evoef2Decoder) Nothing
        |> optional "rosetta" (Decode.maybe rosettaDecoder) Nothing


physicochemicalDecoder : Decoder Physicochemical
physicochemicalDecoder =
    Decode.succeed Physicochemical
        |> optional "num_residues" (Decode.maybe Decode.float) Nothing
        |> optional "mass" (Decode.maybe Decode.float) Nothing
        |> optional "charge" (Decode.maybe Decode.float) Nothing
        |> optional "isoelectric_point" (Decode.maybe Decode.float) Nothing
        |> optional "packing_density" (Decode.maybe Decode.float) Nothing
        |> optional "hydrophobic_fitness" (Decode.maybe Decode.float) Nothing
        |> optional "solubility" (Decode.maybe solubilityDecoder) Nothing
        |> optional "dssp" (Decode.maybe dsspDecoder) Nothing
        |> optional "aa_composition" aaCompositionDecoder []
        |> optional "ss_composition" ssCompositionDecoder []
        |> optional "energy" energyDecoder { bude = Nothing, dfire2 = Nothing, evoef2 = Nothing, rosetta = Nothing }



-- ENCODERS --


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


solubilityEncoder : Solubility -> JsonEncode.Value
solubilityEncoder sol =
    JsonEncode.object
        [ ( "aggrescan3d_total_value", JsonEncode.float sol.total )
        , ( "aggrescan3d_avg_value", JsonEncode.float sol.avg )
        , ( "aggrescan3d_min_value", JsonEncode.float sol.min )
        , ( "aggrescan3d_max_value", JsonEncode.float sol.max )
        ]


dsspEncoder : Dssp -> JsonEncode.Value
dsspEncoder dssp =
    JsonEncode.object
        [ ( "full_sequence", JsonEncode.string dssp.full_seq )
        , ( "dssp_sequence", JsonEncode.string dssp.dssp_seq )
        ]


authorEncoder : Author -> JsonEncode.Value
authorEncoder author =
    JsonEncode.object
        [ ( "forename", JsonEncode.string author.forename )
        , ( "surname", JsonEncode.string author.surname )
        ]


referenceEncoder : Reference -> JsonEncode.Value
referenceEncoder reference =
    JsonEncode.object
        [ ( "doi", JsonEncode.string reference.doi )
        , ( "pubmed", JsonEncode.string reference.pubmed )
        , ( "csd", JsonEncode.string reference.csd )
        , ( "issn", JsonEncode.string reference.issn )
        , ( "astm", JsonEncode.string reference.astm )
        ]


cathEncoder : Cath -> JsonEncode.Value
cathEncoder cath =
    JsonEncode.object
        [ ( "code", JsonEncode.string cath.code )
        , ( "name", JsonEncode.string cath.name )
        ]


relatedEncoder : Related -> JsonEncode.Value
relatedEncoder related =
    JsonEncode.object
        [ ( "sim", JsonEncode.float related.similarity )
        , ( "partner", JsonEncode.string related.partner )
        ]


budeEncoder : Bude -> JsonEncode.Value
budeEncoder b =
    JsonEncode.object
        [ ( "budeff_total", maybeFloatToJson b.budeff_total )
        , ( "budeff_steric", maybeFloatToJson b.budeff_steric )
        , ( "budeff_desolvation", maybeFloatToJson b.budeff_desolvation )
        , ( "budeff_charge", maybeFloatToJson b.budeff_charge )
        ]


dfire2Encoder : Dfire2 -> JsonEncode.Value
dfire2Encoder d =
    JsonEncode.object [ ( "dfire2_total", maybeFloatToJson d.dfire2_total ) ]


evoef2Encoder : Evoef2 -> JsonEncode.Value
evoef2Encoder e =
    JsonEncode.object
        [ ( "evoef2_total", maybeFloatToJson e.evoef2_total )
        , ( "evoef2_ref_total", maybeFloatToJson e.evoef2_ref_total )
        , ( "evoef2_intraR_total", maybeFloatToJson e.evoef2_intraR_total )
        , ( "evoef2_interS_total", maybeFloatToJson e.evoef2_interS_total )
        , ( "evoef2_interD_total", maybeFloatToJson e.evoef2_interD_total )
        ]


rosettaEncoder : RosettaEnergy -> JsonEncode.Value
rosettaEncoder r =
    JsonEncode.object
        [ ( "rosetta_total", maybeFloatToJson r.rosetta_total )
        , ( "rosetta_vdw_atr", maybeFloatToJson r.rosetta_vdw_atr )
        , ( "rosetta_vdw_rep", maybeFloatToJson r.rosetta_vdw_rep )
        , ( "rosetta_vdw_intra_rep", maybeFloatToJson r.rosetta_vdw_intra_rep )
        , ( "rosetta_electrostatics", maybeFloatToJson r.rosetta_electrostatics )
        , ( "rosetta_solvation_isotropic", maybeFloatToJson r.rosetta_solvation_isotropic )
        , ( "rosetta_solvation_anisotropic_polar_atoms", maybeFloatToJson r.rosetta_solvation_anisotropic_polar_atoms )
        , ( "rosetta_solvation_isotropic_iR", maybeFloatToJson r.rosetta_solvation_isotropic_iR )
        , ( "rosetta_hbond_lr_bb", maybeFloatToJson r.rosetta_hbond_lr_bb )
        , ( "rosetta_hbond_sr_bb", maybeFloatToJson r.rosetta_hbond_sr_bb )
        , ( "rosetta_hbond_bb_sc", maybeFloatToJson r.rosetta_hbond_bb_sc )
        , ( "rosetta_hbond_sc", maybeFloatToJson r.rosetta_hbond_sc )
        , ( "rosetta_disulfides", maybeFloatToJson r.rosetta_disulfides )
        , ( "rosetta_backbone_torsion_preference", maybeFloatToJson r.rosetta_backbone_torsion_preference )
        , ( "rosetta_aa_propensity", maybeFloatToJson r.rosetta_aa_propensity )
        , ( "rosetta_dunbrack_rotamer", maybeFloatToJson r.rosetta_dunbrack_rotamer )
        , ( "rosetta_omega", maybeFloatToJson r.rosetta_omega )
        , ( "rosetta_pro_close", maybeFloatToJson r.rosetta_pro_close )
        , ( "rosetta_yhh_planarity", maybeFloatToJson r.rosetta_yhh_planarity )
        ]


energyEncoder : Energy -> JsonEncode.Value
energyEncoder e =
    JsonEncode.object
        (List.filterMap identity
            [ Maybe.map (\b -> ( "budeff", budeEncoder b )) e.bude
            , Maybe.map (\d -> ( "dfire2", dfire2Encoder d )) e.dfire2
            , Maybe.map (\ev -> ( "evoef2", evoef2Encoder ev )) e.evoef2
            , Maybe.map (\r -> ( "rosetta", rosettaEncoder r )) e.rosetta
            ]
        )


physicochemicalEncoder : Physicochemical -> JsonEncode.Value
physicochemicalEncoder p =
    JsonEncode.object
        [ ( "num_residues", maybeFloatToJson p.num_residues )
        , ( "mass", maybeFloatToJson p.mass )
        , ( "charge", maybeFloatToJson p.charge )
        , ( "isoelectric_point", maybeFloatToJson p.isoelectric_point )
        , ( "packing_density", maybeFloatToJson p.packing_density )
        , ( "hydrophobic_fitness", maybeFloatToJson p.hydrophobic_fitness )
        , ( "solubility"
          , case p.solubility of
                Nothing ->
                    JsonEncode.null

                Just s ->
                    solubilityEncoder s
          )
        , ( "dssp"
          , case p.dssp of
                Nothing ->
                    JsonEncode.null

                Just d ->
                    dsspEncoder d
          )
        , ( "aa_composition", listToObject p.aa_composition )
        , ( "ss_composition", listToObject p.ss_composition )
        , ( "energy", energyEncoder p.energy )
        ]



-- TEXT PREOCESSING AREA --


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
    , String.join " " proteinDesign.exptl_method
    , proteinDesign.synthesis_comment
    , xtalToString proteinDesign.crystal_structure
    , String.join " " <| List.map cathCodeToString proteinDesign.cath_full
    , String.join " " <| List.map cathNameToString proteinDesign.cath_full
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
    , String.join " " <| List.map cathCodeToString proteinDesign.cath_full
    , String.join " " <| List.map cathNameToString proteinDesign.cath_full
    ]
        |> String.join "\n"
        |> String.toLower


refToString : Reference -> String
refToString reference =
    String.join "|" <|
        [ reference.doi
        , reference.pubmed
        , reference.csd
        , reference.issn
        , reference.astm
        ]


cathToString : Cath -> String
cathToString cath =
    if cath.code == "" then
        ""

    else
        cath.code ++ " — " ++ cath.name


cathCodeToString : Cath -> String
cathCodeToString cath =
    if cath.code == "" then
        ""

    else
        cath.code


cathNameToString : Cath -> String
cathNameToString cath =
    if cath.name == "" then
        ""

    else
        cath.name


relatedToString : Related -> String
relatedToString related =
    if related.partner == "" then
        ""

    else
        related.partner ++ "(" ++ String.fromFloat related.similarity ++ ")"


cathClassCode : Cath -> String
cathClassCode cath =
    cath.code
        |> String.split "."
        |> List.head
        |> Maybe.withDefault ""


chainToString : Chain -> String
chainToString chain =
    "id:"
        ++ chain.chain_id
        ++ ";type:"
        ++ chain.chain_type
        ++ ";source:"
        ++ chain.chain_source
        ++ ";length:"
        ++ String.fromInt chain.chain_length
        ++ ";seq_nat:"
        ++ chain.chain_seq_nat
        ++ ";seq_unnat:"
        ++ chain.chain_seq_unnat


authorToString : Author -> String
authorToString author =
    author.forename ++ " " ++ author.surname


authorsToString : List Author -> String
authorsToString authors =
    String.join ";" <| List.map authorToString authors


xtalToString : Xtal -> String
xtalToString xtal =
    String.join "|" [ xtal.length_a, xtal.length_b, xtal.length_c, xtal.angle_a, xtal.angle_b, xtal.angle_g ]


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



-- DESIGN DETAILS AREA: STUBS FOR HOME PAGE, DETAILED TABLES FOR INDIVIDUAL PAGES --


{-| A simple view that shows basic data about a design. Used for lists etc.
-}
designCard : Element.Length -> ProteinDesignStub -> Element msg
designCard widthDesignCard design =
    link
        [ width <| widthDesignCard
        , clip
        ]
        { url = Urls.internalRelatedLink ++ design.pdb
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
                    [ paragraph (Style.monospacedFont ++ [ Font.size 16 ]) [ text <| String.toUpper <| design.pdb ]
                    , paragraph [ Font.size 11, Font.color <| rgb255 130 130 130 ] [ wrappedRow [] [ text design.subtitle ] ]
                    , paragraph [ Font.size 11 ] [ wrappedRow [] [ text (authorsToString design.authors) ] ]
                    ]
                ]
        }


designDetailsFromProteinDesign : ProteinDesign -> List (DesignDetails msg)
designDetailsFromProteinDesign proteinDesign =
    [ { header = "PDB code"
      , property =
            newTabLink
                [ Font.color <| rgb255 104 176 171
                , Font.underline
                ]
                { url =
                    Urls.externalRelatedLink
                        ++ proteinDesign.pdb
                , label =
                    proteinDesign.pdb
                        |> text
                }
      }
    , { header = "Release date"
      , property =
            if String.isEmpty <| Date.toIsoString proteinDesign.release_date then
                text "-"

            else
                text <| Date.toIsoString proteinDesign.release_date
      }
    , { header = "Subtitle"
      , property =
            if String.isEmpty proteinDesign.subtitle then
                text "-"

            else
                text <| proteinDesign.subtitle
      }
    , { header = "CATH"
      , property =
            if List.isEmpty proteinDesign.cath_arch then
                text "unknown"

            else
                column [ width fill ] <|
                    (List.map (\cath -> text <| cathToString cath) <|
                        List.sortBy .code <|
                            proteinDesign.cath_arch
                    )
      }
    , { header = "Sequence related designs (bits)"
      , property =
            if List.isEmpty proteinDesign.seq_thr_sim_designed then
                if proteinDesign.seq_max_sim_designed == defaultRelated || proteinDesign.seq_max_sim_designed.partner == proteinDesign.pdb then
                    text "-"

                else
                    (\related -> relatedDetail related Urls.internalRelatedLink) proteinDesign.seq_max_sim_designed

            else
                row []
                    (List.intersperse (text "; ") <|
                        List.map (\related -> relatedDetail related Urls.internalRelatedLink) proteinDesign.seq_thr_sim_designed
                    )
      }
    , { header = "Sequence related proteins (bits)"
      , property =
            if List.isEmpty proteinDesign.seq_thr_sim_natural then
                if proteinDesign.seq_max_sim_natural == defaultRelated then
                    text "-"

                else
                    (\related -> relatedDetail related Urls.externalRelatedLink) proteinDesign.seq_max_sim_natural

            else
                row []
                    (List.intersperse (text "; ") <|
                        List.map (\related -> relatedDetail related Urls.externalRelatedLink) proteinDesign.seq_thr_sim_natural
                    )
      }
    , { header = "Structure related designs (LDDT)"
      , property =
            if List.isEmpty proteinDesign.struct_thr_sim_designed then
                if proteinDesign.struct_max_sim_designed == defaultRelated || proteinDesign.struct_max_sim_designed.partner == proteinDesign.pdb then
                    text "-"

                else
                    (\related -> relatedDetail related Urls.internalRelatedLink) proteinDesign.struct_max_sim_designed

            else
                row []
                    (List.intersperse (text ";") <|
                        List.map (\related -> relatedDetail related Urls.internalRelatedLink) proteinDesign.struct_thr_sim_designed
                    )
      }
    , { header = "Structure related proteins (LDDT)"
      , property =
            if List.isEmpty proteinDesign.struct_thr_sim_natural then
                if proteinDesign.struct_max_sim_natural == defaultRelated then
                    text "-"

                else
                    (\related -> relatedDetail related Urls.externalRelatedLink) proteinDesign.struct_max_sim_natural

            else
                row []
                    (List.intersperse (text "; ") <|
                        List.map (\related -> relatedDetail related Urls.externalRelatedLink) proteinDesign.struct_thr_sim_natural
                    )
      }
    , { header = "Authors"
      , property =
            if List.isEmpty proteinDesign.authors then
                text "-"

            else
                text <| authorsToString proteinDesign.authors
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
            if String.isEmpty proteinDesign.publication_ref.doi && String.isEmpty proteinDesign.publication_ref.pubmed then
                text "-"

            else
                newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url =
                        if String.isEmpty proteinDesign.publication_ref.doi then
                            if String.isEmpty proteinDesign.publication_ref.pubmed then
                                Urls.externalRelatedLink
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
    , { header = "Symmetry group"
      , property =
            if String.isEmpty proteinDesign.symmetry then
                text "-"

            else
                text <| proteinDesign.symmetry
      }
    , { header = "Experimental charact. method"
      , property =
            if List.isEmpty proteinDesign.exptl_method then
                text "-"

            else
                text <| String.join "," proteinDesign.exptl_method
      }
    , { header = "Synthesis comment"
      , property =
            if String.isEmpty proteinDesign.synthesis_comment then
                text "-"

            else
                text <| proteinDesign.synthesis_comment
      }
    , { header = "Formula weight"
      , property =
            if String.isEmpty <| String.fromFloat proteinDesign.formula_weight then
                text "-"

            else
                text <| String.fromFloat proteinDesign.formula_weight ++ " Da"
      }
    ]
