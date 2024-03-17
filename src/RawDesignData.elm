module RawDesignData exposing (..)

import Date
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import ProteinDesign exposing (ProteinDesign)
import Time exposing (Month(..))



-- TYPES


type alias RawDesignData =
    { identifier : String
    , data : Data
    , classification : String
    , keywords : String
    , date : String
    , authors : List Author
    , doi : String
    , pubmed_id : Int
    , abstract : String
    }


rawDesignDecoder : Decoder RawDesignData
rawDesignDecoder =
    Decode.succeed RawDesignData
        |> required "PDB" string
        |> required "Data" dataDecoder
        |> required "Classification" string
        |> required "Keywords" string
        |> required "Date" string
        |> required "Authors" (list authorDecoder)
        |> required "DOI" string
        |> required "Pubmed ID" int
        |> required "Abstract" string


toProteinDesign : RawDesignData -> Maybe ProteinDesign
toProteinDesign rawData =
    let
        pdbCode =
            String.toLower rawData.identifier

        structuralKeywords =
            ProteinDesign.stringToKeyword <| String.trim rawData.keywords

        depositionDate =
            Date.fromIsoString rawData.date
                |> Result.withDefault (Date.fromCalendarDate 1900 Jan 1)

        picturePath =
            "https://cdn.rcsb.org/images/structures/"
                ++ String.slice 1 3 pdbCode
                ++ "/"
                ++ pdbCode
                ++ "/"
                ++ pdbCode
                ++ "_assembly-1.jpeg"

        doiLink =
            rawData.doi

        sequences =
            List.concatMap (\polyEntity -> extractSequencesFromPolyEntity polyEntity) rawData.data.polymerEntities

        classification =
            ProteinDesign.stringToClassfication rawData.classification

        authors =
            rawData.authors
                |> List.map (\author -> String.join "" author.forename ++ " " ++ String.join " " author.surname)
                |> String.join ", "

        pubmedID =
            rawData.pubmed_id

        abstract =
            rawData.abstract
    in
    ProteinDesign pdbCode structuralKeywords depositionDate picturePath doiLink sequences classification authors pubmedID abstract |> Just


extractSequencesFromPolyEntity : PolyEntities -> List String
extractSequencesFromPolyEntity polyEntity =
    let
        entityPoly =
            polyEntity.entity_poly
    in
    [ entityPoly.pdbx_seq_one_letter_code_can ]


type alias NonpolymerComp =
    { chem_comp_id : String
    , chem_comp_name : String
    }


type alias NonpolymerEntity =
    { nonpolymer_comp : NonpolymerComp }


type alias PolymerEntityInstanceCount =
    { polymer_entity_instance_count : Int }


type alias RcsbAssemblyInfo =
    { rcsb_assembly_info : PolymerEntityInstanceCount }


type alias EntityPoly =
    { pdbx_seq_one_letter_code_can : String }


type alias PolyEntities =
    { entity_poly : EntityPoly }


type alias StructKeywords =
    { pdbx_keywords : String }


type alias Refine =
    { b_iso_mean : Maybe Float }


type alias RcsbPrimaryCitation =
    { pdbx_database_id_DOI : Maybe String }


type alias RcsbEntryInfo =
    { deposited_nonpolymer_entity_instance_count : Int
    , deposited_polymer_entity_instance_count : Int
    , disulfide_bond_count : Int
    , polymer_composition : String
    , resolution_combined : Maybe (List Float)
    }


type alias RcsbEntryContainerIdentifiers =
    { entry_id : String }


type alias RcsbAccessionInfo =
    { deposit_date : String }


type alias ExptlCrystalGrow =
    { method : Maybe String
    , pH : Maybe Float
    }


type alias Exptl =
    { method : String }


type alias Author =
    { forename : List String
    , surname : List String
    }


type alias Data =
    { rcsb_id : String
    , em3DReconstruction : Maybe String
    , exptl : List Exptl
    , exptlCrystalGrow : Maybe ExptlCrystalGrow
    , rcsbAccessionInfo : RcsbAccessionInfo
    , rcsbEntryContainerIdentifiers : RcsbEntryContainerIdentifiers
    , rcsbEntryInfo : RcsbEntryInfo
    , rcsbPrimaryCitation : RcsbPrimaryCitation
    , refine : Maybe Refine
    , structKeywords : StructKeywords
    , polymerEntities : List PolyEntities
    , assemblies : List RcsbAssemblyInfo
    , nonpolymerEntities : Maybe (List NonpolymerEntity)
    }



-- DECODERS


nonpolymerCompDecoder : Decoder NonpolymerComp
nonpolymerCompDecoder =
    Decode.succeed NonpolymerComp
        |> requiredAt [ "chem_comp", "id" ] string
        |> requiredAt [ "chem_comp", "name" ] string


nonpolymerEntitiesDecoder : Decoder NonpolymerEntity
nonpolymerEntitiesDecoder =
    Decode.succeed NonpolymerEntity
        |> required "nonpolymer_comp" nonpolymerCompDecoder


polymerEntityInstanceCountDecoder : Decoder PolymerEntityInstanceCount
polymerEntityInstanceCountDecoder =
    Decode.succeed PolymerEntityInstanceCount
        |> required "polymer_entity_instance_count" int


rcsbAssemblyInfoDecoder : Decoder RcsbAssemblyInfo
rcsbAssemblyInfoDecoder =
    Decode.succeed RcsbAssemblyInfo
        |> required "rcsb_assembly_info" polymerEntityInstanceCountDecoder


entityPolyDecoder : Decoder EntityPoly
entityPolyDecoder =
    Decode.succeed EntityPoly
        |> required "pdbx_seq_one_letter_code_can" string


polymerEntitiesDecoder : Decoder PolyEntities
polymerEntitiesDecoder =
    Decode.succeed PolyEntities
        |> required "entity_poly" entityPolyDecoder


structKeywordsDecoder : Decoder StructKeywords
structKeywordsDecoder =
    Decode.succeed StructKeywords
        |> required "pdbx_keywords" string



-- TODO make type with fields "PROTEIN BINDING",
-- "DE NOVO PROTEIN", "STRUCTURAL PROTEIN", "UNKNOWN FUNCTION",
-- "DESIGNED PEPTIDE", "METAL BINDING PROTEIN", "COILED COIL"
-- "SYNTHETIC PROTEIN MODEL", "ALPHA-HELICAL BUNDLE", "TRANSCRIPTION"
-- "NOVEL SEQUENCE", "BETA BETA ALPHA MOTIF", "GROWTH RESPONSE PROTEIN"
-- TODO? single "DE NOVO PROTEIN DESIGN" -> change in the data file?


refineDecoder : Decoder Refine
refineDecoder =
    Decode.succeed Refine
        |> optional "b_iso_mean" (Decode.nullable float) Nothing



-- ## "B_iso_mean" in JSON changed to "b_iso_mean"


rcsbPrimaryCitationDecoder : Decoder RcsbPrimaryCitation
rcsbPrimaryCitationDecoder =
    Decode.succeed RcsbPrimaryCitation
        |> optional "pdbx_database_id_DOI" (Decode.nullable string) Nothing


rcsbEntryInfoDecoder : Decoder RcsbEntryInfo
rcsbEntryInfoDecoder =
    Decode.succeed RcsbEntryInfo
        |> required "deposited_nonpolymer_entity_instance_count" int
        |> required "deposited_polymer_entity_instance_count" int
        |> required "disulfide_bond_count" int
        |> required "polymer_composition" string
        |> optional "resolution_combined" (Decode.nullable (list float)) Nothing


rcsbEntryContainerIdentifiersDecoder : Decoder RcsbEntryContainerIdentifiers
rcsbEntryContainerIdentifiersDecoder =
    Decode.succeed RcsbEntryContainerIdentifiers
        |> required "entry_id" string


rcsbAccessionInfoDecoder : Decoder RcsbAccessionInfo
rcsbAccessionInfoDecoder =
    Decode.succeed RcsbAccessionInfo
        |> required "deposit_date" string


exptlCrystalGrowDecoder : Decoder ExptlCrystalGrow
exptlCrystalGrowDecoder =
    Decode.succeed ExptlCrystalGrow
        |> optional "method" (Decode.nullable string) Nothing
        |> optional "pH" (Decode.nullable float) Nothing


exptlDecoder : Decoder Exptl
exptlDecoder =
    Decode.succeed Exptl
        |> required "method" string


authorDecoder : Decoder Author
authorDecoder =
    Decode.succeed Author
        |> required "forename" (list string)
        |> required "surname" (list string)


dataDecoder : Decoder Data
dataDecoder =
    Decode.succeed Data
        |> required "rcsb_id" string
        |> optional "em_3d_reconstruction" (Decode.nullable string) Nothing
        |> required "exptl" (list exptlDecoder)
        |> optional "exptl_crystal_grow" (Decode.nullable exptlCrystalGrowDecoder) Nothing
        |> required "rcsb_accession_info" rcsbAccessionInfoDecoder
        |> required "rcsb_entry_container_identifiers" rcsbEntryContainerIdentifiersDecoder
        |> required "rcsb_entry_info" rcsbEntryInfoDecoder
        |> required "rcsb_primary_citation" rcsbPrimaryCitationDecoder
        |> optional "refine" (Decode.nullable refineDecoder) Nothing
        |> required "struct_keywords" structKeywordsDecoder
        |> required "polymer_entities" (list polymerEntitiesDecoder)
        |> required "assemblies" (list rcsbAssemblyInfoDecoder)
        |> optional "nonpolymer_entities" (Decode.nullable (list nonpolymerEntitiesDecoder)) Nothing
