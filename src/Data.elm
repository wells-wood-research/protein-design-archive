module Data exposing (Design, getAllDesigns)

import Date
import Json.Decode as JDe
import Json.Decode.Pipeline exposing (hardcoded, required, requiredAt)
import Time exposing (Month(..))


-- Things to add:
-- * Crystallisation conditions
-- * Publicatoin doi and link
-- * All the pictures
-- * The design method
type alias Design =
    { pdbCode : String
    , depositionDate : Date.Date
    , method : String
    , picturePath : String
    , structuralKeywords : String
    , publicationLink : String
    , sequences : List String
    }


getAllDesigns : List Design
getAllDesigns =
    JDe.decodeString
        (designDecoder
            |> JDe.field "data"
            |> JDe.list
        )
        rawData
        |> Result.withDefault []


designDecoder : JDe.Decoder Design
designDecoder =
    JDe.succeed makeDesign
        |> required "rcsb_id" JDe.string
        |> requiredAt [ "rcsb_accession_info", "deposit_date" ] JDe.string
        |> required "exptl" (JDe.index 0 (JDe.field "method" JDe.string))
        |> hardcoded "blah"
        |> hardcoded "eep"
        |> hardcoded "doi"
        |> required "polymer_entities"
            (JDe.list
                (JDe.at [ "entity_poly", "pdbx_seq_one_letter_code_can" ] JDe.string)
            )


makeDesign : String -> String -> String -> String -> String -> String -> List String -> Design
makeDesign pdbCode depositionDate method picturePath structuralKeywords publicationLink sequences =
    { pdbCode = pdbCode
    , depositionDate =
        depositionDate
            |> String.split "T"
            |> List.head
            |> Maybe.withDefault "ERROR"
            |> Date.fromIsoString
            |> Result.withDefault (Date.fromCalendarDate 1900 Jan 1)
    , method = method
    , picturePath = "%PUBLIC_URL%/designs/1cos.jpg"
    , structuralKeywords = "ALPHA-HELICAL BUNDLE"
    , publicationLink = "10.1126/science.8446897"
    , sequences = sequences
    }


rawData : String
rawData =
    """
[
  {
    "identifier": "1AL1",
    "data": {
      "rcsb_id": "1AL1",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "1990-07-02T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1AL1"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 1,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.7
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": null
      },
      "refine": [
        {
          "B_iso_mean": null
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "SYNTHETIC PROTEIN MODEL"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XELLKKLLEELKG"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 6
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "SO4",
              "name": "SULFATE ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1BB1",
    "data": {
      "rcsb_id": "1BB1",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": null,
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "1998-04-28T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1BB1"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 1,
        "deposited_polymer_entity_instance_count": 3,
        "disulfide_bond_count": 0,
        "polymer_composition": "heteromeric protein",
        "resolution_combined": [
          1.8
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": null
      },
      "refine": [
        {
          "B_iso_mean": 18.8
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN DESIGN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XAEIAAIEYEQAAIKEEIAAIKDKIAAIKEYIAAIX"
          }
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEKIAAIKEEQAAIEEEIQAIKEEIAAIKYLIAQIX"
          }
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XAEIAAIKYKQAAIKNEIAAIKQEIAAIEQMIAAIX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 3
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "CL",
              "name": "CHLORIDE ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1BYZ",
    "data": {
      "rcsb_id": "1BYZ",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, SITTING DROP",
          "pH": 8
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "1998-10-20T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1BYZ"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 7,
        "deposited_polymer_entity_instance_count": 4,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          0.9
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": null
      },
      "refine": [
        {
          "B_iso_mean": null
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XELLKKLLEELKG"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 4
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "ETA",
              "name": "ETHANOLAMINE"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MPD",
              "name": "(4S)-2-METHYL-2,4-PENTANEDIOL"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "CL",
              "name": "CHLORIDE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MRD",
              "name": "(4R)-2-METHYLPENTANE-2,4-DIOL"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1COS",
    "data": {
      "rcsb_id": "1COS",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "1993-01-22T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1COS"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 3,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.1
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": null
      },
      "refine": [
        {
          "B_iso_mean": null
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "ALPHA-HELICAL BUNDLE"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEWEALEKKLAALESKLQALEKKLEALEHGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 3
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1DJF",
    "data": {
      "rcsb_id": "1DJF",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "1999-12-03T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1DJF"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/bi000208x"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "QAPAYKKAAKKLAES"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1EC5",
    "data": {
      "rcsb_id": "1EC5",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 4.6
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2000-01-25T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1EC5"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 3,
        "deposited_polymer_entity_instance_count": 3,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.5
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.97.12.6298"
      },
      "refine": [
        {
          "B_iso_mean": 41.4
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQLIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "ZN",
              "name": "ZINC ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1FME",
    "data": {
      "rcsb_id": "1FME",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2000-08-16T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1FME"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1006/jmbi.2000.4345"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "EQYTAKYKGRTFRNEKELRDFIEKFKGR"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1FMH",
    "data": {
      "rcsb_id": "1FMH",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2000-08-17T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1FMH"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 1,
        "polymer_composition": "heteromeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/bi001242e"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "TRANSCRIPTION"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVAQLEKEVAQAEAENYQLEQEVAQLEHECGX"
          }
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVQALKKRVQALKARNYAAKQKVQALRHKCGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1FSD",
    "data": {
      "rcsb_id": "1FSD",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "1997-06-09T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1FSD"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1126/science.278.5335.82"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "NOVEL SEQUENCE"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "QQYTAKIKGRTFRNEKELRDFIEKFKGR"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1FSV",
    "data": {
      "rcsb_id": "1FSV",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "1997-10-26T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1FSV"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1126/science.278.5335.82"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "BETA BETA ALPHA MOTIF"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "QQYTAKIKGRTFRNEKELRDFIEKFKGR"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1G6U",
    "data": {
      "rcsb_id": "1G6U",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION",
          "pH": 6.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2000-11-07T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1G6U"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 2,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.48
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.98.4.1404"
      },
      "refine": [
        {
          "B_iso_mean": 16.5
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SLAALKSELQALKKEGFSPEELAALESELQALEKKLAALKSKLQALKG"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 6
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "SO4",
              "name": "SULFATE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "TFA",
              "name": "trifluoroacetic acid"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1HCW",
    "data": {
      "rcsb_id": "1HCW",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "1996-09-20T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1HCW"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": null
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "GROWTH RESPONSE PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XYTVPSATFSRSDELAKLLRLHAGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1HQJ",
    "data": {
      "rcsb_id": "1HQJ",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 5.6
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2000-12-18T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1HQJ"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 25,
        "deposited_polymer_entity_instance_count": 12,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.2
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": null
      },
      "refine": [
        {
          "B_iso_mean": null
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "DELERRIRELEARIK"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 3
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 3
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 3
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 3
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 12
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "PB",
              "name": "LEAD (II) ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "SIN",
              "name": "SUCCINIC ACID"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "SO4",
              "name": "SULFATE ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1IC9",
    "data": {
      "rcsb_id": "1IC9",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-03-30T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1IC9"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1038/88604"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SKYEYTIPSYTFRGPGCPTLKPAITVRCE"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1ICL",
    "data": {
      "rcsb_id": "1ICL",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-04-02T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1ICL"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1038/88604"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SKYEYTVPSYTFRGPGCPTVKPAISLRCE"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1ICO",
    "data": {
      "rcsb_id": "1ICO",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-04-02T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1ICO"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1038/88604"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SKYEYTIPSYTFRGPGCPTVKPAVTIRCE"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1J4M",
    "data": {
      "rcsb_id": "1J4M",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-10-10T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1J4M"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.012583999"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "RGKWTYNGITYEGR"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1JM0",
    "data": {
      "rcsb_id": "1JM0",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2001-07-17T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JM0"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 14,
        "deposited_polymer_entity_instance_count": 6,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.7
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja010506x"
      },
      "refine": [
        {
          "B_iso_mean": 26.02
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQAIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MN",
              "name": "MANGANESE (II) ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "DMS",
              "name": "DIMETHYL SULFOXIDE"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1JMB",
    "data": {
      "rcsb_id": "1JMB",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2001-07-18T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JMB"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 5,
        "deposited_polymer_entity_instance_count": 3,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.2
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja010506x"
      },
      "refine": [
        {
          "B_iso_mean": 41.8
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQAIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MN",
              "name": "MANGANESE (II) ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "DMS",
              "name": "DIMETHYL SULFOXIDE"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1JY4",
    "data": {
      "rcsb_id": "1JY4",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-09-11T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JY4"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja0174276"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "RGECKFTVPGRTALNTPAVQKWHFVLPGYKCEILA"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1JY6",
    "data": {
      "rcsb_id": "1JY6",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-09-11T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JY6"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja0174276"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "RGECKFTVPGRTALNTPAVQKWHFVLPGYKCEILA"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1JY9",
    "data": {
      "rcsb_id": "1JY9",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-09-11T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JY9"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.211536998"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "TTTTRYVEVPGKKILQTTTT"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1K09",
    "data": {
      "rcsb_id": "1K09",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-09-18T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1K09"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 0,
        "polymer_composition": "heteromeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1110/ps.4440102"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKAKIIRYFYNAKDGLAQTFVYGGCX"
          }
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKARIIRYFYNAKDGKAQTFVYGGCX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1K43",
    "data": {
      "rcsb_id": "1K43",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2001-10-05T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1K43"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.012583999"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "RGKWTYNGITYEGR"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1KD8",
    "data": {
      "rcsb_id": "1KD8",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.7
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2001-11-12T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1KD8"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 6,
        "disulfide_bond_count": 3,
        "polymer_composition": "heteromeric protein",
        "resolution_combined": [
          1.9
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.261563398"
      },
      "refine": [
        {
          "B_iso_mean": 35.8
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVKQLEAEVEEIESEVWHLENEVARLEKENAECEA"
          }
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKVKQLKAKVEELKSKLWHLKNKVARLKKKNAECKA"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1KD9",
    "data": {
      "rcsb_id": "1KD9",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.7
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2001-11-12T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1KD9"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 6,
        "disulfide_bond_count": 3,
        "polymer_composition": "heteromeric protein",
        "resolution_combined": [
          2.1
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.261563398"
      },
      "refine": [
        {
          "B_iso_mean": 58.2
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVKQLEAEVEELESELWHLENEVARLEKENAECEA"
          }
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKVKQLKAKVEELKSKLWHLKNKVARLKKKNAECKA"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1KDD",
    "data": {
      "rcsb_id": "1KDD",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.7
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2001-11-12T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1KDD"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 6,
        "disulfide_bond_count": 2,
        "polymer_composition": "heteromeric protein",
        "resolution_combined": [
          2.14
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.261563398"
      },
      "refine": [
        {
          "B_iso_mean": 59.1
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKVKQLKAKVEELKSKLWHLKNKVARLKKKNAECKA"
          }
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVKQLEAEVEELESEIWHLENEVARLEKENAECEA"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1KYC",
    "data": {
      "rcsb_id": "1KYC",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-02-04T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1KYC"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 3,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.45
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1016/S0022-2836(02)00114-6"
      },
      "refine": [
        {
          "B_iso_mean": null
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "EELRRRIEELERRIRX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 3
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "SO4",
              "name": "SULFATE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "SIN",
              "name": "SUCCINIC ACID"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1L2Y",
    "data": {
      "rcsb_id": "1L2Y",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-02-25T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1L2Y"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1038/nsb798"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "NLYIQWLKDGGPSSGRPPPS"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1L4X",
    "data": {
      "rcsb_id": "1L4X",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 4.6
        },
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 4.6
        },
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 9
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-03-06T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1L4X"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 10,
        "deposited_polymer_entity_instance_count": 8,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1006/jsbi.2002.4467"
      },
      "refine": [
        {
          "B_iso_mean": 55
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "DELERAIRELAARIKX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 8
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "SIN",
              "name": "SUCCINIC ACID"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "CL",
              "name": "CHLORIDE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MG",
              "name": "MAGNESIUM ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1LE0",
    "data": {
      "rcsb_id": "1LE0",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-04-09T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1LE0"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.091100898"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SWTWEGNKWTWKX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1LE1",
    "data": {
      "rcsb_id": "1LE1",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-04-09T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1LE1"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.091100898"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SWTWENGKWTWKX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1LE3",
    "data": {
      "rcsb_id": "1LE3",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-04-09T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1LE3"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.091100898"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "PROTEIN BINDING"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "GEWTWDDATKTWTWTEX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1LQ7",
    "data": {
      "rcsb_id": "1LQ7",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-05-09T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1LQ7"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja0264201"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "GSRVKALEEKVKALEEKVKALGGGGRIEELKKKWEELKKKIEELGGGGEVKKVEEEVKKLEEEIKKL"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1LT1",
    "data": {
      "rcsb_id": "1LT1",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-05-20T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1LT1"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 12,
        "deposited_polymer_entity_instance_count": 8,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.91
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1002/anie.200390127"
      },
      "refine": [
        {
          "B_iso_mean": 21.179
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQGIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MN",
              "name": "MANGANESE (II) ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1M3W",
    "data": {
      "rcsb_id": "1M3W",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-07-01T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1M3W"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 2,
        "deposited_polymer_entity_instance_count": 4,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.8
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1016/S0022-2836(02)01441-9"
      },
      "refine": [
        {
          "B_iso_mean": 39.5
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "CGGGEIWKLHEEFLKKFEELLKLHEERLKKMX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 4
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "HG",
              "name": "MERCURY (II) ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1MJ0",
    "data": {
      "rcsb_id": "1MJ0",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 4.8
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-08-26T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1MJ0"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 3,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.031
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.0337680100"
      },
      "refine": [
        {
          "B_iso_mean": 17.984
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "MRGSHHHHHHGSDLGKKLLEAARAGQDDEVRILMANGADVNATDNDGYTPLHLAASNGHLEIVEVLLKNGADVNASDLTGITPLHLAAATGHLEIVEVLLKHGADVNAYDNDGHTPLHLAAKYGHLEIVEVLLKHGADVNAQDKFGKTAFDISIDNGNEDLAEILQ"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "SO4",
              "name": "SULFATE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "TRS",
              "name": "2-AMINO-2-HYDROXYMETHYL-PROPANE-1,3-DIOL"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1N09",
    "data": {
      "rcsb_id": "1N09",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-10-11T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1N09"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja028075l"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XCTWEGNKLTCX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1N0A",
    "data": {
      "rcsb_id": "1N0A",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-10-11T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1N0A"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1110/ps.0228603"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XCTWEPDGKLTCX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1N0C",
    "data": {
      "rcsb_id": "1N0C",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-10-11T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1N0C"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja028075l"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XCHWEGNKLVCX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1N0D",
    "data": {
      "rcsb_id": "1N0D",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2002-10-11T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1N0D"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 1,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja028075l"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XCVWEGNKLHCX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1N0Q",
    "data": {
      "rcsb_id": "1N0Q",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-10-14T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1N0Q"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 1,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.26
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.252537899"
      },
      "refine": [
        {
          "B_iso_mean": 12.618
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "STRUCTURAL PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "NGRTPLHLAARNGHLEVVKLLLEAGADVNAKDKNGRTPLHLAARNGHLEVVKLLLEAGADVNAKDKNGRTPLHLAARNGHLEVVKLLLEAGAY"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "TFA",
              "name": "trifluoroacetic acid"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1N0R",
    "data": {
      "rcsb_id": "1N0R",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 6.4
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-10-14T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1N0R"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 2,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.5
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.252537899"
      },
      "refine": [
        {
          "B_iso_mean": 9.619
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "STRUCTURAL PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "NGRTPLHLAARNGHLEVVKLLLEAGADVNAKDKNGRTPLHLAARNGHLEVVKLLLEAGADVNAKDKNGRTPLHLAARNGHLEVVKLLLEAGADVNAKDKNGRTPLHLAARNGHLEVVKLLLEAGAY"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "BR",
              "name": "BROMIDE ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1NA0",
    "data": {
      "rcsb_id": "1NA0",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 8
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-11-26T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1NA0"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 12,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.6
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1016/S0969-2126(03)00076-5"
      },
      "refine": [
        {
          "B_iso_mean": 12.652
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "GAMDPGNSAEAWYNLGNAYYKQGDYDEAIEYYQKALELDPNNAEAWYNLGNAYYKQGDYDEAIEYYQKALELDPNNAEAWYNLGNAYYKQGDYDEAIEYYQKALELDPNNAEAKQNLGNAKQKQG"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "PB",
              "name": "LEAD (II) ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "ACT",
              "name": "ACETATE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "CL",
              "name": "CHLORIDE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "NA",
              "name": "SODIUM ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MG",
              "name": "MAGNESIUM ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "IPT",
              "name": "1-methylethyl 1-thio-beta-D-galactopyranoside"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1NA3",
    "data": {
      "rcsb_id": "1NA3",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 8
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2002-11-26T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1NA3"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 5,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.55
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1016/S0969-2126(03)00076-5"
      },
      "refine": [
        {
          "B_iso_mean": 12.877
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "GAMDPGNSAEAWYNLGNAYYKQGDYDEAIEYYQKALELDPNNAEAWYNLGNAYYKQGDYDEAIEYYQKALELDPNNAEAKQNLGNAKQKQG"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "TRS",
              "name": "2-AMINO-2-HYDROXYMETHYL-PROPANE-1,3-DIOL"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "IPT",
              "name": "1-methylethyl 1-thio-beta-D-galactopyranoside"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MG",
              "name": "MAGNESIUM ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1NVO",
    "data": {
      "rcsb_id": "1NVO",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2003-02-04T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1NVO"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.0730771100"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "UNKNOWN FUNCTION"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQLIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1OVR",
    "data": {
      "rcsb_id": "1OVR",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2003-03-27T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1OVR"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 5,
        "deposited_polymer_entity_instance_count": 4,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.99
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja054199x"
      },
      "refine": [
        {
          "B_iso_mean": 32.402
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQLIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "MN",
              "name": "MANGANESE (II) ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1OVU",
    "data": {
      "rcsb_id": "1OVU",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2003-03-27T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1OVU"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 7,
        "deposited_polymer_entity_instance_count": 4,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          3.1
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja054199x"
      },
      "refine": [
        {
          "B_iso_mean": 58.981
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQAIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "CO",
              "name": "COBALT (II) ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1OVV",
    "data": {
      "rcsb_id": "1OVV",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2003-03-27T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1OVV"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 7,
        "deposited_polymer_entity_instance_count": 6,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.9
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja054199x"
      },
      "refine": [
        {
          "B_iso_mean": 61.7
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQAIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 6
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "CO",
              "name": "COBALT (II) ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1P68",
    "data": {
      "rcsb_id": "1P68",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2003-04-29T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1P68"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.1835644100"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "MYGKLNDLLEDLQEVLKNLHKNWHGGKDNLHDVDNHLQNVIEDIHDFMQGGGSGGKLQEMMKEFQQVLDELNNHLQGGKHTVHHIEQNIKEIFHHLEELVHR"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1PBZ",
    "data": {
      "rcsb_id": "1PBZ",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "2003-05-15T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1PBZ"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 1,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 2,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.2231273100"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XCGAEAAKAHAKAAEAGCX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "PC3",
              "name": "COPROPORPHYRIN I CONTAINING CO(III)"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1PSV",
    "data": {
      "rcsb_id": "1PSV",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "1997-10-29T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1PSV"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1006/jmbi.1997.1341"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DESIGNED PEPTIDE"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "KPYTARIKGRTFSNEKELRDFLETFTGR"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1PYZ",
    "data": {
      "rcsb_id": "1PYZ",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "EVAPORATION",
          "pH": null
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2003-07-09T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1PYZ"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 2,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.25
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1007/s00775-004-0600-x"
      },
      "refine": [
        {
          "B_iso_mean": null
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "METAL BINDING PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XESQLHSNKRX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 4
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "CL",
              "name": "CHLORIDE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "DEU",
              "name": "CO(III)-(DEUTEROPORPHYRIN IX)"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1QP6",
    "data": {
      "rcsb_id": "1QP6",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal_grow": null,
      "rcsb_accession_info": {
        "deposit_date": "1999-06-01T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1QP6"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 2,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": null
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1021/ja9733649"
      },
      "refine": null,
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "GEVEELEKKFKELWKGPRRGEIEELHKKFHELIKG"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 2
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1QYS",
    "data": {
      "rcsb_id": "1QYS",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP, STREAK SEEDING",
          "pH": 6.6
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2003-09-11T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1QYS"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.5
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1126/science.1089427"
      },
      "refine": [
        {
          "B_iso_mean": 65.5
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "MGDIQVQVNIDDNGKNFDYTYTVTTESELQKVLNELMDYIKKQGAKRVRISITARTKKEAEKFAAILIKVFAELGYNDINVTFDGDTVTVEGQLEGGSLEHHHHHH"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 1
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1RH4",
    "data": {
      "rcsb_id": "1RH4",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": null,
          "pH": 8.1
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "1998-10-07T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1RH4"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 3,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.9
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1126/science.282.5393.1462"
      },
      "refine": [
        {
          "B_iso_mean": null
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "COILED COIL"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XAALAQIKKEIAYLLAKIKAEILAALKKIKQEIAX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 4
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "IPA",
              "name": "ISOPROPYL ALCOHOL"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1S9Z",
    "data": {
      "rcsb_id": "1S9Z",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 6.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2004-02-06T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1S9Z"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 2,
        "deposited_polymer_entity_instance_count": 1,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.01
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.0306786101"
      },
      "refine": [
        {
          "B_iso_mean": 49.2
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XSIRELEARIRELELRIG"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 3
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "NA",
              "name": "SODIUM ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "ZN",
              "name": "ZINC ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1SN9",
    "data": {
      "rcsb_id": "1SN9",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 8.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2004-03-10T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1SN9"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 0,
        "deposited_polymer_entity_instance_count": 4,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.2
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.0401245101"
      },
      "refine": [
        {
          "B_iso_mean": 22.3
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XYRIPSYDFADELAKLLRQAAGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 4
          }
        }
      ],
      "nonpolymer_entities": null
    },
    "type": "entry"
  },
  {
    "identifier": "1SNA",
    "data": {
      "rcsb_id": "1SNA",
      "em_3d_reconstruction": null,
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5
        }
      ],
      "rcsb_accession_info": {
        "deposit_date": "2004-03-10T00:00:00Z"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1SNA"
      },
      "rcsb_entry_info": {
        "deposited_nonpolymer_entity_instance_count": 1,
        "deposited_polymer_entity_instance_count": 4,
        "disulfide_bond_count": 0,
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.5
        ]
      },
      "rcsb_primary_citation": {
        "pdbx_database_id_DOI": "10.1073/pnas.0401245101"
      },
      "refine": [
        {
          "B_iso_mean": 15.2
        }
      ],
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XYRIPSYDFADELMKLLRQAAGX"
          }
        }
      ],
      "assemblies": [
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 4
          }
        },
        {
          "rcsb_assembly_info": {
            "polymer_entity_instance_count": 4
          }
        }
      ],
      "nonpolymer_entities": [
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "id": "IPA",
              "name": "ISOPROPYL ALCOHOL"
            }
          }
        }
      ]
    },
    "type": "entry"
  }
]
"""
