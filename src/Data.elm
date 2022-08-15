module Data exposing (Design, getAllDesigns)

import Date
import Json.Decode as JDe
import Json.Decode.Pipeline exposing (hardcoded, required, requiredAt, optionalAt)
import Time exposing (Month(..))


-- Things to add:
-- * Crystallisation conditions
-- * Temperature
-- * Publicatoin doi, link, authors and abstract
-- * All the pictures
--   * Have a toggle for asym unit and biol unit
--   * URLs are preferred for the pictures so we don't need to host
-- * NGL view?
-- * The design method
type alias Design =
    { pdbCode : String
    , depositionDate : Date.Date
    , method : String
    , picturePath : String
    , structuralKeywords : String
    , doi : String
    , publicationTitle : String
    , authors : List String
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
        |> Debug.log "eep"
        |> Result.withDefault []


designDecoder : JDe.Decoder Design
designDecoder =
    JDe.succeed makeDesign
        |> required "rcsb_id" JDe.string
        |> requiredAt [ "rcsb_primary_citation", "year" ] JDe.int
        |> required "exptl" (JDe.index 0 (JDe.field "method" JDe.string))
        |> requiredAt [ "struct_keywords", "pdbx_keywords" ] JDe.string
        |> requiredAt [ "pubmed", "rcsb_pubmed_doi" ] JDe.string
        |> requiredAt [ "rcsb_primary_citation", "title" ] JDe.string
        |> requiredAt [ "rcsb_primary_citation", "rcsb_authors" ] (JDe.list JDe.string)
        |> required "polymer_entities"
            (JDe.list
                (JDe.at [ "entity_poly", "pdbx_seq_one_letter_code_can" ] JDe.string)
            )


makeDesign : String -> Int -> String -> String -> String -> String -> List String -> List String -> Design
makeDesign inPdbCode depositionDate method structuralKeywords doi publicationTitle authors sequences =
    let
        pdbCode = String.toLower inPdbCode
    in
    { pdbCode = pdbCode
    , depositionDate =
        String.fromInt depositionDate
            ++ "-01-01"
            |> Date.fromIsoString
            |> Result.withDefault (Date.fromCalendarDate 1900 Jan 1)
    , method = method
    -- Example: https://cdn.rcsb.org/images/structures/pn/4pnb/4pnb_assembly-1.jpeg
    , picturePath = "https://cdn.rcsb.org/images/structures/"
        ++ String.slice 1 3 pdbCode
        ++ "/"
        ++ pdbCode
        ++ "/"
        ++ pdbCode
        ++ "_assembly-1.jpeg"
    , structuralKeywords = structuralKeywords
    , doi = doi
    , publicationTitle = publicationTitle
    , authors = authors
    , sequences = sequences
    }


rawData : String
rawData =
    """
[
  {
    "identifier": "1G6U",
    "data": {
      "rcsb_id": "1G6U",
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 42.32
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION",
          "pH": 6.5,
          "pdbx_details": "100 mM MES, pH 6.5, 100 mM NaCl, 2.6 M ammonium sulfate, 1% dioxane, VAPOR DIFFUSION, temperature 298K",
          "temp": 298
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11171963
        },
        "rcsb_pubmed_doi": "10.1073/pnas.98.4.1404"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1G6U"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.48
        ],
        "diffrn_resolution_high": {
          "value": 1.48
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "98",
        "page_first": "1404",
        "page_last": "1409",
        "pdbx_database_id_DOI": "10.1073/pnas.98.4.1404",
        "rcsb_authors": [
          "Ogihara, N.L.",
          "Ghirlanda, G.",
          "Bryson, J.W.",
          "Gingery, M.",
          "DeGrado, W.F.",
          "Eisenberg, D."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Design of three-dimensional domain-swapped dimers and fibrous oligomers.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SLAALKSELQALKKEGFSPEELAALESELQALEKKLAALKSKLQALKG",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
              "name": "SULFATE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "name": "trifluoroacetic acid"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1L4X",
    "data": {
      "rcsb_id": "1L4X",
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 66.67
        },
        {
          "density_percent_sol": null
        },
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 4.6,
          "pdbx_details": "magnesium sulphate; sodium acetate or bicine, pH 4.6, VAPOR DIFFUSION, HANGING DROP, temperature 100K",
          "temp": null
        },
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 4.6,
          "pdbx_details": "magnesium sulphate; sodium acetate or bicine, pH 4.6, VAPOR DIFFUSION, HANGING DROP, temperature 100K",
          "temp": null
        },
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 9,
          "pdbx_details": "magnesium sulphate; sodium acetate or bicine, pH 9.0, VAPOR DIFFUSION, HANGING DROP, temperature 100K",
          "temp": null
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 12064934
        },
        "rcsb_pubmed_doi": "10.1006/jsbi.2002.4467"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1L4X"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2
        ],
        "diffrn_resolution_high": {
          "value": 2
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "137",
        "page_first": "65",
        "page_last": "72",
        "pdbx_database_id_DOI": "10.1006/jsbi.2002.4467",
        "rcsb_authors": [
          "Meier, M.",
          "Lustig, A.",
          "Aebi, U.",
          "Burkhard, P."
        ],
        "rcsb_journal_abbrev": "J Struct Biol",
        "title": "Removing an interhelical salt bridge abolishes coiled-coil formation in a de novo designed peptide",
        "year": 2002
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "DELERAIRELAARIKX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
              "name": "SUCCINIC ACID"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "name": "CHLORIDE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "name": "MAGNESIUM ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1EC5",
    "data": {
      "rcsb_id": "1EC5",
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 32
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 4.6,
          "pdbx_details": "CRYSTALS WERE GROWN FROM AN AMMONIUM SULFATE 2M SOLUTION, pH 4.6, VAPOR DIFFUSION, HANGING DROP",
          "temp": 277
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 10841536
        },
        "rcsb_pubmed_doi": "10.1073/pnas.97.12.6298"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1EC5"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.5
        ],
        "diffrn_resolution_high": {
          "value": 2.5
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "97",
        "page_first": "6298",
        "page_last": "6305",
        "pdbx_database_id_DOI": "10.1073/pnas.97.12.6298",
        "rcsb_authors": [
          "Lombardi, A.",
          "Summa, C.M.",
          "Geremia, S.",
          "Randaccio, L.",
          "Pavone, V.",
          "DeGrado, W.F."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Inaugural article: retrostructural analysis of metalloproteins: application to the design of a minimal model for diiron proteins.",
        "year": 2000
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQLIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
              "name": "ZINC ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1HQJ",
    "data": {
      "rcsb_id": "1HQJ",
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 29.28
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 5.6,
          "pdbx_details": "Ammonium Sulfate, Sodium Acetate, pH 5.6, VAPOR DIFFUSION, HANGING DROP, temperature 298K",
          "temp": 298
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11206050
        },
        "rcsb_pubmed_doi": "10.1110/ps.9.12.2294"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1HQJ"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.2
        ],
        "diffrn_resolution_high": {
          "value": 1.2
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "9",
        "page_first": "2294",
        "page_last": "2301",
        "pdbx_database_id_DOI": null,
        "rcsb_authors": [
          "Burkhard, P.",
          "Meier, M.",
          "Lustig, A."
        ],
        "rcsb_journal_abbrev": "Protein Sci",
        "title": "Design of a minimal protein oligomerization domain by a structural approach.",
        "year": 2000
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "DELERRIRELEARIK",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
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
              "name": "LEAD (II) ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "name": "SUCCINIC ACID"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "name": "SULFATE ION"
            }
          }
        }
      ]
    },
    "type": "entry"
  },
  {
    "identifier": "1JM0",
    "data": {
      "rcsb_id": "1JM0",
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 42
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5,
          "pdbx_details": "PEG 400 , Mn(CH3COO)2 , DMSO, Tris-HCl, pH 7.50, VAPOR DIFFUSION, HANGING DROP, temperature 277K",
          "temp": 277
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11749531
        },
        "rcsb_pubmed_doi": "10.1021/ja010506x"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JM0"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.7
        ],
        "diffrn_resolution_high": {
          "value": 1.7
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "123",
        "page_first": "12749",
        "page_last": "12757",
        "pdbx_database_id_DOI": "10.1021/ja010506x",
        "rcsb_authors": [
          "Di Costanzo, L.",
          "Wade, H.",
          "Geremia, S.",
          "Randaccio, L.",
          "Pavone, V.",
          "DeGrado, W.F.",
          "Lombardi, A."
        ],
        "rcsb_journal_abbrev": "J Am Chem Soc",
        "title": "Toward the de novo design of a catalytically active helix bundle: a substrate-accessible carboxylate-bridged dinuclear metal center.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQAIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
              "name": "MANGANESE (II) ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
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
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 48.39
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5,
          "pdbx_details": "PEG 200, DMSO, Mn(CH3COO)2 , TRIS, pH 7.50, VAPOR DIFFUSION, HANGING DROP, temperature 279K",
          "temp": 279
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11749531
        },
        "rcsb_pubmed_doi": "10.1021/ja010506x"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JMB"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.2
        ],
        "diffrn_resolution_high": {
          "value": 2.2
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "123",
        "page_first": "12749",
        "page_last": "12757",
        "pdbx_database_id_DOI": "10.1021/ja010506x",
        "rcsb_authors": [
          "Di Costanzo, L.",
          "Wade, H.",
          "Geremia, S.",
          "Randaccio, L.",
          "Pavone, V.",
          "DeGrado, W.F.",
          "Lombardi, A."
        ],
        "rcsb_journal_abbrev": "J Am Chem Soc",
        "title": "Toward the de novo design of a catalytically active helix bundle: a substrate-accessible carboxylate-bridged dinuclear metal center.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XDYLRELLKLELQAIKQYREALEYVKLPVLAKILEDEEKHIEWLETILGX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
              "name": "MANGANESE (II) ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11982362
        },
        "rcsb_pubmed_doi": "10.1021/ja0174276"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JY4"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "124",
        "page_first": "4987",
        "page_last": "4994",
        "pdbx_database_id_DOI": "10.1021/ja0174276",
        "rcsb_authors": [
          "Venkatraman, J.",
          "Nagana Gowda, G.A.",
          "Balaram, P."
        ],
        "rcsb_journal_abbrev": "J Am Chem Soc",
        "title": "Design and construction of an open multistranded beta-sheet polypeptide stabilized by a disulfide bridge.",
        "year": 2002
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "RGECKFTVPGRTALNTPAVQKWHFVLPGYKCEILA",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11982362
        },
        "rcsb_pubmed_doi": "10.1021/ja0174276"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JY6"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "124",
        "page_first": "4987",
        "page_last": "4994",
        "pdbx_database_id_DOI": "10.1021/ja0174276",
        "rcsb_authors": [
          "Venkatraman, J.",
          "Nagana Gowda, G.A.",
          "Balaram, P."
        ],
        "rcsb_journal_abbrev": "J Am Chem Soc",
        "title": "Design and construction of an open multistranded beta-sheet polypeptide stabilized by a disulfide bridge.",
        "year": 2002
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "RGECKFTVPGRTALNTPAVQKWHFVLPGYKCEILA",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
    "identifier": "1K09",
    "data": {
      "rcsb_id": "1K09",
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 12021452
        },
        "rcsb_pubmed_doi": "10.1110/ps.4440102"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1K09"
      },
      "rcsb_entry_info": {
        "polymer_composition": "heteromeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "11",
        "page_first": "1539",
        "page_last": "1551",
        "pdbx_database_id_DOI": "10.1110/ps.4440102",
        "rcsb_authors": [
          "Carulla, N.",
          "Woodward, C.",
          "Barany, G."
        ],
        "rcsb_journal_abbrev": "Protein Sci",
        "title": "BetaCore, a designed water soluble four-stranded antiparallel beta-sheet protein.",
        "year": 2002
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKAKIIRYFYNAKDGLAQTFVYGGCX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKARIIRYFYNAKDGKAQTFVYGGCX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
    "identifier": "1KYC",
    "data": {
      "rcsb_id": "1KYC",
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 28.17
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.5,
          "pdbx_details": "ammonium sulfate, HEPES, pH 7.5, VAPOR DIFFUSION, HANGING DROP, temperature 298K",
          "temp": 298
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 12054832
        },
        "rcsb_pubmed_doi": "10.1016/S0022-2836(02)00114-6"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1KYC"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          1.45
        ],
        "diffrn_resolution_high": {
          "value": 1.45
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "318",
        "page_first": "901",
        "page_last": "910",
        "pdbx_database_id_DOI": "10.1016/S0022-2836(02)00114-6",
        "rcsb_authors": [
          "Burkhard, P.",
          "Ivaninskii, S.",
          "Lustig, A."
        ],
        "rcsb_journal_abbrev": "J Mol Biol",
        "title": "Improving coiled-coil stability by optimizing ionic interactions.",
        "year": 2002
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "EELRRRIEELERRIRX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
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
              "name": "SULFATE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "name": "SUCCINIC ACID"
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
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 45
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": null,
          "pH": 7.5,
          "pdbx_details": "pH 7.5",
          "temp": null
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 10210186
        },
        "rcsb_pubmed_doi": "10.1110/ps.8.1.84"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1BB1"
      },
      "rcsb_entry_info": {
        "polymer_composition": "heteromeric protein",
        "resolution_combined": [
          1.8
        ],
        "diffrn_resolution_high": {
          "value": 1.8
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "8",
        "page_first": "84",
        "page_last": "90",
        "pdbx_database_id_DOI": null,
        "rcsb_authors": [
          "Nautiyal, S.",
          "Alber, T."
        ],
        "rcsb_journal_abbrev": "Protein Sci",
        "title": "Crystal structure of a designed, thermostable, heterotrimeric coiled coil.",
        "year": 1999
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN DESIGN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XAEIAAIEYEQAAIKEEIAAIKDKIAAIKEYIAAIX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": [
            {
              "ncbi_scientific_name": null
            }
          ],
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEKIAAIKEEQAAIEEEIQAIKEEIAAIKYLIAQIX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": [
            {
              "ncbi_scientific_name": null
            }
          ],
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XAEIAAIKYKQAAIKNEIAAIKQEIAAIEQMIAAIX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": [
            {
              "ncbi_scientific_name": null
            }
          ],
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
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
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 27.13
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, SITTING DROP",
          "pH": 8,
          "pdbx_details": "91% 2-METHYL-2,4-PENTANEDIOL, 78mM TRIETHANOLAMINE-HCL pH8, 52mM ETHANOLAMINE-HCL pH9.75, pH 8.0, VAPOR DIFFUSION, SITTING DROP",
          "temp": null
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 10422828
        },
        "rcsb_pubmed_doi": "10.1110/ps.8.7.1400"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1BYZ"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          0.9
        ],
        "diffrn_resolution_high": {
          "value": 0.9
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "8",
        "page_first": "1400",
        "page_last": "1409",
        "pdbx_database_id_DOI": null,
        "rcsb_authors": [
          "Prive, G.G.",
          "Anderson, D.H.",
          "Wesson, L.",
          "Cascio, D.",
          "Eisenberg, D."
        ],
        "rcsb_journal_abbrev": "Protein Sci",
        "title": "Packed protein bilayers in the 0.90 A resolution structure of a designed alpha helical bundle.",
        "year": 1999
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XELLKKLLEELKG",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
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
              "name": "ETHANOLAMINE"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "name": "(4S)-2-METHYL-2,4-PENTANEDIOL"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
              "name": "CHLORIDE ION"
            }
          }
        },
        {
          "nonpolymer_comp": {
            "chem_comp": {
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
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 40.98
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 8446897
        },
        "rcsb_pubmed_doi": "10.1126/science.8446897"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1COS"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": [
          2.1
        ],
        "diffrn_resolution_high": {
          "value": 2.1
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "259",
        "page_first": "1288",
        "page_last": "1293",
        "pdbx_database_id_DOI": null,
        "rcsb_authors": [
          "Lovejoy, B.",
          "Choe, S.",
          "Cascio, D.",
          "McRorie, D.K.",
          "DeGrado, W.F.",
          "Eisenberg, D."
        ],
        "rcsb_journal_abbrev": "Science",
        "title": "Crystal structure of a synthetic triple-stranded alpha-helical bundle.",
        "year": 1993
      },
      "struct_keywords": {
        "pdbx_keywords": "ALPHA-HELICAL BUNDLE"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEWEALEKKLAALESKLQALEKKLEALEHGX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": null
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": null,
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 10913242
        },
        "rcsb_pubmed_doi": "10.1021/bi000208x"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1DJF"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "39",
        "page_first": "8362",
        "page_last": "8373",
        "pdbx_database_id_DOI": "10.1021/bi000208x",
        "rcsb_authors": [
          "Montserret, R.",
          "McLeish, M.J.",
          "Bockmann, A.",
          "Geourjon, C.",
          "Penin, F."
        ],
        "rcsb_journal_abbrev": "Biochemistry",
        "title": "Involvement of electrostatic interactions in the mechanism of peptide folding induced by sodium dodecyl sulfate binding.",
        "year": 2000
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "QAPAYKKAAKKLAES",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
    "identifier": "1FME",
    "data": {
      "rcsb_id": "1FME",
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": null,
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11292351
        },
        "rcsb_pubmed_doi": "10.1006/jmbi.2000.4345"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1FME"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "307",
        "page_first": "1411",
        "page_last": "1418",
        "pdbx_database_id_DOI": "10.1006/jmbi.2000.4345",
        "rcsb_authors": [
          "Sarisky, C.A.",
          "Mayo, S.L."
        ],
        "rcsb_journal_abbrev": "J Mol Biol",
        "title": "The beta-beta-alpha fold: explorations in sequence space.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "EQYTAKYKGRTFRNEKELRDFIEKFKGR",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": null,
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11041845
        },
        "rcsb_pubmed_doi": "10.1021/bi001242e"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1FMH"
      },
      "rcsb_entry_info": {
        "polymer_composition": "heteromeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "39",
        "page_first": "12804",
        "page_last": "12818",
        "pdbx_database_id_DOI": "10.1021/bi001242e",
        "rcsb_authors": [
          "Marti, D.N.",
          "Jelesarov, I.",
          "Bosshard, H.R."
        ],
        "rcsb_journal_abbrev": "Biochemistry",
        "title": "Interhelical ion pairing in coiled coils: solution structure of a heterodimeric leucine zipper and determination of pKa values of Glu side chains.",
        "year": 2000
      },
      "struct_keywords": {
        "pdbx_keywords": "TRANSCRIPTION"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVAQLEKEVAQAEAENYQLEQEVAQLEHECGX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVQALKKRVQALKARNYAAKQKVQALRHKCGX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": null,
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 9311930
        },
        "rcsb_pubmed_doi": "10.1126/science.278.5335.82"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1FSD"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "278",
        "page_first": "82",
        "page_last": "87",
        "pdbx_database_id_DOI": "10.1126/science.278.5335.82",
        "rcsb_authors": [
          "Dahiyat, B.I.",
          "Mayo, S.L."
        ],
        "rcsb_journal_abbrev": "Science",
        "title": "De novo protein design: fully automated sequence selection.",
        "year": 1997
      },
      "struct_keywords": {
        "pdbx_keywords": "NOVEL SEQUENCE"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "QQYTAKIKGRTFRNEKELRDFIEKFKGR",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": [
            {
              "ncbi_scientific_name": null
            }
          ],
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": null,
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 9311930
        },
        "rcsb_pubmed_doi": "10.1126/science.278.5335.82"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1FSV"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "278",
        "page_first": "82",
        "page_last": "87",
        "pdbx_database_id_DOI": "10.1126/science.278.5335.82",
        "rcsb_authors": [
          "Dahiyat, B.I.",
          "Mayo, S.L."
        ],
        "rcsb_journal_abbrev": "Science",
        "title": "De novo protein design: fully automated sequence selection.",
        "year": 1997
      },
      "struct_keywords": {
        "pdbx_keywords": "BETA BETA ALPHA MOTIF"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "QQYTAKIKGRTFRNEKELRDFIEKFKGR",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": [
            {
              "ncbi_scientific_name": null
            }
          ],
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
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
    "identifier": "1IC9",
    "data": {
      "rcsb_id": "1IC9",
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": null,
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11373623
        },
        "rcsb_pubmed_doi": "10.1038/88604"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1IC9"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "8",
        "page_first": "535",
        "page_last": "539",
        "pdbx_database_id_DOI": "10.1038/88604",
        "rcsb_authors": [
          "Ottesen, J.J.",
          "Imperiali, B."
        ],
        "rcsb_journal_abbrev": "Nat Struct Biol",
        "title": "Design of a discretely folded mini-protein motif with predominantly beta-structure.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SKYEYTIPSYTFRGPGCPTLKPAITVRCE",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": "synthetic construct"
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": null,
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11373623
        },
        "rcsb_pubmed_doi": "10.1038/88604"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1ICL"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "8",
        "page_first": "535",
        "page_last": "539",
        "pdbx_database_id_DOI": "10.1038/88604",
        "rcsb_authors": [
          "Ottesen, J.J.",
          "Imperiali, B."
        ],
        "rcsb_journal_abbrev": "Nat Struct Biol",
        "title": "Design of a discretely folded mini-protein motif with predominantly beta-structure.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SKYEYTVPSYTFRGPGCPTVKPAISLRCE",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": null,
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11373623
        },
        "rcsb_pubmed_doi": "10.1038/88604"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1ICO"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "8",
        "page_first": "535",
        "page_last": "539",
        "pdbx_database_id_DOI": "10.1038/88604",
        "rcsb_authors": [
          "Ottesen, J.J.",
          "Imperiali, B."
        ],
        "rcsb_journal_abbrev": "Nat Struct Biol",
        "title": "Design of a discretely folded mini-protein motif with predominantly beta-structure.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SKYEYTIPSYTFRGPGCPTVKPAVTIRCE",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11782528
        },
        "rcsb_pubmed_doi": "10.1073/pnas.012583999"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1J4M"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "99",
        "page_first": "614",
        "page_last": "619",
        "pdbx_database_id_DOI": "10.1073/pnas.012583999",
        "rcsb_authors": [
          "Pastor, M.T.",
          "Lopez de la Paz, M.",
          "Lacroix, E.",
          "Serrano, L.",
          "Perez-Paya, E."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Combinatorial approaches: a new tool to search for highly structured beta-hairpin peptides.",
        "year": 2002
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "RGKWTYNGITYEGR",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
    "identifier": "1JY9",
    "data": {
      "rcsb_id": "1JY9",
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11593011
        },
        "rcsb_pubmed_doi": "10.1073/pnas.211536998"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1JY9"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "98",
        "page_first": "12015",
        "page_last": "12020",
        "pdbx_database_id_DOI": "10.1073/pnas.211536998",
        "rcsb_authors": [
          "Stanger, H.E.",
          "Syud, F.A.",
          "Espinosa, J.F.",
          "Giriat, I.",
          "Muir, T.",
          "Gellman, S.H."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Length-dependent stability and strand length limits in antiparallel beta -sheet secondary structure.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "TTTTRYVEVPGKKILQTTTT",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
    "identifier": "1K43",
    "data": {
      "rcsb_id": "1K43",
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11782528
        },
        "rcsb_pubmed_doi": "10.1073/pnas.012583999"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1K43"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "99",
        "page_first": "614",
        "page_last": "619",
        "pdbx_database_id_DOI": "10.1073/pnas.012583999",
        "rcsb_authors": [
          "Pastor, M.T.",
          "Lopez de la Paz, M.",
          "Lacroix, E.",
          "Serrano, L.",
          "Perez-Paya, E."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Combinatorial approaches: a new tool to search for highly structured beta-hairpin peptides.",
        "year": 2002
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "RGKWTYNGITYEGR",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
    "identifier": "1L2Y",
    "data": {
      "rcsb_id": "1L2Y",
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11979279
        },
        "rcsb_pubmed_doi": "10.1038/nsb798"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1L2Y"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "9",
        "page_first": "425",
        "page_last": "430",
        "pdbx_database_id_DOI": "10.1038/nsb798",
        "rcsb_authors": [
          "Neidigh, J.W.",
          "Fesinmeyer, R.M.",
          "Andersen, N.H."
        ],
        "rcsb_journal_abbrev": "Nat Struct Biol",
        "title": "Designing a 20-residue protein.",
        "year": 2002
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "NLYIQWLKDGGPSSGRPPPS",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
    "identifier": "1LE0",
    "data": {
      "rcsb_id": "1LE0",
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11331745
        },
        "rcsb_pubmed_doi": "10.1073/pnas.091100898"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1LE0"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "98",
        "page_first": "5578",
        "page_last": "5583",
        "pdbx_database_id_DOI": "10.1073/pnas.091100898",
        "rcsb_authors": [
          "Cochran, A.G.",
          "Skelton, N.J.",
          "Starovasnik, M.A."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Tryptophan zippers: stable, monomeric beta -hairpins.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SWTWEGNKWTWKX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11331745
        },
        "rcsb_pubmed_doi": "10.1073/pnas.091100898"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1LE1"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "98",
        "page_first": "5578",
        "page_last": "5583",
        "pdbx_database_id_DOI": "10.1073/pnas.091100898",
        "rcsb_authors": [
          "Cochran, A.G.",
          "Skelton, N.J.",
          "Starovasnik, M.A."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Tryptophan zippers: stable, monomeric beta -hairpins.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "SWTWENGKWTWKX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "SOLUTION NMR"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": null
        }
      ],
      "exptl_crystal_grow": null,
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11331745
        },
        "rcsb_pubmed_doi": "10.1073/pnas.091100898"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1LE3"
      },
      "rcsb_entry_info": {
        "polymer_composition": "homomeric protein",
        "resolution_combined": null,
        "diffrn_resolution_high": null
      },
      "rcsb_primary_citation": {
        "journal_volume": "98",
        "page_first": "5578",
        "page_last": "5583",
        "pdbx_database_id_DOI": "10.1073/pnas.091100898",
        "rcsb_authors": [
          "Cochran, A.G.",
          "Skelton, N.J.",
          "Starovasnik, M.A."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Tryptophan zippers: stable, monomeric beta -hairpins.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "PROTEIN BINDING"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "GEWTWDDATKTWTWTEX",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 56.1
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.7,
          "pdbx_details": "PEG 4000, Na Hepes, 2-propanol, pH 7.7, VAPOR DIFFUSION, HANGING DROP, temperature 293K",
          "temp": 293
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11752430
        },
        "rcsb_pubmed_doi": "10.1073/pnas.261563398"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1KD8"
      },
      "rcsb_entry_info": {
        "polymer_composition": "heteromeric protein",
        "resolution_combined": [
          1.9
        ],
        "diffrn_resolution_high": {
          "value": 1.85
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "98",
        "page_first": "14825",
        "page_last": "14830",
        "pdbx_database_id_DOI": "10.1073/pnas.261563398",
        "rcsb_authors": [
          "Keating, A.E.",
          "Malashkevich, V.N.",
          "Tidor, B.",
          "Kim, P.S."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Side-chain repacking calculations for predicting structures and stabilities of heterodimeric coiled coils.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVKQLEAEVEEIESEVWHLENEVARLEKENAECEA",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKVKQLKAKVEELKSKLWHLKNKVARLKKKNAECKA",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 57.2
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.7,
          "pdbx_details": "PEG 4000, Na Hepes, 2-propanol, pH 7.7, VAPOR DIFFUSION, HANGING DROP, temperature 293K",
          "temp": 293
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11752430
        },
        "rcsb_pubmed_doi": "10.1073/pnas.261563398"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1KD9"
      },
      "rcsb_entry_info": {
        "polymer_composition": "heteromeric protein",
        "resolution_combined": [
          2.1
        ],
        "diffrn_resolution_high": {
          "value": 2
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "98",
        "page_first": "14825",
        "page_last": "14830",
        "pdbx_database_id_DOI": "10.1073/pnas.261563398",
        "rcsb_authors": [
          "Keating, A.E.",
          "Malashkevich, V.N.",
          "Tidor, B.",
          "Kim, P.S."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Side-chain repacking calculations for predicting structures and stabilities of heterodimeric coiled coils.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVKQLEAEVEELESELWHLENEVARLEKENAECEA",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKVKQLKAKVEELKSKLWHLKNKVARLKKKNAECKA",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
      "exptl": [
        {
          "method": "X-RAY DIFFRACTION"
        }
      ],
      "exptl_crystal": [
        {
          "density_percent_sol": 56.5
        }
      ],
      "exptl_crystal_grow": [
        {
          "method": "VAPOR DIFFUSION, HANGING DROP",
          "pH": 7.7,
          "pdbx_details": "PEG 4000, Na Hepes, 2-propanol, pH 7.7, VAPOR DIFFUSION, HANGING DROP, temperature 293K",
          "temp": 293
        }
      ],
      "pubmed": {
        "rcsb_pubmed_container_identifiers": {
          "pubmed_id": 11752430
        },
        "rcsb_pubmed_doi": "10.1073/pnas.261563398"
      },
      "rcsb_entry_container_identifiers": {
        "entry_id": "1KDD"
      },
      "rcsb_entry_info": {
        "polymer_composition": "heteromeric protein",
        "resolution_combined": [
          2.14
        ],
        "diffrn_resolution_high": {
          "value": 2.05
        }
      },
      "rcsb_primary_citation": {
        "journal_volume": "98",
        "page_first": "14825",
        "page_last": "14830",
        "pdbx_database_id_DOI": "10.1073/pnas.261563398",
        "rcsb_authors": [
          "Keating, A.E.",
          "Malashkevich, V.N.",
          "Tidor, B.",
          "Kim, P.S."
        ],
        "rcsb_journal_abbrev": "Proc Natl Acad Sci U S A",
        "title": "Side-chain repacking calculations for predicting structures and stabilities of heterodimeric coiled coils.",
        "year": 2001
      },
      "struct_keywords": {
        "pdbx_keywords": "DE NOVO PROTEIN"
      },
      "polymer_entities": [
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XKVKQLKAKVEELKSKLWHLKNKVARLKKKNAECKA",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
        },
        {
          "entity_poly": {
            "pdbx_seq_one_letter_code_can": "XEVKQLEAEVEELESEIWHLENEVARLEKENAECEA",
            "rcsb_entity_polymer_type": "Protein"
          },
          "rcsb_entity_host_organism": null,
          "rcsb_entity_source_organism": [
            {
              "ncbi_scientific_name": null
            }
          ]
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
  }
]
"""
