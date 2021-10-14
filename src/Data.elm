module Data exposing (Design, designs, expMethodToString)

import Date
import Time exposing (Month(..))


type alias Design =
    { pdbCode : String
    , depositionDate : Date.Date
    , method : ExpMethod
    , picturePath : String
    , structuralKeywords : String
    , publicationLink : String
    , sequence : String
    }


type ExpMethod
    = Xray Float
    | NMR
    | CryoEM


expMethodToString : ExpMethod -> String
expMethodToString method =
    case method of
        Xray resolution ->
            "Xray Crystallography ("
                ++ String.fromFloat resolution
                ++ " Å)"

        NMR ->
            "NMR"

        CryoEM ->
            "Cryo EM"


designs : List Design
designs =
    [ { pdbCode = "1cos"
      , depositionDate = Date.fromCalendarDate 1993 Jan 22
      , method = Xray 2.1
      , picturePath = "%PUBLIC_URL%/designs/1cos.jpg"
      , structuralKeywords = "ALPHA-HELICAL BUNDLE"
      , publicationLink = "10.1126/science.8446897"
      , sequence = "XEWEALEKKLAALESKLQALEKKLEALEHGX"
      }
    , { pdbCode = "1fsd"
      , depositionDate = Date.fromCalendarDate 1997 Jun 9
      , method = NMR
      , picturePath = "%PUBLIC_URL%/designs/1fsd.jpg"
      , structuralKeywords = "NOVEL SEQUENCE"
      , publicationLink = "10.1126/science.278.5335.82"
      , sequence = "QQYTAKIKGRTFRNEKELRDFIEKFKGR"
      }
    , { pdbCode = "1jy4"
      , depositionDate = Date.fromCalendarDate 2001 Sep 11
      , method = NMR
      , picturePath = "%PUBLIC_URL%/designs/1jy4.jpg"
      , structuralKeywords = "DE NOVO PROTEIN"
      , publicationLink = "10.1021/ja0174276"
      , sequence = "RGECKFTVPGRTALNTPAVQKWHFVLPGYKCEILA"
      }
    , { pdbCode = "1byz"
      , depositionDate = Date.fromCalendarDate 1998 Oct 28
      , method = Xray 0.9
      , picturePath = "%PUBLIC_URL%/designs/1byz.jpg"
      , structuralKeywords = "DE NOVO PROTEIN"
      , publicationLink = "10.1110/ps.8.7.1400"
      , sequence = "XELLKKLLEELKG"
      }
    , { pdbCode = "1bb1"
      , depositionDate = Date.fromCalendarDate 1999 Feb 2
      , method = Xray 1.8
      , picturePath = "%PUBLIC_URL%/designs/1bb1.jpg"
      , structuralKeywords = "DE NOVO PROTEIN DESIGN"
      , publicationLink = "10.1110/ps.8.1.84"
      , sequence = "XAEIAAIEYEQAAIKEEIAAIKDKIAAIKEYIAAIX"
      }
    ]


rawData : String
rawData =
    """
    [
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1126/science.8446897"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1COS"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              2.1
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1126/science.278.5335.82"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1FSD"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1126/science.278.5335.82"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1FSV"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
          "nonpolymer_entities": null
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1110/ps.8.7.1400"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1BYZ"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 7,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              0.9
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1110/ps.8.1.84"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1BB1"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 1,
            "disulfide_bond_count": 0,
            "polymer_composition": "heteromeric protein",
            "resolution_combined": [
              1.8
            ]
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
                  "name": "CHLORIDE ION"
                }
              }
            }
          ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1021/bi000208x"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1DJF"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.97.12.6298"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1EC5"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 3,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              2.5
            ]
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
                  "name": "ZINC ION"
                }
              }
            }
          ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1021/bi001242e"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1FMH"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 1,
            "polymer_composition": "heteromeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.98.4.1404"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1G6U"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 2,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              1.48
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1110/ps.9.12.2294"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1HQJ"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 25,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              1.2
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1038/88604"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1IC9"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 1,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1038/88604"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1ICO"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 1,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1038/88604"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1ICL"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 1,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
          "nonpolymer_entities": null
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1006/jmbi.2000.4345"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1FME"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.211536998"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1JY9"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.012583999"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1J4M"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.012583999"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1K43"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.261563398"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1KDD"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 2,
            "polymer_composition": "heteromeric protein",
            "resolution_combined": [
              2.14
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.261563398"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1KD9"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 3,
            "polymer_composition": "heteromeric protein",
            "resolution_combined": [
              2.1
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.261563398"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1KD8"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 3,
            "polymer_composition": "heteromeric protein",
            "resolution_combined": [
              1.9
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1021/ja010506x"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1JM0"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 14,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              1.7
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1021/ja010506x"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1JMB"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 5,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              2.2
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.091100898"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1LE1"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
          "nonpolymer_entities": null
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.091100898"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1LE0"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1073/pnas.091100898"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1LE3"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
          "nonpolymer_entities": null
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1038/nsb798"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1L2Y"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1021/ja0174276"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1JY6"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 1,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
          "nonpolymer_entities": null
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1021/ja0174276"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": 
            "entry_id": "1JY4"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 1,
            "polymer_composition": "homomeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1110/ps.4440102"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1K09"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 0,
            "disulfide_bond_count": 0,
            "polymer_composition": "heteromeric protein",
            "resolution_combined": null
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
          "assemblies": null,
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1016/S0022-2836(02)00114-6"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1KYC"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 3,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              1.45
            ]
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
          "pubmed": {
            "rcsb_pubmed_doi": "10.1006/jsbi.2002.4467"
          },
          "rcsb_binding_affinity": null,
          "rcsb_entry_container_identifiers": {
            "entry_id": "1L4X"
          },
          "rcsb_entry_info": {
            "deposited_nonpolymer_entity_instance_count": 10,
            "disulfide_bond_count": 0,
            "polymer_composition": "homomeric protein",
            "resolution_combined": [
              2
            ]
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
      }
    ]
    """
