import pandas as pd
from json import loads, dumps
import urllib.request
from bs4 import BeautifulSoup
import os
import requests
import Bio
from Bio.PDB.MMCIFParser import MMCIFParser
from Bio.PDB.MMCIF2Dict import MMCIF2Dict
import re
import json
import sys
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

nltk.download('punkt')
nltk.download('stopwords')

# Suggesting classification relies partially on labels given by D.N. Woolfson
# as part of his work on the "A Brief History of De Novo Protein Design (...)" paper
dek_data = pd.read_json("./dek_classification.csv").sort_values(by="pdb").reset_index(drop=True)
# The rest of classification suggestion relies on authors:
class_dict = {
    "small, non-systematic, and other":["minimal"],
    "engineered":["engineered"],
    "D.N. Woolfson":["rational"],
    "C.W. Wood":["computational"],
    "D. Baker":["computational", "deep-learning based"],
    "W.F. DeGrado":["computational", "consensus"], #"minimal", "rational", "physics-based"
    "M.H. Hecht":["minimal"],
    "J.S. Richardson":["minimal"],
    "D.C. Richardson":["minimal"],
    "P.L. Dutton":["minimal"],
    "R.S. Hodges":["minimal"],
    "L. Regan":["rational", "consensus"],
    "T. Alber":["rational", "computational", "physics-based"],
    "V.P. Conticello":["rational"],
    "P.S. Kim":["rational", "computational", "physics-based"],
    "V.L. Pecoraro":["rational"],
    "T. Kortemme":["rational"],
    "L. Serrano":["rational"],
    "J.M. Berg":["consensus"],
    "B. Imperiali":["consensus"],
    "Z.-Y. Peng":["consensus"],
    "A. Pluckthun":["consensus"],
    "A.M. Buckle":["consensus"],
    "M. Lehmann":["consensus"],
    "P. Minard":["consensus"],
    "O. Rackham":["consensus"],
    "P.B. Harbury":["computational", "physics-based"],
    "B.R. Donald":["computational"],
    "A.R. Thomson":["computational"],
    "S.L. Mayo":["computational"]
    }

# Need cif files downloaded locally
cif_dir_path = "./data/cif_files/"

# Load a list of pdb codes to collect data for
pdb_codes_horizontal = pd.read_csv("./pdb_codes.csv", sep=",", header=None, axis=1).reset_index(drop=True)
pdb_codes = pdb_codes_horizontal.transpose()

# Initialise dataframe with needed columns
data = pd.DataFrame(columns=["pdb","picture_path", "chains", "authors", "classification", "classification_suggested", "classification_suggested_reason", "subtitle", "tags", "keywords", "release_date", "publication", "publication_ref",  "publication_country", "abstract", "related_pdb", "crystal_structure", "exptl_method", "formula_weight", "synthesis_comment", "review",  "previous_design", "next_design"])

### FUNCTIONS FOR COLLECTING DATA:

def get_picture_path(pdb):
    picture_path = ""
    try:
        picture_path = "https://cdn.rcsb.org/images/structures/" + pdb + "_assembly-1.jpeg"
        response = requests.get(picture_path)
        if response.status_code == 200:
            pass
        else:
            print(pdb + " doesn't have valid picture.")
            summary[pdb].append("Invalid picture.")
            picture_path = ""
    except:
        picture_path = ""

    if (not picture_path) or (picture_path == ""):
        summary[pdb].append("Invalid picture.")
        
    return picture_path

def get_authors(pdb, cif_dict):
    try:
        auth = []
        authors = []
        i = 0
        while i < 100000:
            try:
                if cif_dict["_citation_author.citation_id"][i] == 'primary':
                    auth.append(cif_dict["_citation_author.name"][i].strip())
                    i += 1
                else:
                    break
            except IndexError:
                break
        for author in auth:
            forename = author.split(",")[1].strip()
            surname = author.split(",")[0].strip()
            authors.append({"forename":forename, "surname": surname})
    except:
        authors = []

    if not authors:
        summary[pdb].append("Missing authors.")
        
    return authors

def get_classification(pdb, authors, class_dict):
    classification = "unknown"
    classification_suggested = []
    classification_suggested_reason = []
    
    dek_pdb = dek_data[dek_data["PDB"] == pdb]
    if len(dek_pdb) != 0:
        dek_class = dek_pdb["Classification"].values[0]
        try:
            classification_suggested = class_dict[dek_class]
        except:
            classification_suggested = []
        classification_suggested_reason = ["Dek's classification: " + dek_class]

    for author in authors:
        key = author["forename"] + " " + author["surname"]
        try:
            classification_suggested += class_dict[key]
            classification_suggested_reason += ["Author is: " + key]
        except:
            pass

    if not classification_suggested:
        summary[pdb].append("No suggestion for classification.")
        
    return classification, classification_suggested, classification_suggested_reason

def get_release_date(pdb, cif_dict):
    try:
        release_date = cif_dict["_pdbx_audit_revision_history.revision_date"][0].strip()
    except:
        release_date = '1900-01-01'

    if (not release_date) or (release_date == ""):
        summary[pdb].append("No release date.")
        
    return release_date

def get_publication(pdb, cif_dict):
    publication = ""
    publication_country = ""
    publication_fields = []
    publication_ref = {"DOI":"", "PubMed":"", "CSD":"", "ISSN":"", "ASTM":""}
    
    try:
        publication_title = "\"" + cif_dict["_citation.title"][0].strip().rstrip('.') + "\""
    except:
        publication_title = ""
    try:
        publication_journal_abbrev = cif_dict["_citation.journal_abbrev"][0]
    except:
        publication_journal_abbrev = ""
        
    if ("to be published" in publication_journal_abbrev.lower()) or (publication_journal_abbrev == "") or (not publication_journal_abbrev) or ("tba" in publication_title.lower()):
        summary[pdb].append("Publication \"to be published\" ")
        publication = "To be published"
    
    else:
        try:
            publication_journal_volume = cif_dict["_citation.journal_volume"][0].strip().rstrip('.')
        except:
            publication_journal_volume = ""
        try:
            publication_page_first = cif_dict["_citation.page_first"][0].strip().rstrip('.')
        except:
            publication_page_first = ""
        try:
            publication_page_last = cif_dict["_citation.page_last"][0].strip().rstrip('.')
        except:
            publication_page_last = ""
        try:
            publication_id_astm = cif_dict["_citation.journal_id_ASTM"][0].strip().rstrip('.')
        except:
            publication_id_astm = ""
        try:
            publication_country = cif_dict["_citation.country"][0].strip()
        except:
            publication_country = ""
        try:
            publication_id_issn = cif_dict["_citation.journal_id_ISSN"][0].strip().rstrip('.')
        except:
            publication_id_issn = ""
        try:
            publication_id_csd = cif_dict["_citation.journal_id_CSD"][0].strip().rstrip('.')
        except:
            publication_id_csd = ""
        try:
            publication_id_pubmed = cif_dict["_citation.pdbx_database_id_PubMed"][0].strip().rstrip('.')
        except:
            publication_id_pubmed = ""
        try:
            publication_id_doi = cif_dict["_citation.pdbx_database_id_DOI"][0].strip().rstrip('.')
        except:
            publication_id_doi = ""

        if (not publication_page_last or publication_page_last == "" or publication_page_last == "?"):
            if (not publication_page_first or publication_page_first == "" or publication_page_first == "?"):
                publication_page_range = ""
            else:
                publication_page_range = publication_page_first
        else:
            if (not publication_page_first or publication_page_first == "" or publication_page_first == "?"):
                publication_page_range = publication_page_last
            else:
                publication_page_range = publication_page_first + "-" + publication_page_last
                
        publication_fields = [publication_title, publication_journal_abbrev, publication_journal_volume, publication_page_range]
        for i in publication_fields:
            if (not i) or (i == "") or (i == "?"):
                pass
            else:
                if not publication or publication == "":
                    publication = i
                else:
                    publication = publication + ", " + i
        
        publication_ref["DOI"] = (publication_id_doi if publication_id_doi != "?" else "")
        publication_ref["PubMed"] = (publication_id_pubmed if publication_id_pubmed != "?" else "")
        publication_ref["CSD"] = (publication_id_csd if publication_id_csd != "?" else "")
        publication_ref["ISSN"] = (publication_id_issn if publication_id_issn != "?" else "")
        publication_ref["ASTM"] = (publication_id_astm if publication_id_astm != "?" else "")

    if (publication == "") or (not publication) or (publication == "To be published"):
        summary[pdb].append("No publication citation info.")

    try:
        if (publication_ref["DOI"] == "") or (not publication_ref["DOI"]):
            summary[pdb].append("Missing DOI")
    except:
        pass

    return publication, publication_ref, publication_country

def get_chains(pdb, cif_dict):
    try:
        i = 0
        chains = []
        seq_unnat = cif_dict["_entity_poly.pdbx_seq_one_letter_code"]
        seq_nat = cif_dict["_entity_poly.pdbx_seq_one_letter_code_can"]
        seq_id = cif_dict["_entity_poly.pdbx_strand_id"]
        while i < 100000:
            try:
                chain_seq_unnat = seq_unnat[i].strip().replace("\n", "")
                chain_seq_nat = seq_nat[i].strip().replace("\n", "")
                chain_id = seq_id[i].strip()
                chains.append({"chain_id": chain_id, "chain_seq_unnat": chain_seq_unnat, "chain_seq_nat": chain_seq_nat})
                i+=1
            except IndexError:
                break
    except:
        chains = []

    if not chains:
        summary[pdb].append("Missing sequence information.")
    
    return chains

def get_tags(pdb, cif_dict):
    tags = []
    try:
        subtitle = [title.capitalize().strip().rstrip('.') for title in cif_dict["_struct.title"]][0]
    except:
        summary[pdb].append("No keyword.")
        subtitle = ""
    try:
        keyword_struct_pdbx_descriptor = [keyword.lower().strip().rstrip('.') for keyword in cif_dict["_struct_keywords.pdbx_keywords"]]
    except:
        keyword_struct_pdbx_descriptor = []
    try:
        keyword_text = [keyword.lower().strip().rstrip('.') for keyword in cif_dict["_struct_keywords.text"][0].split(",")]
    except:
        keyword_text = []
    try:
        for tag in (keyword_struct_pdbx_descriptor+keyword_text):
            if tag not in tags:
                tags.append(tag.strip())
    except:
        tags = []

    if (not subtitle) or (subtitle == ""):
        summary[pdb].append("No subtitle.")
    
    if not tags:
        summary[pdb].append("No tags.")

    tags = list(set(tags))
    
    return subtitle, tags

def get_xray(pdb, cif_dict):
    try:
        xray_cell_length_a = cif_dict["_cell.length_a"][0].strip()
        xray_cell_length_b = cif_dict["_cell.length_b"][0].strip()
        xray_cell_length_c = cif_dict["_cell.length_c"][0].strip()
    except:
        xray_cell_length_a = ""
        xray_cell_length_b = ""
        xray_cell_length_c = ""
    try:
        xray_cell_angle_alpha = cif_dict["_cell.angle_alpha"][0].strip()
        xray_cell_angle_beta = cif_dict["_cell.angle_beta"][0].strip()
        xray_cell_angle_gamma = cif_dict["_cell.angle_gamma"][0].strip()
    except:
        xray_cell_angle_alpha = ""
        xray_cell_angle_beta = ""
        xray_cell_angle_gamma = ""
    try:
        xray_symmetry_space_group_name_H_M = cif_dict["_symmetry.space_group_name_H-M"].strip()
    except:
        xray_symmetry_space_group_name_H_M = ""
        
    crystal_structure = {"length_a":xray_cell_length_a, "length_b":xray_cell_length_b, "length_c":xray_cell_length_c, "angle_a":xray_cell_angle_alpha, "angle_b":xray_cell_angle_beta, "angle_g":xray_cell_angle_gamma}

    for key, value in crystal_structure.items():
        if (not value) or (value == "") or (value == "?"):
            crystal_structure[key] = ""
    
    for key, value in crystal_structure.items():
        if (not value) or (value == ""):
            summary[pdb].append("Missing crystal structure information.")
            break
    
    return crystal_structure

def get_exptl(pdb, cif_dict):
    try:
        exptl_method = [exptl.upper().strip().rstrip('.') for exptl in cif_dict["_exptl.method"]]
    except:
        exptl_method = ""
    try:
        formula_weight = cif_dict["_entity.formula_weight"][0].strip()
    except:
        formula_weight = ""
    try:
        synthesis_comment = cif_dict["_pdbx_entity_src_syn.details"][0].capitalize().strip().rstrip('.')
    except:
        synthesis_comment =  ""

    if (not exptl_method) or (exptl_method == ""):
        summary[pdb].append("Missing exptl_method information.")
    if (not formula_weight) or (formula_weight == ""):
        summary[pdb].append("Missing formula_weight information.")
    if (not synthesis_comment) or (synthesis_comment == ""):
        summary[pdb].append("Missing synthesis_comment information.")
    
    return exptl_method, formula_weight, synthesis_comment

def get_related(pdb, cif_dict):
    try:
        i = 0
        related_pdbs = []
        while i < 100000:
            try:
                related = cif_dict["_pdbx_database_related.db_id"][i]
                related_pdbs.append(related.lower())
                i+=1
            except IndexError:
                break
    except:
        related_pdbs = []
        
    return related_pdbs

def get_prev_and_next_design(dataframe):
    for i in range(len(dataframe)):
        previous_index = i - 1 if i > 0 else (len(dataframe) - 1)
        next_index = i + 1 if i < (len(dataframe) - 1) else 0
        
        dataframe.at[i, "previous_design"] = dataframe.at[previous_index, "pdb"]
        dataframe.at[i, "next_design"] = dataframe.at[next_index, "pdb"]

def convert_weight_to_float(dataframe):
    dataframe["formula_weight"] = dataframe["formula_weight"].astype(float)

def get_abstract(pdb):
    try:
        url = "https://www.rcsb.org/structure/"+pdb
        with urllib.request.urlopen(url) as response:
            xml_content = response.read()
        soup = BeautifulSoup(xml_content, "html.parser")
        text = soup.text
        # Extract abstract
        abstract_match = re.search(r"PubMed Abstract:\xa0([^&]+)", text)
        if abstract_match:
            abstract = abstract_match.group(1)
        else:
            abstract = "No description found."
    except:
        abstract = "No description found."

    if (not abstract) or (abstract == "") or ("abctract" == "No description found."):
        summary[pdb].append("No abstract description found.")
    
    return abstract

def extract_keywords_nltk(text):
    stop_words = set(stopwords.words('english'))
    word_tokens = word_tokenize(text)
    filtered_words = [word.lower() for word in word_tokens if word.lower() not in stop_words and word.isalnum()]
    return list(set(filtered_words))

### ALL DATA FILLED IN:

def fill_data(data, pdb):
    summary[pdb] = []
    
    try:
        cif_dict = MMCIF2Dict(cif_dir_path+pdb.upper()+".cif")
    except:
        print(pdb+" file not found.")

    picture_path = get_picture_path(pdb)
    authors = get_authors(pdb, cif_dict)
    classification, classification_suggested, classification_suggested_reason = get_classification(pdb, authors, class_dict)
    release_date = get_release_date(pdb, cif_dict)
    publication, publication_ref, publication_country = get_publication(pdb, cif_dict)
    chains = get_chains(pdb, cif_dict)
    subtitle, tags = get_tags(pdb, cif_dict)
    crystal_structure = get_xray(pdb, cif_dict)
    exptl_method, formula_weight, synthesis_comment = get_exptl(pdb, cif_dict)
    related_pdb = get_related(pdb, cif_dict)
    review = True
    abstract = get_abstract(pdb)
    keywords = extract_keywords_nltk(abstract)
    
    pdb_data = {"pdb":pdb, "picture_path":picture_path, "chains":chains, "authors":authors, 
                "classification":classification, "classification_suggested":classification_suggested, "classification_suggested_reason":classification_suggested_reason, 
                "subtitle":subtitle, "tags":tags, "keywords":keywords, "release_date":release_date, 
                "publication":publication, "publication_ref":publication_ref, "publication_country":publication_country,
                "abstract":abstract, "related_pdb":related_pdb, "crystal_structure":crystal_structure, 
                "exptl_method":exptl_method, "formula_weight":formula_weight, "synthesis_comment":synthesis_comment, 
                "review":review}
    
    for key in pdb_data:
        if pdb_data[key] == "?":
            pdb_data[key] = ""
        try:
            pdb_data[key] = pdb_data[key].replace("\n", " ")
        except:
            continue

    index = len(data.index)
    for key, value in pdb_data.items():
        data.at[index, key] = value
    
    return data

def main(data, pdb_codes):
    summary = {}

    for i, pdb_code in pdb_codes[0].items():
        print(str(i) + "/" + str(len(pdb_codes)))
        data = fill_data(data, pdb_code)

    prev_next_data = get_prev_and_next_design(data)
    float_prev_next_data = convert_weight_to_float(prev_next_data)

    float_prev_next_data.to_json("data.json", orient="records", indent=4)
    summary.to_json("summary.json", orient="records", indent=4)



