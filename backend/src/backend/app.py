import json
import typing as t
import csv
import io

from flask import Flask, request
from flask_cors import CORS

from backend.db import CLIENT, DESIGNS, PDA_DB

app = Flask(__name__)
CORS(app, origins=["https://pragmaticproteindesign.bio.ed.ac.uk"])


@app.get("/all-designs")
def get_all_design_data():
    """Gets all the design data."""
    selected_pdb_files_str = request.args.get("pdb-codes")
    print("This:", selected_pdb_files_str)
    designs = []
    projection = {
        "_id": 0,
        "keywords": 0,
        "classification_suggested": 0,
        "classification_suggested_reason": 0,
        "picture_path": 0,
        "abstract": 0,
        "review": 0,
        "previous_design": 0,
        "next_design": 0
    }
    if selected_pdb_files_str:
        pdb_codes = json.loads(selected_pdb_files_str)
        designs = list(DESIGNS.find({"pdb": {"$in": pdb_codes}}, projection=projection))
    else:
        designs = list(DESIGNS.find({}, projection=projection))
    return designs


@app.get("/all-design-stubs")
def get_all_design_stubs():
    """Gets all the design stub data for the front page."""
    # query the database using pymongo
    projection = {
        "_id": 0,
        "pdb": 1,
        "picture_path": 1,
        "authors": 1,
        "subtitle": 1,
        "tags": 1,
        "keywords": 1,
        "release_date": 1,
        "publication": 1,
        "seq_max_sim_natural": 1,
        "struct_max_sim_natural": 1
    }
    designs = list(DESIGNS.find({}, projection=projection))
    return designs


@app.get("/design-details/<designId>")
def get_design_details(designId: str) -> t.Any:
    """Gets complete data for the one design that this details page is for."""
    projection = {"_id": 0}
    design = DESIGNS.find_one({"pdb": designId.lower()}, projection)
    return design
