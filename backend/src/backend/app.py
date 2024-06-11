import json
import typing as t

from flask import Flask
from flask_cors import CORS

from backend.db import CLIENT, DESIGNS, PDA_DB

app = Flask(__name__)
CORS(app, origins=["https://pragmaticproteindesign.bio.ed.ac.uk"])


@app.get("/all-designs")
def get_all_design_data():
    """Gets all the design data."""
    projection = {"_id": 0}
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
        "publication": 1
    }
    designs = list(DESIGNS.find({}, projection=projection))
    return designs


@app.get("/design-details/<designId>")
def get_design_details(designId: str) -> t.Any:
    """Gets complete data for the one design that this details page is for."""
    projection = {"_id": 0}
    design = DESIGNS.find_one({"pdb": designId.lower()}, projection)
    return design


def get_surrounding_pdb_codes(designId) -> t.Tuple[t.Optional[str], t.Optional[str]]:
    """Gets complete data for the one design that this details page is for,
    as well as the previous and next design on the list"""
    projection = {"_id": 0, "pdb": 1}
    design_pdbs = sorted(
        list(set([d["pdb"] for d in list(DESIGNS.find({}, projection))]))
    )

    previous_design: t.Optional[str] = None
    next_design: t.Optional[str] = None

    try:
        current_design_index = design_pdbs.index(designId.upper())
        previous_design_index = current_design_index - 1
        next_design_index = current_design_index + 1
        if (previous_design_index) > 0:
            previous_design = design_pdbs[previous_design_index]
        else:
            previous_design = design_pdbs[-1]
        if (next_design_index) < (len(design_pdbs) - 1):
            next_design = design_pdbs[next_design_index]
        else:
            next_design = design_pdbs[0]

    except ValueError:
        pass
    return (previous_design, next_design)
