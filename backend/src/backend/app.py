from flask import Flask
from flask_cors import CORS
import json

from backend.db import CLIENT, DESIGNS, PDA_DB

app = Flask(__name__)
CORS(app, origins=["http://localhost:1234"])

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
    projection = {"_id": 0, "pdb": 1, "picture_path": 1, "authors": 1, "release_date": 1}
    designs = list(DESIGNS.find({}, projection=projection))
    return designs

@app.get("/design-details/<designId>")
def get_design_details(designId):
    """Gets complete data for the one design that this details page is for."""
    projection = {"_id": 0}
    desIdUp = designId.upper()
    design = list(DESIGNS.find({"pdb": desIdUp}, projection, limit=1))
    return design

@app.get("/all-design-pdbs")
def get_all_pdb_codes():
    """Gets complete data for the one design that this details page is for,
    as well as the previous and next design on the list"""
    projection = {"_id": 0, "pdb": 1}
    designPdbs = list(DESIGNS.find({}, projection))
    return designPdbs

@app.get("/three-design-pdbs/<designId>")
def get_surrounding_pdb_codes(designId):
    """Gets complete data for the one design that this details page is for,
    as well as the previous and next design on the list"""
    designPdbs = get_all_pdb_codes()

    for i, d in enumerate(designPdbs):
        if d["pdb"].upper() == designId.upper():
            curr = i

    if curr == 0:
        prev = len(designPdbs)
    else:
        prev = curr - 1

    if curr == len(designPdbs):
        next = 0
    else:
        next = curr + 1

    prevCurrNext = [designPdbs[prev], designPdbs[curr], designPdbs[next]]
    return prevCurrNext

@app.get("/three-designs/<designId>")
def get_surrounding_designs(designId):
    """Gets complete data for the one design that this details page is for,
    as well as the previous and next design on the list"""
    designPdbs = get_all_pdb_codes()
    projection = {"_id" : 0}

    for i, d in enumerate(designPdbs):
        if d["pdb"].upper() == designId.upper():
            curr = i

    if curr == 0:
        prev = len(designPdbs)
    else:
        prev = curr - 1

    if curr == len(designPdbs):
        next = 0
    else:
        next = curr + 1

    prevCurrNext = [designPdbs[prev]["pdb"], designPdbs[curr]["pdb"], designPdbs[next]["pdb"]]

    designs = list(DESIGNS.find({"pdb": {"$in": prevCurrNext}}, projection, limit=3))

    return designs