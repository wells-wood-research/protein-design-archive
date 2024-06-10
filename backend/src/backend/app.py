import json
import typing as t

from flask import Flask
from flask_cors import CORS

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
