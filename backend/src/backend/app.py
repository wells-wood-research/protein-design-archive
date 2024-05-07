from flask import Flask

from backend.db import CLIENT, PDA_DB, DESIGNS

app = Flask(__name__)


@app.route("/")
def serve_application():
    return "<p><b>Hello</b>, World!</p>"


@app.get("/all-design-stubs")
def get_all_design_stubs():
    """Gets all the design stub data for the front page."""
    # query the database using pymongo
    designs = list(DESIGNS.find())
    for design in designs:
        del design["_id"]
    return designs


def setup_test_database():
    """Temporary test function for setting up the database."""
    test_designs = [
        {"pdb_code": "1al1", "description": "design that is for 1al1"},
        {"pdb_code": "1al2", "description": "design that is for 1al2"},
        {"pdb_code": "1al3", "description": "design that is for 1al3"},
        {"pdb_code": "1xx5", "description": "design that is for 1xx5"},
        {"pdb_code": "4al7", "description": "design that is for 4al7"},
    ]
    _ = DESIGNS.insert_many(test_designs)
    return

