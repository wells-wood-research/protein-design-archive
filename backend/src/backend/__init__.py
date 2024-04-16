import pymongo
from flask import Flask

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


def setup_test_database() -> pymongo.MongoClient:
    """Temporary test function for setting up the database."""
    client = pymongo.MongoClient("localhost", 27017)
    pda_db = client.pda
    designs = pda_db.designs
    test_designs = [
        {"pdb_code": "1al1", "description": "design that is for 1al1"},
        {"pdb_code": "1al2", "description": "design that is for 1al2"},
        {"pdb_code": "1al3", "description": "design that is for 1al3"},
        {"pdb_code": "1xx5", "description": "design that is for 1xx5"},
        {"pdb_code": "4al7", "description": "design that is for 4al7"},
    ]
    _ = designs.insert_many(test_designs)
    return client, pda_db, designs


# Setup database connection
CLIENT, PDA_DB, DESIGNS = setup_test_database()
