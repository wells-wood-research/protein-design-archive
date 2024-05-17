from flask import Flask
from flask_cors import CORS

from backend.db import CLIENT, DESIGNS, PDA_DB

app = Flask(__name__)
CORS(app, origins=["http://localhost:1234"])


@app.get("/all-design-stubs")
def get_all_design_stubs():
    """Gets all the design stub data for the front page."""
    # query the database using pymongo
    projection = {"_id": 0}
    designs = list(DESIGNS.find({}, projection=projection))
    return designs
