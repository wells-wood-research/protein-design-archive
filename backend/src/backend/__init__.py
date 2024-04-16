from flask import Flask

app = Flask(__name__)

@app.route("/")
def serve_application():
    return "<p><b>Hello</b>, World!</p>"

@app.get("/all-design-stubs")
def get_all_design_stubs():
    """Gets all the design stub data for the front page."""
    # query the database using pymongo
    return {"design1": "blah"}
