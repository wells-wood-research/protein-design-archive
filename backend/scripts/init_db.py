import argparse
import json


def main():
    parser = argparse.ArgumentParser(
        prog="init_db",
        description="Initialises PDA database.",
    )
    parser.add_argument("filename", help="Path to raw design data from database.")
    args = parser.parse_args()

    with open(args.filename, "r") as inf:
        print(args.filename)
        # designs = json.load(inf)
        # print(designs)


def import_from_json():
    return
