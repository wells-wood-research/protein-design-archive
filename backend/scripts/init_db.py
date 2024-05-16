import json
import typing as t

import click

from backend.db import DESIGNS


@click.command()
@click.argument("input_json_path", type=click.Path(exists=True))
def main(input_json_path):
    """CLI tool for import the database."""
    click.echo(f"Loading input file: {input_json_path}")
    design_records = import_from_json(input_json_path)
    write_records_to_db(design_records)


def import_from_json(json_path) -> t.List[t.Dict[str, t.Any]]:
    with open(json_path) as json_file:
        design_records = json.load(json_file)
    return design_records


def write_records_to_db(design_records):
    returned = DESIGNS.insert_many(design_records)
    click.echo(returned)


if __name__ == "__main__":
    main()
