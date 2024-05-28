# Protein Design Archive (PDA) Database

An up-to-date, complete, online resource for the protein design community and beyond.

## Data preparation

### 1. Data collection

Data has been scraped from the RCSB PDB database using the script in ```backend/scripts/data_collection.py```.

It relies on having a list of PDB codes to scrape data for, cif files to scrape information from, and in-house rules for assigning suggested classification for the protein designs.

### 2. Downloading PDB files for display with NGL viewer

Run the following commands:

```chmod +x download_pdbs.sh```

```bash download_pdbs.sh -f pdb_codes.csv -o frontend/static/pdb_files -a```

Make sure to specify the output path correctly and adjusting the ```frontend/static/ngl-web-component.js``` file to point to the correct online path of the pdb files' directory.