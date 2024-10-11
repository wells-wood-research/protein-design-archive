# Define the paths to the files
frontend_file_path = "./frontend/src/Urls.elm"
backend_app_file_path = "./backend/src/backend/app.py"
backend_db_file_path = "./backend/src/backend/db.py"

# Define the replacements
replacements = {
    frontend_file_path: {
        "http://localhost:5000": "https://pragmaticproteindesign.bio.ed.ac.uk/pda-api"
    },
    backend_app_file_path: {
        "http://localhost:1234": "https://pragmaticproteindesign.bio.ed.ac.uk"
    },
    backend_db_file_path: {
        "localhost": "pda-mongo"
    }
}

def replace_in_file(file_path, replacements_dict):
    """Replace all occurrences in the file based on the provided dictionary."""
    # Read the file content
    with open(file_path, 'r') as file:
        content = file.read()

    # Replace each occurrence
    for old_value, new_value in replacements_dict.items():
        content = content.replace(old_value, new_value)

    # Write the modified content back to the file
    with open(file_path, 'w') as file:
        file.write(content)
    print(f"Replacements done in {file_path}")

# Apply the replacements for each file
for file_path, file_replacements in replacements.items():
    replace_in_file(file_path, file_replacements)

print("All replacements have been successfully applied.")