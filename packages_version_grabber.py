import os
import json

# Path to the R packages directory
r_packages_dir = "/usr/local/lib/R/site-library/"

# Output JSON file
output_file = "r_packages_versions_and_descriptions.json"

# Dictionary to store package information
packages_info = {}

# Iterate through each package directory
for package in os.listdir(r_packages_dir):
    package_path = os.path.join(r_packages_dir, package)
    description_file = os.path.join(package_path, "DESCRIPTION")
    
    # Check if the DESCRIPTION file exists
    if os.path.isfile(description_file):
        with open(description_file, "r") as desc:
            package_name = None
            package_version = None
            package_description = None
            
            # Read the DESCRIPTION file line by line
            for line in desc:
                if line.startswith("Package:"):
                    package_name = line.split(":", 1)[1].strip()
                elif line.startswith("Version:"):
                    package_version = line.split(":", 1)[1].strip()
                elif line.startswith("Description:"):
                    package_description = line.split(":", 1)[1].strip()
                elif package_description and line.startswith(" "):  # Handle multi-line descriptions
                    package_description += " " + line.strip()
                else:
                    # Stop reading if all fields are found
                    if package_name and package_version and package_description:
                        break
            
            # Add the package info to the dictionary
            if package_name and package_version and package_description:
                packages_info[package_name] = {
                    "version": package_version,
                    "description": package_description
                }

# Write the package information to a JSON file
with open(output_file, "w") as json_file:
    json.dump(packages_info, json_file, indent=4)

print(f"Package information saved to {output_file}")