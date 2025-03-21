# Load required libraries
if (!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

if (!require(remotes)) {
  install.packages("remotes")
  library(remotes)
}

# Path to the JSON file
json_file <- "r_packages_versions_and_descriptions.json"

# Read the JSON file
packages_info <- fromJSON(json_file)

# Function to check if a package is installed with the correct version
is_package_installed <- function(pkg, version) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(FALSE)
  }
  installed_version <- as.character(packageVersion(pkg))
  return(installed_version == version)
}

# Iterate through each package in the JSON file
for (pkg in names(packages_info)) {
  version <- packages_info[[pkg]]$version
  description <- packages_info[[pkg]]$description
  
  cat(sprintf("Processing package: %s (Version: %s)\n", pkg, version))
  
  if (is_package_installed(pkg, version)) {
    cat(sprintf("Package '%s' is already installed with the correct version (%s).\n", pkg, version))
  } else {
    cat(sprintf("Installing or updating package '%s' to version %s...\n", pkg, version))
    tryCatch(
      {
        remotes::install_version(pkg, version = version, repos = "https://cran.r-project.org")
        cat(sprintf("Successfully installed '%s' version %s.\n", pkg, version))
      },
      error = function(e) {
        cat(sprintf("Failed to install '%s' version %s. Error: %s\n", pkg, version, e$message))
      }
    )
  }
}

cat("All packages have been processed.\n")