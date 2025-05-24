# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "dplyr", "purrr", "readr", "ipumsr", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# --- IPUMS API Setup ---

# Function to get API key from user or environment
get_api_key <- function() {
  api_key_env <- Sys.getenv("IPUMS_API_KEY")
  
  if (nzchar(api_key_env)) {
    cat("Using IPUMS API key found in environment variable IPUMS_API_KEY.\n")
    api_key <- api_key_env
  } else {
    ipums_url <- "https://account.ipums.org/api_keys"
    cat("This script requires an IPUMS API key, which can be obtained at", ipums_url, "\n")
    api_key <- readline(prompt = "Please enter your API key: ")
    
    # Set the key in the current session environment
    if (nzchar(api_key)) {
      Sys.setenv(IPUMS_API_KEY = api_key)
      cat("IPUMS API key has been set for this session.\n")
    }
  }
  
  if (!nzchar(api_key)) {
    stop("API key is required to download NHGIS data. Exiting.")
  }
  cat("IPUMS API key accepted for this session.\n")
  
  return(api_key)
}

# Set the default IPUMS collection
set_ipums_default_collection("nhgis")

# Get API key
api_key <- get_api_key()

# Create necessary directories
required_dirs <- c(
  "Data/IPUMS_Raw",
  "Data/Shapefiles"
)

# Create each directory if it doesn't exist
for (dir_path in required_dirs) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

# Define years to download
years <- c("1850", "1860", "1870", "1880", "1890", "1900")

# Define download directory
download_dir <- "Data/IPUMS_Raw"

# Function to recursively extract all zip files in a directory
extract_all_zips <- function(directory) {
  # Get all zip files in the directory (recursively)
  zip_files <- list.files(directory, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  
  if (length(zip_files) == 0) {
    return(invisible())
  }
  
  cat(sprintf("Found %d zip files to extract in %s\n", length(zip_files), directory))
  
  for (zip_file in zip_files) {
    extract_dir <- gsub("\\.zip$", "", zip_file)
    if (!dir.exists(extract_dir)) {
      cat("Extracting:", basename(zip_file), "to", extract_dir, "\n")
      dir.create(extract_dir, recursive = TRUE)
      tryCatch({
        utils::unzip(zip_file, exdir = extract_dir)
      }, error = function(e) {
        warning(sprintf("Failed to extract %s: %s", basename(zip_file), e$message))
      })
    }
  }
  
  # Check if new zip files were created during extraction
  new_zip_files <- list.files(directory, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  new_zip_files <- new_zip_files[!new_zip_files %in% zip_files]
  
  if (length(new_zip_files) > 0) {
    extract_all_zips(directory)  # Recursively extract new zip files
  }
}

# Function to get data specifications for a year (combines specs from both original scripts)
get_data_specs <- function(year) {
  if (year == "1850") {
    pop_specs <- ds_spec(
      "1850_cPAX", 
      data_tables = c("NT1", "NT2", "NT6"),
      geog_levels = "county"
    )
    
    ag_specs <- ds_spec(
      "1850_cAg",
      data_tables = c("NT2", "NT3", "NT6"),
      geog_levels = "county"
    )
    
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1860") {
    pop_specs <- ds_spec(
      "1860_cPAX", 
      data_tables = c("NT1", "NT6"),
      geog_levels = "county"
    )
    
    ag_specs <- ds_spec(
      "1860_cAg",
      data_tables = c("NT1", "NT2", "NT5"),
      geog_levels = "county"
    )
    
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1870") {
    pop_specs <- ds_spec(
      "1870_cPAX", 
      data_tables = c("NT2", "NT4"),
      geog_levels = "county"
    )
    
    ag_specs <- ds_spec(
      "1870_cAg",
      data_tables = c("NT1", "NT2", "NT3", "NT9"),
      geog_levels = "county"
    )
    
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1880") {
    pop_specs <- ds_spec(
      "1880_cPAX", 
      data_tables = c("NT2", "NT4"),
      geog_levels = "county"
    )
    
    ag_specs <- ds_spec(
      "1880_cAg",
      data_tables = c("NT1", "NT9", "NT11", "NT15A"),
      geog_levels = "county"
    )
    
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1890") {
    pop_specs <- ds_spec(
      "1890_cPHAM", 
      data_tables = c("NT1", "NT2", "NT6"),
      geog_levels = "county"
    )
    
    ag_specs <- ds_spec(
      "1890_cAg",
      data_tables = c("NT1", "NT8", "NT9A", "NT21"),
      geog_levels = "county"
    )
    
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1900") {
    pop_specs <- ds_spec(
      "1900_cPHAM", 
      data_tables = c("NT1", "NT2", "NT7"),
      geog_levels = "county"
    )
    
    ag_specs <- ds_spec(
      "1900_cAg",
      data_tables = c("NT1", "NT10", "NT11", "NT13", "NT39"),
      geog_levels = "county"
    )
    
    return(list(pop_specs, ag_specs))
  }
  else {
    stop(paste("No dataset definitions for year", year))
  }
}

# Function to download data for a specific year
download_year_data <- function(year, api_key, download_dir) {
  year_dir <- file.path(download_dir, year)
  if (!dir.exists(year_dir)) {
    dir.create(year_dir)
  }
  
  cat(sprintf("\n--- Downloading data for year: %s ---\n", year))
  
  # Get data specifications for this year
  data_specs <- get_data_specs(year)
  
  # Define shapefile name - using 2000 TIGER files
  shapefile_name <- paste0("us_county_", year, "_tl2000")
  
  # Define extract with all data tables and shapefile
  combined_extract <- define_extract_nhgis(
    description = paste("Combined data for", year),
    datasets = data_specs,
    shapefiles = shapefile_name
  )
  
  # Submit and wait for extract
  cat("Submitting extract request...\n")
  submitted_extract <- submit_extract(combined_extract, api_key = api_key)
  
  cat("Waiting for extract to complete...\n")
  ready_extract <- wait_for_extract(submitted_extract, api_key = api_key)
  
  # Download extract
  cat("Downloading extract files...\n")
  downloaded_files <- download_extract(
    ready_extract, 
    download_dir = year_dir, 
    api_key = api_key, 
    overwrite = FALSE
  )
  
  # Extract the downloaded zip files
  for (zip_file in downloaded_files) {
    if (file.exists(zip_file) && grepl("\\.zip$", zip_file)) {
      extract_dir <- file.path(year_dir, gsub("\\.zip$", "", basename(zip_file)))
      if (!dir.exists(extract_dir)) {
        cat("Extracting:", basename(zip_file), "\n")
        dir.create(extract_dir, recursive = TRUE)
        utils::unzip(zip_file, exdir = extract_dir)
      }
    }
  }
  
  # Recursively extract all zip files
  cat("Performing deep extraction of all zip files...\n")
  extract_all_zips(year_dir)
  
  cat(sprintf("Data for %s downloaded and extracted.\n", year))
  
  return(list(
    year = year,
    files = downloaded_files,
    dir = year_dir
  ))
}

# Function to organize shapefiles
organize_shapefiles <- function(year_data) {
  year <- year_data$year
  year_dir <- year_data$dir
  
  shapefile_dest_dir <- file.path("Data/Shapefiles", year)
  if (!dir.exists(shapefile_dest_dir)) {
    dir.create(shapefile_dest_dir, recursive = TRUE)
  }
  
  # Find shapefiles in downloaded data
  shp_files <- list.files(year_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  if (length(shp_files) == 0) {
    warning(paste("No shapefile found for year", year))
    return(FALSE)
  }
  
  # Get all related files (shp, shx, dbf, prj, etc.)
  for (shp_file in shp_files) {
    base_name <- tools::file_path_sans_ext(shp_file)
    base_dir <- dirname(shp_file)
    
    # Get all files with the same base name
    related_files <- list.files(
      base_dir, 
      pattern = paste0("^", basename(base_name), "\\.[a-zA-Z0-9]+$"), 
      full.names = TRUE
    )
    
    # Copy all related files to destination
    for (file in related_files) {
      file_dest <- file.path(shapefile_dest_dir, basename(file))
      if (!file.exists(file_dest)) {
        file.copy(file, file_dest)
        cat("Copied", basename(file), "to", shapefile_dest_dir, "\n")
      }
    }
  }
  
  return(TRUE)
}

# Function to process CSV files for a specific year
process_csv_files <- function(year_data) {
  year <- year_data$year
  year_dir <- year_data$dir
  
  # Find all CSV files
  csv_files <- list.files(year_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  csv_files <- csv_files[!grepl("codebook", csv_files)]
  
  if (length(csv_files) == 0) {
    warning(paste("No CSV files found for year", year))
    return(NULL)
  }
  
  # Read all CSV files
  data_tables <- list()
  for (csv_path in csv_files) {
    # Extract dataset ID from filename
    ds_match <- str_match(basename(csv_path), "_ds(\\d+)_")
    if (length(ds_match) > 0 && !is.na(ds_match[1, 2])) { 
      ds_id <- paste0("ds", ds_match[1, 2])
      cat(sprintf("Reading dataset %s from %s...\n", ds_id, basename(csv_path)))
      data_tables[[ds_id]] <- tryCatch({
        read_csv(csv_path, show_col_types = FALSE) %>% 
          mutate(GISJOIN = as.character(GISJOIN))
      }, error = function(e) {
        warning(sprintf("Error reading %s: %s", basename(csv_path), e$message))
        NULL
      })
    }
  }
  
  # Filter out any tables that failed to read
  data_tables <- data_tables[!sapply(data_tables, is.null)]
  
  if (length(data_tables) == 0) {
    warning(paste("Failed to read any valid data tables for year", year))
    return(NULL)
  }
  
  # Combine all tables
  cat("Joining all data tables for year", year, "...\n")
  combined_data <- data_tables[[1]]
  if (length(data_tables) > 1) {
    for (i in 2:length(data_tables)) {
      table_to_join <- data_tables[[i]] %>% select(GISJOIN, where(is.numeric))
      combined_data <- full_join(combined_data, table_to_join, by = "GISJOIN")
    }
  }
  
  # Add year column
  combined_data$year <- as.integer(year)
  
  return(combined_data)
}

# Function to merge all census data into a single CSV
merge_census_data <- function(all_data) {
  # Combine all years
  cat("Merging all census data...\n")
  merged_data <- bind_rows(all_data)
  
  # Write to a single CSV file
  output_file <- "Data/census.csv"
  write_csv(merged_data, output_file)
  
  cat("All census data has been merged into", output_file, "\n")
  cat("Total rows:", nrow(merged_data), "\n")
  cat("Total columns:", ncol(merged_data), "\n")
  cat("Years included:", paste(sort(unique(merged_data$year)), collapse=", "), "\n")
  
  return(output_file)
}

# Main function
main <- function() {
  cat("=== Starting IPUMS Data Downloader ===\n")
  
  # Download data for all years
  all_downloads <- list()
  for (year in years) {
    all_downloads[[year]] <- download_year_data(year, api_key, download_dir)
  }
  
  # Organize shapefiles
  cat("\n=== Organizing Shapefiles ===\n")
  for (year in years) {
    organize_shapefiles(all_downloads[[year]])
  }
  
  # Process CSV files
  cat("\n=== Processing Census CSV Files ===\n")
  all_data <- list()
  for (year in years) {
    all_data[[year]] <- process_csv_files(all_downloads[[year]])
  }
  
  # Merge all census data
  cat("\n=== Merging Census Data ===\n")
  merged_file <- merge_census_data(all_data[!sapply(all_data, is.null)])
  
  cat("\n=== Data Processing Complete ===\n")
  cat("The following files have been created:\n")
  cat("1. Shapefiles organized in Data/Shapefiles/[YEAR]/ directories\n")
  cat("2. Combined census data in", merged_file, "\n")
}

# Run main function
main()