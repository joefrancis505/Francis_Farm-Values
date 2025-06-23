setwd(getSrcDirectory(function(dummy) {dummy}))
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load required packages
packages <- c("dplyr", "purrr", "readr", "ipumsr", "stringr")
install.packages(packages[!packages %in% rownames(installed.packages())])
invisible(lapply(packages, library, character.only = TRUE))

# IPUMS API Setup
get_api_key <- function() {
  api_key_env <- Sys.getenv("IPUMS_API_KEY")
  
  if (nzchar(api_key_env)) {
    return(api_key_env)
  } else {
    api_key <- readline(prompt = "Enter your IPUMS API key: ")
    if (nzchar(api_key)) {
      Sys.setenv(IPUMS_API_KEY = api_key)
      return(api_key)
    } else {
      stop("API key is required.")
    }
  }
}

set_ipums_default_collection("nhgis")
api_key <- get_api_key()

# Create directories
dir.create("Data/IPUMS_Raw", recursive = TRUE, showWarnings = FALSE)
dir.create("Data/Shapefiles", recursive = TRUE, showWarnings = FALSE)

# Define years and download directory
years <- c("1850", "1860", "1870", "1880", "1890", "1900")
download_dir <- "Data/IPUMS_Raw"

# Minimal data specifications - only essential variables
get_data_specs <- function(year) {
  if (year == "1850") {
    pop_specs <- ds_spec("1850_cPAX", data_tables = "NT6", geog_levels = "county")  # Race/Slave Status
    ag_specs <- ds_spec("1850_cAg", data_tables = c("NT2", "NT3"), geog_levels = "county")  # Farm data
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1860") {
    pop_specs <- ds_spec("1860_cPAX", data_tables = "NT6", geog_levels = "county")
    ag_specs <- ds_spec("1860_cAg", data_tables = c("NT1", "NT2"), geog_levels = "county")
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1870") {
    pop_specs <- ds_spec("1870_cPAX", data_tables = "NT2", geog_levels = "county")
    ag_specs <- ds_spec("1870_cAg", data_tables = c("NT1", "NT2"), geog_levels = "county")
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1880") {
    pop_specs <- ds_spec("1880_cPAX", data_tables = "NT2", geog_levels = "county")
    ag_specs <- ds_spec("1880_cAg", data_tables = c("NT1", "NT9"), geog_levels = "county")
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1890") {
    pop_specs <- ds_spec("1890_cPHAM", data_tables = "NT1", geog_levels = "county")
    ag_specs <- ds_spec("1890_cAg", data_tables = c("NT1", "NT8"), geog_levels = "county")
    return(list(pop_specs, ag_specs))
  }
  else if (year == "1900") {
    pop_specs <- ds_spec("1900_cPHAM", data_tables = "NT1", geog_levels = "county")
    ag_specs <- ds_spec("1900_cAg", data_tables = c("NT1", "NT10", "NT39"), geog_levels = "county")
    return(list(pop_specs, ag_specs))
  }
  else {
    stop(paste("No dataset definitions for year", year))
  }
}

# Download and process data for a specific year
download_year_data <- function(year, api_key, download_dir) {
  year_dir <- file.path(download_dir, year)
  dir.create(year_dir, showWarnings = FALSE)
  
  # Get data specifications
  data_specs <- get_data_specs(year)
  shapefile_name <- paste0("us_county_", year, "_tl2000")
  
  # Define and submit extract
  combined_extract <- define_extract_nhgis(
    description = paste("Data for", year),
    datasets = data_specs,
    shapefiles = shapefile_name
  )
  
  submitted_extract <- submit_extract(combined_extract, api_key = api_key)
  ready_extract <- wait_for_extract(submitted_extract, api_key = api_key)
  
  # Download and extract files
  downloaded_files <- download_extract(ready_extract, download_dir = year_dir, api_key = api_key)
  
  for (zip_file in downloaded_files) {
    if (grepl("\\.zip$", zip_file)) {
      extract_dir <- file.path(year_dir, gsub("\\.zip$", "", basename(zip_file)))
      dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(zip_file, exdir = extract_dir)
    }
  }
  
  return(list(year = year, files = downloaded_files, dir = year_dir))
}

# Organize shapefiles
organize_shapefiles <- function(year_data) {
  year <- year_data$year
  year_dir <- year_data$dir
  
  shapefile_dest_dir <- file.path("Data/Shapefiles", year)
  dir.create(shapefile_dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  shp_files <- list.files(year_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  for (shp_file in shp_files) {
    base_name <- tools::file_path_sans_ext(shp_file)
    base_dir <- dirname(shp_file)
    
    related_files <- list.files(base_dir, pattern = paste0("^", basename(base_name), "\\.[a-zA-Z0-9]+$"), full.names = TRUE)
    
    for (file in related_files) {
      file_dest <- file.path(shapefile_dest_dir, basename(file))
      file.copy(file, file_dest, overwrite = TRUE)
    }
  }
}

# Process CSV files
process_csv_files <- function(year_data) {
  year <- year_data$year
  year_dir <- year_data$dir
  
  csv_files <- list.files(year_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  csv_files <- csv_files[!grepl("codebook", csv_files)]
  
  data_tables <- list()
  for (csv_path in csv_files) {
    ds_match <- str_match(basename(csv_path), "_ds(\\d+)_")
    if (!is.na(ds_match[1, 2])) {
      ds_id <- paste0("ds", ds_match[1, 2])
      data_tables[[ds_id]] <- read_csv(csv_path, show_col_types = FALSE) %>% 
        mutate(GISJOIN = as.character(GISJOIN))
    }
  }
  
  # Combine all tables
  combined_data <- data_tables[[1]]
  if (length(data_tables) > 1) {
    for (i in 2:length(data_tables)) {
      table_to_join <- data_tables[[i]] %>% select(GISJOIN, where(is.numeric))
      combined_data <- full_join(combined_data, table_to_join, by = "GISJOIN")
    }
  }
  
  combined_data$year <- as.integer(year)
  return(combined_data)
}

# Main execution
all_downloads <- map(years, ~download_year_data(.x, api_key, download_dir))
walk(all_downloads, organize_shapefiles)

all_data <- map(all_downloads, process_csv_files)
all_data <- all_data[!sapply(all_data, is.null)]

# Merge and save
merged_data <- bind_rows(all_data)
write_csv(merged_data, "Data/census.csv")