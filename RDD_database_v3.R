# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "dplyr", "purrr", "readr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Clear console
cat("\014")

# Function to read specific columns from CSV files with error handling
read_csv_columns <- function(file_path, columns) {
  tryCatch({
    col_types <- cols(
      GISJOIN = col_character(),
      .default = col_double()
    )
    
    df <- read_csv(file_path, col_types = col_types)
    
    if (!"GISJOIN" %in% names(df)) {
      stop("GISJOIN column not found in the CSV file")
    }
    
    missing_cols <- setdiff(columns, names(df))
    if (length(missing_cols) > 0) {
      stop(paste("The following columns are missing from the CSV file:", paste(missing_cols, collapse = ", ")))
    }
    
    df[, c("GISJOIN", columns)]
  }, error = function(e) {
    cat("Error in read_csv_columns function:\n")
    cat("File path:", file_path, "\n")
    cat("Requested columns:", paste(columns, collapse = ", "), "\n")
    cat("Error message:", conditionMessage(e), "\n")
    stop(e)
  })
}

# Function to add slavery legality
add_slavery_legality <- function(df) {
  slave_states <- c(
    "Alabama", "Arkansas", "Delaware", "Florida", "Georgia",
    "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri",
    "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia"
  )
  
  df %>%
    mutate(slavery_legal = as.integer(state %in% slave_states))
}

# Function to get census data for a specific year
get_census_data <- function(year) {
  tryCatch({
    census_data <- switch(as.character(year),
                          "1850" = {
                            pop <- read_csv_columns("Data/Census/nhgis0128_csv/nhgis0128_ds10_1850_county.csv",
                                                    c("ADQ001", "ADZ001", "AE6003"))
                            farm <- read_csv_columns("Data/Census/nhgis0128_csv/nhgis0128_ds9_1850_county.csv",
                                                     c("ADJ001", "ADI001", "ADI002"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              rename(census_pop = ADQ001) %>%
                              transmute(
                                GISJOIN,
                                enslaved = AE6003,
                                farmv_total = ADJ001,
                                land = ADI001 + ADI002,
                                farmv = farmv_total / land,
                                enslaved_share = enslaved / census_pop * 100
                              )
                          },
                          "1860" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds14_1860_county.csv",
                                                    c("AG3001", "AH3003"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds13_1860_county.csv",
                                                     c("AGV001", "AGP001", "AGP002"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                enslaved = AH3003,
                                farmv_total = AGV001,
                                land = AGP001 + AGP002,
                                farmv = farmv_total / land,
                                enslaved_share = enslaved / AG3001 * 100
                              )
                          },
                          stop("Invalid year")
    )
    return(census_data)
  }, error = function(e) {
    cat("Error in get_census_data function for year:", year, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    stop(e)
  })
}

# Function to read geology data
read_geology_data <- function(year, type) {
  file_path <- paste0("Data/Geology/USGS_", type, "/", type, "_", year, ".csv")
  read_csv(file_path) %>%
    select(GISJOIN, paste0(type, "_mean"))
}

# Function to add the border dummy variable
add_border_dummy <- function(df) {
  df %>%
    mutate(border_county = ifelse(is.na(border) | border == 0, 0, 1))
}

# Process_year function
process_year <- function(year) {
  cat("Processing year:", year, "\n")
  
  # Read county shapefile
  counties <- st_read(paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp"))
  
  # Rename columns from shapefiles
  counties <- counties %>%
    rename(
      state = STATENAM,
      year = DECADE
    )
  
  # Get census data for the year
  census_data <- get_census_data(year)
  
  # Read slope and elevation data
  slope_data <- read_geology_data(year, "slope")
  elevation_data <- read_geology_data(year, "elevation")
  
  # Join county data with census data and geology data
  counties_df <- counties %>%
    st_drop_geometry() %>%
    left_join(census_data, by = "GISJOIN") %>%
    left_join(slope_data, by = "GISJOIN") %>%
    left_join(elevation_data, by = "GISJOIN") %>%
    rename(slope = slope_mean, elevation = elevation_mean)
  
  # Add slavery legality dummy variable
  counties_df <- add_slavery_legality(counties_df)
  
  # Add border dummy variable
  counties_df <- add_border_dummy(counties_df)
  
  # Select only the required variables
  counties_df <- counties_df %>%
    select(year, state, slavery_legal, GISJOIN, farmv, slope, elevation, enslaved_share, border_county)
  
  return(counties_df)
}

# List of years to process
years <- c("1850", "1860")

# Process specified years
all_counties <- map_df(years, process_year)

# Write the result to a CSV file
write_csv(all_counties, "RDD_database.csv")

# Print a summary of the processed data
cat("Processing complete. Summary of the data:\n")
cat("Total number of rows:", nrow(all_counties), "\n")
cat("Columns:", paste(colnames(all_counties), collapse=", "), "\n")
cat("Data for each year:\n")
print(table(all_counties$year))
cat("\nData has been written to 'RDD_database.csv'\n")