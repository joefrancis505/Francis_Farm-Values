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
    df[, c("GISJOIN", intersect(columns, names(df)))]
  }, error = function(e) {
    cat("Error in read_csv_columns function:\n")
    cat("File path:", file_path, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    stop(e)
  })
}

add_slavery_legality <- function(df) {
  slave_states <- c(
    "Alabama", "Arkansas", "Delaware", "Florida", "Georgia",
    "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri",
    "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia",
    "Indian Territory", "New Mexico Territory", "Arizona Territory",
    "Arkansas Territory", "Florida Territory", "Kansas Territory", "Missouri Territory",
    "Orleans Territory", "Southwest Territory",
    "West Virginia", "Oklahoma", "Oklahoma Territory",
    "District of Columbia"
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
                              rename(census_pop = ADQ001,
                                     enslaved = AE6003,
                                     farmv_total = ADJ001,
                                     improved = ADI001,
                                     unimproved = ADI002)
                          },
                          "1860" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds14_1860_county.csv",
                                                    c("AG3001", "AHF001", "AH3003"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds13_1860_county.csv",
                                                     c("AGV001", "AGP001", "AGP002"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              rename(census_pop = AG3001,
                                     enslaved = AH3003,
                                     farmv_total = AGV001,
                                     improved = AGP001,
                                     unimproved = AGP002)
                          },
                          "1870" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds17_1870_county.csv",
                                                    c("AK3001", "AK3002", "AK3003", "AK3004", "AKE001"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds16_1870_county.csv",
                                                     c("AJR001", "AJU001", "AJU002", "AJU003", "AJV001"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              rename(census_pop = AJR001,
                                     farmv_total = AJV001,
                                     improved = AJU001,
                                     woodland = AJU002,
                                     other_unimproved = AJU003) %>%
                              mutate(enslaved = 0,
                                     unimproved = woodland + other_unimproved)
                          },
                          "1880" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds23_1880_county.csv",
                                                    c("AO4001", "APP001", "APP002"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds22_1880_county.csv",
                                                     c("AOB001", "AOS001", "AOS002", "AOD001", "AOD002", "AOD003"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              rename(census_pop = AOB001,
                                     enslaved = APP002,
                                     improved = AOS001,
                                     unimproved = AOS002) %>%
                              mutate(farmv_total = AOD001 + AOD002 + AOD003)
                          },
                          "1890" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds27_1890_county.csv",
                                                    c("AUM001", "AUU001", "AV0007", "AV0008"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds26_1890_county.csv",
                                                     c("AUJ001", "AUJ002", "AUK001", "AUK002", "AUK003"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              rename(census_pop = AUM001,
                                     improved = AUJ001,
                                     unimproved = AUJ002) %>%
                              mutate(enslaved = 0,
                                     farmv_total = AUK001 + AUK002 + AUK003)
                          },
                          "1900" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds31_1900_county.csv",
                                                    c("AYM001", "AYT001", "AZ3003", "AZ3004"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds30_1900_county.csv",
                                                     c("AWS001", "AWT001", "AWU001", "AWW001", "AWW002", "AWW003", "AWW004"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              rename(census_pop = AYM001,
                                     improved = AWU001,
                                     total_farmland = AWT001) %>%
                              mutate(enslaved = 0,
                                     unimproved = total_farmland - improved,
                                     farmv_total = AWW001 + AWW002 + AWW003 + AWW004)
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

# Modify the process_year function
process_year <- function(year) {
  cat("Processing year:", year, "\n")
  
  # Read county shapefile
  counties <- st_read(paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp"))
  
  # Keep only necessary columns
  counties <- counties %>%
    select(GISJOIN, STATENAM)
  
  # Rename columns from shapefiles
  counties <- counties %>%
    rename(state = STATENAM)
  
  # Get census data for the year
  census_data <- get_census_data(year)
  
  # Join county data with census data
  counties_df <- counties %>%
    st_drop_geometry() %>%
    left_join(census_data, by = "GISJOIN")
  
  # Add slavery legality dummy variable
  counties_df <- add_slavery_legality(counties_df)
  
  # Add year column
  counties_df$year <- as.integer(year)

  # Select only the requested variables, including only those that exist
  counties_df <- counties_df %>%
    select(GISJOIN, year, state, census_pop, enslaved, 
           any_of(c("farmv_total", "improved", "unimproved", "total_farmland")), 
           slavery_legal)
  
  return(counties_df)
}

# List of years to process
years <- c("1850", "1860", "1870", "1880", "1890", "1900")

# Process all years
all_counties <- map_df(years, process_year)

# Write the result to a CSV file
write_csv(all_counties, "DiD/DiD_database.csv")

# Print a summary of the processed data
cat("Processing complete. Summary of the data:\n")
cat("Total number of rows:", nrow(all_counties), "\n")
cat("Columns:", paste(colnames(all_counties), collapse=", "), "\n")
cat("Data for each year:\n")
print(table(all_counties$year))
cat("\nData has been written to 'DiD/DiD_database.csv'\n")