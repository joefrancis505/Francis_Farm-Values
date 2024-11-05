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

# Function to get census data for a specific year
get_census_data <- function(year) {
  tryCatch({
    census_data <- switch(as.character(year),
                          "1850" = {
                            pop <- read_csv_columns("Data/Census/nhgis0128_csv/nhgis0128_ds10_1850_county.csv",
                                                    c("ADQ001", "AE6003"))
                            farm <- read_csv_columns("Data/Census/nhgis0128_csv/nhgis0128_ds9_1850_county.csv",
                                                     c("ADJ001", "ADI001", "ADI002"))
                            crops <- read_csv_columns("Data/Census/nhgis0138_csv/nhgis0138_ds9_1850_county.csv",
                                                      c("ADM003", "ADM007"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              left_join(crops, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = ADQ001,
                                enslaved = AE6003,
                                farmv_total = ADJ001,
                                improved = ADI001,
                                unimproved = ADI002,
                                land = improved + unimproved,
                                farmv = farmv_total / land,
                                pc_enslaved = enslaved / census_pop * 100,
                                cotton = ADM007 * 400,
                                corn = ADM003
                              )
                          },
                          "1860" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds14_1860_county.csv",
                                                    c("AG3001", "AH3003"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds13_1860_county.csv",
                                                     c("AGV001", "AGP001", "AGP002"))
                            crops <- read_csv_columns("Data/Census/nhgis0138_csv/nhgis0138_ds13_1860_county.csv",
                                                      c("AGY003", "AGY007"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              left_join(crops, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = AG3001,
                                enslaved = AH3003,
                                farmv_total = AGV001,
                                improved = AGP001,
                                unimproved = AGP002,
                                land = improved + unimproved,
                                farmv = farmv_total / land,
                                pc_enslaved = enslaved / census_pop * 100,
                                cotton = AGY007 * 400,
                                corn = AGY003
                              )
                          },
                          "1870" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds17_1870_county.csv",
                                                    c("AK3001"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds16_1870_county.csv",
                                                     c("AJR001", "AJU001", "AJU002", "AJU003", "AJV001"))
                            crops <- read_csv_columns("Data/Census/nhgis0138_csv/nhgis0138_ds16_1870_county.csv",
                                                      c("AJ1004", "AJ1010"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              left_join(crops, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = AJR001,
                                enslaved = 0,
                                farmv_total = AJV001,
                                improved = AJU001,
                                unimproved = AJU002 + AJU003,
                                land = improved + unimproved,
                                farmv = farmv_total / land,
                                pc_enslaved = 0,
                                cotton = AJ1010 * 400,
                                corn = AJ1004
                              )
                          },
                          "1880" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds23_1880_county.csv",
                                                    c("AO4001"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds22_1880_county.csv",
                                                     c("AOB001", "AOS001", "AOS002", "AOD001", "AOD002", "AOD003"))
                            crops <- read_csv_columns("Data/Census/nhgis0138_csv/nhgis0138_ds22_1880_county.csv",
                                                      c("AOH007", "AOH011"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              left_join(crops, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = AOB001,
                                enslaved = 0,
                                farmv_total = AOD001 + AOD002 + AOD003,
                                improved = AOS001,
                                unimproved = AOS002,
                                land = improved + unimproved,
                                farmv = farmv_total / land,
                                pc_enslaved = 0,
                                cotton = AOH011,
                                corn = AOH007
                              )
                          },
                          "1890" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds27_1890_county.csv",
                                                    c("AUM001"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds26_1890_county.csv",
                                                     c("ASW001", "AUJ001", "AUJ002", "AUK001", "AUK002", "AUK003"))
                            crops <- read_csv_columns("Data/Census/nhgis0138_csv/nhgis0138_ds26_1890_county.csv",
                                                      c("ATB003", "ATB007"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              left_join(crops, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = ASW001,
                                enslaved = 0,
                                farmv_total = AUK001 + AUK002 + AUK003,
                                improved = AUJ001,
                                unimproved = AUJ002,
                                land = improved + unimproved,
                                farmv = farmv_total / land,
                                pc_enslaved = 0,
                                cotton = ATB007 * 477,
                                corn = ATB003
                              )
                          },
                          "1900" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds31_1900_county.csv",
                                                    c("AYM001"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds30_1900_county.csv",
                                                     c("AWS001", "AWT001", "AWU001", "AWW001", "AWW002", "AWW003", "AWW004"))
                            crops <- read_csv_columns("Data/Census/nhgis0138_csv/nhgis0138_ds30_1900_county.csv",
                                                      c("AXO003", "AXO024", "AXO026"))
                            pop %>%
                              left_join(farm, by = "GISJOIN") %>%
                              left_join(crops, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = AWS001,
                                enslaved = 0,
                                farmv_total = AWW001 + AWW002 + AWW003 + AWW004,
                                improved = AWU001,
                                unimproved = AWT001 - AWU001,
                                land = improved + unimproved,
                                farmv = farmv_total / land,
                                pc_enslaved = 0,
                                cotton = (AXO024 + AXO026) * 500,
                                corn = AXO003
                              )
                          }
    )
    if (is.null(census_data)) {
      stop(paste("No data returned for year", year))
    }
    return(census_data)
  }, error = function(e) {
    cat("Error in get_census_data function for year:", year, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Function to save data for a specific year
save_year_data <- function(data, year) {
  filename <- paste0("Event_study/", year, "_raw.csv")
  write_csv(data, filename)
  cat("Data for year", year, "has been saved to", filename, "\n")
}

# Modify the process_year function
process_year <- function(year) {
  cat("Processing year:", year, "\n")
  
  tryCatch({
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
    
    if (is.null(census_data)) {
      stop(paste("Census data is NULL for year", year))
    }
    
    # Print debugging information
    cat("Counties data dimensions:", dim(counties), "\n")
    cat("Census data dimensions:", dim(census_data), "\n")
    
    # Join county data with census data
    counties_df <- counties %>%
      st_drop_geometry() %>%
      left_join(census_data, by = "GISJOIN")
    
    # Add year column
    counties_df$year <- as.integer(year)
    
    # Calculate national average farmv for the year
    national_avg_farmv <- counties_df %>%
      filter(!(state %in% c("Alaska Territory", "Hawaii Territory"))) %>%
      summarize(total_farmv = sum(farmv_total, na.rm = TRUE),
                total_land = sum(improved + unimproved, na.rm = TRUE),
                national_avg_farmv = total_farmv / total_land) %>%
      pull(national_avg_farmv)
    
    # Calculate farmv_na
    counties_df <- counties_df %>%
      mutate(farmv_na = farmv / national_avg_farmv * 100)
    
    # Adjust cotton calculation for 1880
    if (year == "1880") {
      counties_df <- counties_df %>%
        mutate(cotton = ifelse(state %in% c("Texas", "Arkansas", "Missouri"), 
                               cotton * 500, 
                               cotton * 475))
    }
    
    # Select only the requested variables
    counties_df <- counties_df %>%
      select(GISJOIN, year, state, census_pop, enslaved, farmv_total, improved, unimproved, land, farmv, pc_enslaved, farmv_na, cotton, corn)
    
    # Save data for this year
    save_year_data(counties_df, year)
    
    return(counties_df)
  }, error = function(e) {
    cat("Error in process_year function for year:", year, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# List of years to process
years <- c("1850", "1860", "1870", "1880", "1890", "1900")

# Process all years
all_counties <- map_df(years, process_year)

# Write the result to a CSV file
write_csv(all_counties, "Event_study/Event_study_database.csv")

# Print a summary of the processed data
cat("\nProcessing complete. Summary of the data:\n")
cat("Total number of rows:", nrow(all_counties), "\n")
cat("Columns:", paste(colnames(all_counties), collapse=", "), "\n")
cat("Data for each year:\n")
print(table(all_counties$year))
cat("\nSummary of farmv_na:\n")
print(summary(all_counties$farmv_na))
cat("\nCombined data has been written to 'Event_study/Event_study_database.csv'\n")
cat("Individual year data has been saved as '[YEAR]_raw.csv' in the Event_study directory\n")