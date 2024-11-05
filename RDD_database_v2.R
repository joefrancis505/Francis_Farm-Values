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
    # Read the first few rows to get column names
    first_rows <- read_csv(file_path, n_max = 5)
    all_columns <- names(first_rows)
    
    # Create a named list of column types
    col_types <- cols()
    
    for (col in all_columns) {
      if (col %in% c("GISJOIN", "STATE", "COUNTY", "STATENAM", "NHGISNAM", "NHGISST", "NHGISCTY", "ICPSRNAM")) {
        col_types[[col]] <- col_character()
      } else {
        col_types[[col]] <- col_double()
      }
    }
    
    # Read the CSV file with specified column types
    df <- read_csv(file_path, col_types = col_types)
    
    if (!"GISJOIN" %in% names(df)) {
      stop("GISJOIN column not found in the CSV file")
    }
    
    missing_cols <- setdiff(columns, names(df))
    if (length(missing_cols) > 0) {
      stop(paste("The following columns are missing from the CSV file:", paste(missing_cols, collapse = ", ")))
    }
    
    result <- df[, c("GISJOIN", columns)]
    
    list(data = result, problems = problems(df))
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
    "Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia",
    "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri",
    "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"
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
                                                    c("ADQ001", "ADZ001", "AE6002", "AE6003"))
                            farm <- read_csv_columns("Data/Census/nhgis0128_csv/nhgis0128_ds9_1850_county.csv",
                                                     c("ADJ001", "ADI001", "ADI002"))
                            
                            # Check for problems in population data
                            if (nrow(pop$problems) > 0) {
                              cat("Problems found in 1850 population data:\n")
                              print(pop$problems)
                            }
                            
                            # Check for problems in farm data
                            if (nrow(farm$problems) > 0) {
                              cat("Problems found in 1850 farm data:\n")
                              print(farm$problems)
                            }
                            
                            pop$data %>%
                              left_join(farm$data, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = ADQ001,
                                black = AE6002 + AE6003,
                                enslaved = AE6003,
                                farmv_total = ADJ001,
                                land = ADI001 + ADI002,
                                farmv = farmv_total / land,
                                pc_black = black / census_pop * 100,
                                pc_enslaved = enslaved / census_pop * 100
                              )
                          },
                          "1860" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds14_1860_county.csv",
                                                    c("AG3001", "AH3002", "AH3003", "AH3005"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds13_1860_county.csv",
                                                     c("AGV001", "AGP001", "AGP002"))
                            
                            # Check for problems in population data
                            if (nrow(pop$problems) > 0) {
                              cat("Problems found in 1860 population data:\n")
                              print(pop$problems)
                            }
                            
                            # Check for problems in farm data
                            if (nrow(farm$problems) > 0) {
                              cat("Problems found in 1860 farm data:\n")
                              print(farm$problems)
                            }
                            
                            pop$data %>%
                              left_join(farm$data, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = AG3001,
                                black = AH3002 + AH3003 + AH3005,
                                enslaved = AH3003,
                                farmv_total = AGV001,
                                land = AGP001 + AGP002,
                                farmv = farmv_total / land,
                                pc_black = black / census_pop * 100,
                                pc_enslaved = enslaved / census_pop * 100
                              )
                          },
                          "1870" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds17_1870_county.csv",
                                                    c("AK3001", "AK3002", "AK3003", "AK3004", "AKE001"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds16_1870_county.csv",
                                                     c("AJR001", "AJU001", "AJU002", "AJU003", "AJV001"))
                            
                            # Check for problems in population data
                            if (nrow(pop$problems) > 0) {
                              cat("Problems found in 1870 population data:\n")
                              print(pop$problems)
                            }
                            
                            # Check for problems in farm data
                            if (nrow(farm$problems) > 0) {
                              cat("Problems found in 1870 farm data:\n")
                              print(farm$problems)
                            }
                            
                            pop$data %>%
                              left_join(farm$data, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = AJR001,
                                black = AK3002,
                                enslaved = 0,
                                farmv_total = AJV001,
                                land = AJU001 + AJU002 + AJU003,
                                farmv = farmv_total / land,
                                pc_black = black / census_pop * 100,
                                pc_enslaved = 0
                              )
                          },
                          "1880" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds23_1880_county.csv",
                                                    c("AO4001", "APP001", "APP002"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds22_1880_county.csv",
                                                     c("AOB001", "AOS001", "AOS002", "AOD001", "AOD002", "AOD003"))
                            
                            # Check for problems in population data
                            if (nrow(pop$problems) > 0) {
                              cat("Problems found in 1880 population data:\n")
                              print(pop$problems)
                            }
                            
                            # Check for problems in farm data
                            if (nrow(farm$problems) > 0) {
                              cat("Problems found in 1880 farm data:\n")
                              print(farm$problems)
                            }
                            
                            pop$data %>%
                              left_join(farm$data, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = AOB001,
                                black = APP002,
                                enslaved = 0,
                                farmv_total = AOD001 + AOD002 + AOD003,
                                land = AOS001 + AOS002,
                                farmv = farmv_total / land,
                                pc_black = black / census_pop * 100,
                                pc_enslaved = 0
                              )
                          },
                          "1890" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds27_1890_county.csv",
                                                    c("AUM001", "AUU001", "AV0007", "AV0008"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds26_1890_county.csv",
                                                     c("ASW001", "AUJ001", "AUJ002", "AUK001", "AUK002", "AUK003"))
                            
                            # Check for problems in population data
                            if (nrow(pop$problems) > 0) {
                              cat("Problems found in 1890 population data:\n")
                              print(pop$problems)
                            }
                            
                            # Check for problems in farm data
                            if (nrow(farm$problems) > 0) {
                              cat("Problems found in 1890 farm data:\n")
                              print(farm$problems)
                            }
                            
                            pop$data %>%
                              left_join(farm$data, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = ASW001,
                                black = AV0007 + AV0008,
                                enslaved = 0,
                                farmv_total = AUK001 + AUK002 + AUK003,
                                land = AUJ001 + AUJ002,
                                farmv = farmv_total / land,
                                pc_black = black / census_pop * 100,
                                pc_enslaved = 0
                              )
                          },
                          "1900" = {
                            pop <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds31_1900_county.csv",
                                                    c("AYM001", "AYT001", "AZ3003", "AZ3004"))
                            farm <- read_csv_columns("Data/Census/nhgis0130_csv/nhgis0130_ds30_1900_county.csv",
                                                     c("AWS001", "AWT001", "AWU001", "AWW001", "AWW002", "AWW003", "AWW004"))
                            
                            # Check for problems in population data
                            if (nrow(pop$problems) > 0) {
                              cat("Problems found in 1900 population data:\n")
                              print(pop$problems)
                            }
                            
                            # Check for problems in farm data
                            if (nrow(farm$problems) > 0) {
                              cat("Problems found in 1900 farm data:\n")
                              print(farm$problems)
                            }
                            
                            pop$data %>%
                              left_join(farm$data, by = "GISJOIN") %>%
                              transmute(
                                GISJOIN,
                                census_pop = AWS001,
                                black = AZ3003 + AZ3004,
                                enslaved = 0,
                                farmv_total = AWW001 + AWW002 + AWW003 + AWW004,
                                land = AWT001,
                                farmv = farmv_total / land,
                                pc_black = black / census_pop * 100,
                                pc_enslaved = 0
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

# Function to read elevation and slope data
read_geology_data <- function(year, type) {
  file_path <- paste0("Data/Geology/USGS_", type, "/", type, "_", year, ".csv")
  read_csv(file_path) %>%
    select(GISJOIN, paste0(type, "_mean"))
}

# Function to read ph data
read_ph_data <- function(year) {
  ph_0_5 <- read_csv(paste0("Data/Geology/Soilgrids/ph_0_5_", year, ".csv"), 
                     col_types = cols(GISJOIN = col_character(), `ph_0-5_mean` = col_double()))
  ph_5_15 <- read_csv(paste0("Data/Geology/Soilgrids/ph_5_15_", year, ".csv"), 
                      col_types = cols(GISJOIN = col_character(), `ph_5-15_mean` = col_double()))
  ph_15_30 <- read_csv(paste0("Data/Geology/Soilgrids/ph_15_30_", year, ".csv"), 
                       col_types = cols(GISJOIN = col_character(), `ph_15-30_mean` = col_double()))
  
  ph_data <- ph_0_5 %>%
    left_join(ph_5_15, by = "GISJOIN") %>%
    left_join(ph_15_30, by = "GISJOIN") %>%
    mutate(ph_mean = (`ph_0-5_mean` + `ph_5-15_mean` + `ph_15-30_mean`) / 3) %>%
    select(GISJOIN, ph_mean)
  
  return(ph_data)
}

# Function to fix geometries in shapefiles
fix_geometries <- function(sf_object) {
  sf_object %>%
    st_make_valid() %>%
    st_buffer(0)
}

# Process_year function
process_year <- function(year) {
  # Read county shapefile
  counties <- st_read(paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp")) %>%
    fix_geometries()
  
  # Rename columns from shapefiles and set border_county to 0 if border is not 1
  counties <- counties %>%
    rename(
      state = STATENAM,
      year = DECADE
    ) %>%
    mutate(border_county = case_when(
      is.na(border) ~ 0,
      border == 1 ~ 1,
      TRUE ~ 0
    ))
  
  # Get census data for the year
  census_data <- get_census_data(year)
  
  # Read slope and elevation data
  slope_data <- read_geology_data(year, "slope")
  elevation_data <- read_geology_data(year, "elevation")
  
  # Read pH data
  ph_data <- read_ph_data(year)
  
  # Join county data with census data and geology data
  counties_df <- counties %>%
    left_join(census_data, by = "GISJOIN") %>%
    left_join(slope_data, by = "GISJOIN") %>%
    left_join(elevation_data, by = "GISJOIN") %>%
    left_join(ph_data, by = "GISJOIN") %>%
    rename(slope = slope_mean, elevation = elevation_mean, ph = ph_mean)
  
  # Add slavery legality dummy variable
  counties_df <- add_slavery_legality(counties_df)
  
  # Select only the required variables
  counties_df <- counties_df %>%
    select(year, state, slavery_legal, GISJOIN, farmv, slope, elevation, ph, pc_enslaved, border_county, pc_black)
  
  return(counties_df)
}

# List of years to process
years <- c("1850", "1860", "1870", "1880", "1890", "1900")

# Process specified years
all_counties <- map(years, function(year) {
  cat("Processing year:", year, "\n")
  process_year(year)
})

# Combine all years and drop geometry
all_counties_df <- do.call(rbind, all_counties) %>%
  st_drop_geometry()

# Write the result to a CSV file
write_csv(all_counties_df, "RDD/RDD_database.csv")

# Print a summary of the processed data
cat("Processing complete. Summary of the data:\n")
cat("Total number of rows:", nrow(all_counties_df), "\n")
cat("Columns:", paste(colnames(all_counties_df), collapse=", "), "\n")
cat("Data for each year:\n")
print(table(all_counties_df$year))
cat("\nData has been written to 'RDD/RDD_database.csv'\n")