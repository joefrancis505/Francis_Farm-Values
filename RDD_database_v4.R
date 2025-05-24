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

# Create necessary directories for output
required_dirs <- c("RDD")
for (dir_path in required_dirs) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

# Function to fix geometries in shapefiles
fix_geometries <- function(sf_object) {
  sf_object %>%
    st_make_valid() %>%
    st_buffer(0)
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

# Function to read geology/soil data
read_geology_data <- function(year, type) {
  file_path <- paste0("Data/Geology/USGS_", type, "/", type, "_", year, ".csv")
  
  tryCatch({
    read_csv(file_path, show_col_types = FALSE) %>%
      select(GISJOIN, contains(paste0(type, "_mean")))
  }, error = function(e) {
    warning(paste("Error reading geology file:", file_path, "\nError:", e$message))
    data.frame(GISJOIN = character(0), mean = numeric(0)) %>% 
      setNames(c("GISJOIN", paste0(type, "_mean")))
  })
}

# Function to read pH data
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

# Process_year function
process_year <- function(year) {
  cat(sprintf("\n--- Processing data for year: %s ---\n", year))
  
  # Find and read shapefile
  shp_dir <- paste0("Data/Shapefiles/", year)
  shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)
  
  if (length(shp_files) == 0) {
    stop(sprintf("No shapefile found for year %s in %s.", year, shp_dir))
  }
  
  shapefile_path <- shp_files[1]
  cat("Using shapefile:", shapefile_path, "\n")
  
  counties <- st_read(shapefile_path, quiet = TRUE) %>%
    fix_geometries()
  
  border_file <- file.path("Data", "Border", paste0("border_", year, ".csv"))
  
  if (file.exists(border_file)) {
    border_data_for_join <- read_csv(
      border_file, 
      show_col_types = FALSE, 
      col_select = c("GISJOIN", "border") 
    ) %>%
      mutate(GISJOIN = as.character(GISJOIN)) %>%
      distinct(GISJOIN, .keep_all = TRUE) # Ensure border data is unique per GISJOIN
    
    counties <- counties %>%
      left_join(border_data_for_join, by = "GISJOIN") %>%
      mutate(border_county = ifelse(!is.na(border) & border == 1, 1, 0))
  } else {
    # If border file doesn't exist, ensure the column is still created to avoid errors later
    counties <- counties %>%
      mutate(border_county = 0) # Or NA_integer_ if preferred, but 0 is consistent with ifelse logic
  }
  
  counties <- counties %>%
    rename(
      state = STATENAM
      # year column assignment is handled later and more robustly
    ) 
  
  census_data <- read_csv("Data/census.csv", 
                          show_col_types = FALSE,
                          # Ensure GISJOIN is read as character if it's not always like "G..."
                          # col_types = cols(GISJOIN = col_character(), .default = col_guess())
  ) %>%
    filter(year == as.integer(!!year)) # Use !! to unquote the 'year' argument if needed in this context, or just year
  
  # Temporary variable for processed census data
  processed_data_temp <- NULL
  
  if (year == "1850") {
    processed_data_temp <- census_data %>%
      transmute(
        GISJOIN,
        census_pop = ADQ001, 
        black = AE6002 + AE6003, 
        enslaved = AE6003,
        farmv_total = ADJ001, 
        land = ADI001 + ADI002,
        farmv = ifelse(land > 0, farmv_total / land, NA_real_),
        pc_black = ifelse(census_pop > 0, black / census_pop * 100, NA_real_),
        pc_enslaved = ifelse(census_pop > 0, enslaved / census_pop * 100, NA_real_)
      )
  } else if (year == "1860") {
    processed_data_temp <- census_data %>%
      transmute(
        GISJOIN,
        census_pop = AG3001, 
        black = AH3002 + AH3003 + AH3005, 
        enslaved = AH3003,
        farmv_total = AGV001, 
        land = AGP001 + AGP002,
        farmv = ifelse(land > 0, farmv_total / land, NA_real_),
        pc_black = ifelse(census_pop > 0, black / census_pop * 100, NA_real_),
        pc_enslaved = ifelse(census_pop > 0, enslaved / census_pop * 100, NA_real_)
      )
  } else if (year == "1870") {
    processed_data_temp <- census_data %>%
      transmute(
        GISJOIN,
        census_pop = AJR001,
        black = AK3002,
        enslaved = 0, # Explicitly 0
        farmv_total = AJV001, 
        land = AJU001 + AJU002 + AJU003,
        farmv = ifelse(land > 0, farmv_total / land, NA_real_),
        pc_black = ifelse(census_pop > 0, black / census_pop * 100, NA_real_),
        pc_enslaved = 0 # Explicitly 0
      )
  } else if (year == "1880") {
    processed_data_temp <- census_data %>%
      transmute(
        GISJOIN,
        census_pop = AOB001,
        black = APP002,
        enslaved = 0, # Explicitly 0
        farmv_total = AOD001 + AOD002 + AOD003, 
        land = AOS001 + AOS002,
        farmv = ifelse(land > 0, farmv_total / land, NA_real_),
        pc_black = ifelse(census_pop > 0, black / census_pop * 100, NA_real_),
        pc_enslaved = 0 # Explicitly 0
      )
  } else if (year == "1890") {
    processed_data_temp <- census_data %>%
      transmute(
        GISJOIN,
        census_pop = ASW001,
        black = AV0007 + AV0008,
        enslaved = 0, # Explicitly 0
        farmv_total = AUK001 + AUK002 + AUK003, 
        land = AUJ001 + AUJ002,
        farmv = ifelse(land > 0, farmv_total / land, NA_real_),
        pc_black = ifelse(census_pop > 0, black / census_pop * 100, NA_real_),
        pc_enslaved = 0 # Explicitly 0
      )
  } else if (year == "1900") {
    processed_data_temp <- census_data %>%
      transmute(
        GISJOIN,
        census_pop = AWS001,
        black = AZ3003 + AZ3004,
        enslaved = 0, # Explicitly 0
        farmv_total = AWW001 + AWW002 + AWW003 + AWW004, 
        land = AWT001,
        farmv = ifelse(land > 0, farmv_total / land, NA_real_),
        pc_black = ifelse(census_pop > 0, black / census_pop * 100, NA_real_),
        pc_enslaved = 0 # Explicitly 0
      )
  }
  
  if (is.null(processed_data_temp)) {
    stop(paste("Could not process census data for year", year))
  }
  
  # De-duplicate the processed census data, keeping the row with the most complete information
  processed_data <- processed_data_temp %>%
    # Ensure GISJOIN is character for consistent joins
    mutate(GISJOIN = as.character(GISJOIN)) %>% 
    group_by(GISJOIN) %>%
    # Arrange by non-NA status of key columns. Rows with data (FALSE for is.na) come first.
    arrange(is.na(census_pop), is.na(farmv), is.na(pc_black)) %>%
    slice(1) %>% # Take the first row for each GISJOIN (the "best" one)
    ungroup()
  
  # Read slope and elevation data (consider adding distinct(GISJOIN, .keep_all = TRUE) here too)
  slope_data <- read_geology_data(year, "slope") %>% 
    mutate(GISJOIN = as.character(GISJOIN)) %>%
    distinct(GISJOIN, .keep_all = TRUE)
  elevation_data <- read_geology_data(year, "elevation") %>%
    mutate(GISJOIN = as.character(GISJOIN)) %>%
    distinct(GISJOIN, .keep_all = TRUE)
  
  # Read pH data (consider adding distinct(GISJOIN, .keep_all = TRUE) here too)
  ph_data <- read_ph_data(year) %>%
    mutate(GISJOIN = as.character(GISJOIN)) %>%
    distinct(GISJOIN, .keep_all = TRUE)
  
  # Join county data with census data and geology data
  counties_df <- counties %>%
    # Ensure GISJOIN in counties sf is character for consistent joins
    mutate(GISJOIN = as.character(GISJOIN)) %>%
    left_join(processed_data, by = "GISJOIN") %>%
    left_join(slope_data, by = "GISJOIN") %>%
    left_join(elevation_data, by = "GISJOIN") %>%
    left_join(ph_data, by = "GISJOIN") %>%
    rename(slope = slope_mean, elevation = elevation_mean, ph = ph_mean)
  
  counties_df <- add_slavery_legality(counties_df)
  
  # Add the year column using the function's year argument
  counties_df <- counties_df %>%
    mutate(year = as.integer(!!year)) # Assign the correct year
  
  # Select only the required variables
  # Ensure border_county is selected if it was created
  required_cols <- c("year", "state", "slavery_legal", "GISJOIN", "farmv", 
                     "slope", "elevation", "ph", "pc_enslaved", "pc_black")
  if ("border_county" %in% names(counties_df)) {
    required_cols <- c(required_cols, "border_county")
  } else {
    # If border_county was not added (e.g. file never exists for any year), add it as NA or 0
    counties_df <- counties_df %>% mutate(border_county = 0) # Or NA_integer_
    required_cols <- c(required_cols, "border_county")
  }
  
  counties_df <- counties_df %>%
    select(all_of(required_cols))
  
  return(counties_df)
}

# Main function
main <- function() {
  cat("=== Starting Data Processing ===\n")
  
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
}

# Run main function
main()