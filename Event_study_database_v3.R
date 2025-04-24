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
required_dirs <- c("Event_study")
for (dir_path in required_dirs) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

# Function to save data for a specific year
save_year_data <- function(data, year) {
  filename <- paste0("Event_study/", year, "_raw.csv")
  write_csv(data, filename)
  cat("Data for year", year, "has been saved to", filename, "\n")
}

# Process data for a single year
process_year <- function(year) {
  cat("Processing year:", year, "\n")
  
  tryCatch({
    # Find and read shapefile
    shp_dir <- paste0("Data/Shapefiles/", year)
    shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_files) == 0) {
      stop(paste("No shapefile found for year", year, "in", shp_dir))
    }
    
    # Read county shapefile
    shapefile_path <- shp_files[1]
    cat("Using shapefile:", shapefile_path, "\n")
    
    counties <- st_read(shapefile_path, quiet = TRUE)
    
    # Keep only necessary columns
    counties <- counties %>%
      select(GISJOIN, STATENAM) %>%
      rename(state = STATENAM)
    
    # Read census data from central file
    census_data <- read_csv("Data/census.csv", show_col_types = FALSE) %>%
      filter(year == as.integer(year))
    
    # Process based on year
    processed_data <- NULL
    if (year == "1850") {
      processed_data <- census_data %>%
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
    } else if (year == "1860") {
      processed_data <- census_data %>%
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
    } else if (year == "1870") {
      processed_data <- census_data %>%
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
    } else if (year == "1880") {
      processed_data <- census_data %>%
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
    } else if (year == "1890") {
      processed_data <- census_data %>%
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
    } else if (year == "1900") {
      processed_data <- census_data %>%
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
    
    if (is.null(processed_data)) {
      stop(paste("Census data is NULL for year", year))
    }
    
    # Join county data with census data
    counties_df <- counties %>%
      st_drop_geometry() %>%
      left_join(processed_data, by = "GISJOIN")
    
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

# --- Main Function ---
main <- function() {
  cat("=== Starting Data Processing ===\n")
  
  # Define years
  years <- c("1850", "1860", "1870", "1880", "1890", "1900")
  
  # Process all years
  all_counties_list <- list()
  for (year in years) {
    counties_df <- process_year(year)
    if (!is.null(counties_df)) {
      all_counties_list[[year]] <- counties_df
    }
  }
  
  # Check if any years were processed
  if (length(all_counties_list) == 0) {
    stop("Processing failed for all years. No data to combine.")
  }
  
  # Combine all processed years
  all_counties <- bind_rows(all_counties_list)
  
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
}

# Run main function
main()