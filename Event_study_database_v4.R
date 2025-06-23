# Event_study_database.R - Final Replication Version
# Creates database for event study analysis

setwd(getSrcDirectory(function(dummy) {dummy}))
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load packages
packages <- c("sf", "dplyr", "readr")
install.packages(packages[!packages %in% rownames(installed.packages())])
invisible(lapply(packages, library, character.only = TRUE))

# Create output directory
dir.create("Event_study", showWarnings = FALSE)

# Process data for a single year
process_year <- function(year) {
  # Read shapefile
  shp_dir <- paste0("Data/Shapefiles/", year)
  shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)
  if (length(shp_files) == 0) stop(paste("No shapefile found for year", year))
  
  counties <- st_read(shp_files[1], quiet = TRUE) %>%
    select(GISJOIN, STATENAM) %>%
    rename(state = STATENAM)
  
  # Read census data
  census_data <- read_csv("Data/census.csv", show_col_types = FALSE) %>%
    filter(year == as.integer(year))
  
  # Process based on year
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
  
  # Join county data with census data
  counties_df <- counties %>%
    st_drop_geometry() %>%
    left_join(processed_data, by = "GISJOIN") %>%
    mutate(year = as.integer(year))
  
  # Calculate national average farmv for the year
  national_avg_farmv <- counties_df %>%
    filter(!(state %in% c("Alaska Territory", "Hawaii Territory"))) %>%
    summarize(total_farmv = sum(farmv_total, na.rm = TRUE),
              total_land = sum(land, na.rm = TRUE),
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
  
  return(counties_df)
}

# Process all years
years <- c("1850", "1860", "1870", "1880", "1890", "1900")
all_counties <- map_dfr(years, process_year)

# Save database
write_csv(all_counties, "Event_study/Event_study_database.csv")