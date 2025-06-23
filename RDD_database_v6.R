setwd(getSrcDirectory(function(dummy) {dummy}))
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load packages
packages <- c("sf", "dplyr", "readr")
install.packages(packages[!packages %in% rownames(installed.packages())])
invisible(lapply(packages, library, character.only = TRUE))

# Create output directory
dir.create("RDD", showWarnings = FALSE)

# Add slavery legality based on state
add_slavery_legality <- function(df) {
  slave_states <- c(
    "Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia",
    "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri",
    "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"
  )
  
  df %>%
    mutate(slavery_legal = as.integer(state %in% slave_states))
}

# Process data for a specific year
process_year <- function(year) {
  # Read shapefile
  shp_dir <- paste0("Data/Shapefiles/", year)
  shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)
  if (length(shp_files) == 0) stop(paste("No shapefile found for year", year))
  
  counties <- st_read(shp_files[1], quiet = TRUE) %>%
    st_make_valid() %>%
    rename(state = STATENAM) %>%
    mutate(GISJOIN = as.character(GISJOIN))
  
  # Read border data if available
  border_file <- file.path("Data", "Border", paste0("border_", year, ".csv"))
  if (file.exists(border_file)) {
    border_data <- read_csv(border_file, show_col_types = FALSE) %>%
      mutate(GISJOIN = as.character(GISJOIN)) %>%
      distinct(GISJOIN, .keep_all = TRUE)
    
    counties <- counties %>%
      left_join(border_data %>% select(GISJOIN, border), by = "GISJOIN") %>%
      mutate(border_county = ifelse(!is.na(border) & border == 1, 1, 0))
  } else {
    counties <- counties %>% mutate(border_county = 0)
  }
  
  # Read and process census data
  census_data <- read_csv("Data/census.csv", show_col_types = FALSE) %>%
    filter(year == as.integer(!!year))
  
  if (year == "1850") {
    processed_data <- census_data %>%
      transmute(
        GISJOIN = as.character(GISJOIN),
        farmv_total = ADJ001, 
        land = ADI001 + ADI002,
        farmv = ifelse(land > 0, farmv_total / land, NA_real_)
      )
  } else if (year == "1860") {
    processed_data <- census_data %>%
      transmute(
        GISJOIN = as.character(GISJOIN),
        farmv_total = AGV001, 
        land = AGP001 + AGP002,
        farmv = ifelse(land > 0, farmv_total / land, NA_real_)
      )
  }
  
  # Join and finalize
  counties_df <- counties %>%
    left_join(processed_data, by = "GISJOIN") %>%
    add_slavery_legality() %>%
    mutate(year = as.integer(year)) %>%
    select(year, state, slavery_legal, GISJOIN, farmv, border_county) %>%
    st_drop_geometry()
  
  return(counties_df)
}

# Process years needed for RDD (1850 and 1860 only)
years <- c("1850", "1860")
all_counties <- map_dfr(years, process_year)

# Save database
write_csv(all_counties, "RDD/RDD_database.csv")