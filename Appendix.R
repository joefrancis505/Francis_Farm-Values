# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "dplyr", "tidyr", "readr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Create Appendix directory if it doesn't exist
if (!dir.exists("RDD/Appendix")) {
  dir.create("RDD/Appendix", recursive = TRUE)
  cat("Created directory: RDD/Appendix\n")
}

# Function to load, fix, and check shapefiles
load_and_fix_shapefile <- function(path, name) {
  tryCatch({
    sf <- st_read(path, quiet = TRUE)
    sf <- st_make_valid(sf)
    sf <- st_cast(sf, "MULTILINESTRING")
    return(sf)
  }, error = function(e) {
    stop("Error loading or fixing ", name, " shapefile: ", conditionMessage(e))
  })
}

# Function to generate points along the border
generate_border_points <- function(border, n = 20) {
  if (st_geometry_type(border, by_geometry = FALSE) == "MULTILINESTRING") {
    border_line <- st_cast(border, "LINESTRING")
  } else if (st_geometry_type(border, by_geometry = FALSE) == "LINESTRING") {
    border_line <- border
  } else {
    stop("Border geometry is neither LINESTRING nor MULTILINESTRING")
  }
  
  border_points <- st_line_sample(border_line, n = n)
  border_points <- st_cast(border_points, "POINT")
  st_sf(point_id = 1:n, geometry = border_points)
}

# Function to calculate distance to nearest border point
calculate_border_distance <- function(counties, border) {
  # Ensure counties are points (centroids)
  if (st_geometry_type(counties, by_geometry = FALSE) != "POINT") {
    counties <- st_centroid(counties)
  }
  
  # Ensure border is a LINESTRING or MULTILINESTRING
  if (!st_geometry_type(border, by_geometry = FALSE) %in% c("LINESTRING", "MULTILINESTRING")) {
    stop("Border must be a LINESTRING or MULTILINESTRING")
  }
  
  # Calculate distances
  distances <- st_distance(counties, border, by_element = FALSE)
  
  # Return the minimum distance for each county
  apply(distances, 1, min)
}

# Function to assign counties to border points
assign_counties_to_points <- function(data_year_sf, border_points) {
  # Calculate distance from each county to each border point
  county_points <- st_geometry(data_year_sf)
  border_point_geoms <- st_geometry(border_points)
  
  # Find closest border point for each county
  distances_to_points <- st_distance(county_points, border_point_geoms)
  closest_point <- apply(distances_to_points, 1, which.min)
  
  # Add closest point info to data
  data_year_sf$closest_point <- closest_point
  
  return(data_year_sf)
}

# Function to find the county shapefile for a specific year
find_county_shapefile <- function(year, ipums_dir = NULL) {
  year_str <- as.character(year)
  
  # Only check in the Shapefiles directory
  shapefile_dir <- paste0("Data/Shapefiles/", year_str)
  if (!dir.exists(shapefile_dir)) {
    stop(paste("Shapefile directory not found:", shapefile_dir))
  }
  
  shp_files <- list.files(shapefile_dir, pattern = "\\.shp$", 
                          full.names = TRUE, recursive = FALSE)
  if (length(shp_files) == 0) {
    stop(paste("No shapefile found in directory:", shapefile_dir))
  }
  
  cat("Using shapefile:", shp_files[1], "from Shapefiles directory\n")
  return(shp_files[1])
}

# Check if border files exist, if not search for them
find_border_file <- function(default_path, search_pattern) {
  if (file.exists(default_path)) {
    return(default_path)
  }
  
  # Search for the file using pattern
  border_files <- list.files("Data", pattern = search_pattern, 
                             full.names = TRUE, recursive = TRUE)
  
  if (length(border_files) > 0) {
    cat("Found alternative border file:", border_files[1], "\n")
    return(border_files[1])
  }
  
  stop(paste("Border file not found at", default_path, "and no alternatives found"))
}

# Main function to create appendix data (simplified version)
create_appendix_data <- function(year = 1860, ipums_dir = "Data/IPUMS") {
  
  cat("Creating appendix data for year", year, "\n")
  
  # Load border data
  default_border_file <- "Data/Border/1820_border/1820_border.shp"
  border_file <- find_border_file(default_border_file, "1820.*border.*\\.shp$")
  
  border <- load_and_fix_shapefile(border_file, paste("Border", year))
  border <- st_cast(border, "LINESTRING")
  
  # Generate 20 border points
  border_points <- generate_border_points(border, n = 20)
  
  # Add parity dummy variable to border points (1 for odd, 0 for even)
  border_points$point_parity <- border_points$point_id %% 2
  
  # Save border points geopackage
  border_output_path <- paste0("RDD/Appendix/", year, "_border_points.gpkg")
  st_write(border_points, border_output_path, delete_layer = TRUE)
  cat("Saved border points to", border_output_path, "\n")
  
  # Load data
  rdd_database_path <- "RDD/RDD_database.csv"
  if (!file.exists(rdd_database_path)) {
    stop("RDD database not found at ", rdd_database_path)
  }
  
  data <- read_csv(rdd_database_path, show_col_types = FALSE)
  
  data_year <- data %>%
    filter(year == !!year) %>%
    filter(!(state %in% c("Kansas Territory", "Nebraska Territory", "Indian Territory", "Unorganized Territory", "Kansas", "Nebraska"))) %>%
    mutate(treatment = slavery_legal)
  
  # Find county shapefile
  county_shapefile <- find_county_shapefile(year, ipums_dir)
  
  # Create centroids with all data
  counties_sf <- st_read(county_shapefile, quiet = TRUE) %>%
    st_transform(st_crs(border))
  
  # Handle different column naming in IPUMS shapefiles
  if ("STATENAM" %in% names(counties_sf) && !"state" %in% names(counties_sf)) {
    counties_sf <- counties_sf %>% rename(state = STATENAM)
  }
  
  data_year_sf <- counties_sf %>%
    st_centroid() %>%
    left_join(data_year, by = "GISJOIN") %>%
    st_as_sf() %>%
    mutate(log_farmv = ifelse(farmv > 0, log(farmv), NA)) %>%
    filter(!is.na(log_farmv) & is.finite(log_farmv))
  
  # Calculate distances and assign counties to border points
  data_year_sf$border_dist <- calculate_border_distance(data_year_sf, border)
  data_year_sf <- assign_counties_to_points(data_year_sf, border_points)
  
  # Add dummy variable based on closest_point % 2 (1 for odd points, 0 for even points)
  data_year_sf$point_parity <- data_year_sf$closest_point %% 2
  
  # Create the appendix data with proper column selection
  available_cols <- names(data_year_sf)
  cat("Available columns:", paste(available_cols, collapse = ", "), "\n")
  
  # Select core columns that definitely exist
  core_cols <- c("GISJOIN", "year", "treatment", "slavery_legal", "farmv", "log_farmv",
                 "border_dist", "closest_point", "point_parity")
  
  # Add state column if it exists
  if ("state" %in% available_cols) {
    core_cols <- c(core_cols, "state")
  } else if ("STATENAM" %in% available_cols) {
    core_cols <- c(core_cols, "STATENAM")
    data_year_sf <- data_year_sf %>% rename(state = STATENAM)
    core_cols[core_cols == "STATENAM"] <- "state"
  }
  
  # Add county column if it exists
  county_col_candidates <- c("county", "COUNTY", "COUNTYNAM", "NAME")
  county_col <- county_col_candidates[county_col_candidates %in% available_cols][1]
  if (!is.na(county_col)) {
    core_cols <- c(core_cols, county_col)
    if (county_col != "county") {
      data_year_sf <- data_year_sf %>% rename(county = all_of(county_col))
      core_cols[core_cols == county_col] <- "county"
    }
  }
  
  # Select only available columns
  existing_cols <- core_cols[core_cols %in% names(data_year_sf)]
  
  appendix_data <- data_year_sf %>%
    select(all_of(existing_cols), geometry)
  
  # Save appendix geopackage with all county centroids
  output_path <- paste0("RDD/Appendix/", year, "_county_centroids_with_assignments.gpkg")
  st_write(appendix_data, output_path, delete_layer = TRUE)
  cat("Saved appendix data to", output_path, "\n")
  
  return(appendix_data)
}

# Function to run the simplified appendix analysis
run_appendix_analysis <- function(year = 1860) {
  cat("=== SIMPLIFIED APPENDIX ANALYSIS ===\n")
  cat("Creating county centroids with border point assignments for year", year, "\n\n")
  
  result <- create_appendix_data(year)
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Files created:\n")
  cat("1.", paste0("RDD/Appendix/", year, "_border_points.gpkg\n"))
  cat("2.", paste0("RDD/Appendix/", year, "_county_centroids_with_assignments.gpkg\n"))
  
  cat("\nThe border points geopackage contains:\n")
  cat("- 20 border points as point geometries\n")
  cat("- Point IDs (point_id: 1-20)\n")
  cat("- Point parity dummy (point_parity: 1 for odd points, 0 for even points)\n")
  
  cat("\nThe county centroids geopackage contains:\n")
  cat("- County centroids as point geometries\n")
  cat("- GISJOIN identifiers\n")
  cat("- Border point assignments (closest_point column)\n")
  cat("- Point parity dummy (point_parity: 1 for odd points, 0 for even points)\n")
  cat("- Distance to border (border_dist column)\n")
  cat("- Treatment variables and other county data\n")
  
  return(result)
}

# Main execution: Run the simplified analysis for 1860
run_appendix_analysis(1860)