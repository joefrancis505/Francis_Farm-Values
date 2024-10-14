# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "stars", "fasterize", "dplyr", "purrr", "readr", "exactextractr", "terra")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Clear console
cat("\014")

# Define the years to process
years <- seq(1850, 1900, by = 10)

# Function to fix geometries in shapefiles
fix_geometries <- function(sf_object) {
  sf_object %>%
    st_make_valid() %>%
    st_buffer(0)
}

# Function to join shapefile with census data
join_data <- function(sf_object, census_data) {
  sf_object %>%
    left_join(census_data, by = "GISJOIN")
}

# Function to calculate densities
calculate_densities <- function(sf_object) {
  sf_object %>%
    mutate(across(c(farmv_total, improved, unimproved, enslaved, census_pop), 
                  ~ . / SHAPE_AREA, 
                  .names = "{.col}_density"))
}

# Rasterize_data function
rasterize_data <- function(sf_object, resolution = 100) {
  library(terra)
  
  bbox <- st_bbox(sf_object)
  
  # Extract CRS as a proj4string
  crs_proj4 <- st_crs(sf_object)$proj4string
  
  # Create raster template
  raster_template <- rast(xmin=bbox["xmin"], xmax=bbox["xmax"], 
                          ymin=bbox["ymin"], ymax=bbox["ymax"], 
                          resolution=resolution)
  
  # Set CRS for the raster template using proj4string
  crs(raster_template) <- crs_proj4
  
  variables <- c("farmv_total_density", "improved_density", "unimproved_density", 
                 "enslaved_density", "census_pop_density")
  
  rasterize_variable <- function(variable) {
    if (variable %in% names(sf_object)) {
      cat(sprintf("Rasterizing %s...\n", variable))
      sf_object[[variable]] <- as.numeric(sf_object[[variable]])
      r <- terra::rasterize(vect(sf_object), raster_template, field = variable, fun = "sum")
      return(r)
    } else {
      cat(sprintf("Skipping %s (not available)...\n", variable))
      return(NULL)
    }
  }
  
  raster_list <- lapply(variables, rasterize_variable)
  raster_list <- raster_list[!sapply(raster_list, is.null)]
  
  if (length(raster_list) > 0) {
    raster_stack <- do.call(c, raster_list)
    names(raster_stack) <- variables[sapply(raster_list, function(x) !is.null(x))]
  } else {
    stop("No variables were successfully rasterized.")
  }
  
  return(raster_stack)
}

# Zonal_stats function
zonal_stats <- function(raster_stack, zones_sf) {
  library(exactextractr)
  
  cat("Performing zonal statistics with exactextractr...\n")
  stats <- exact_extract(raster_stack, zones_sf, fun = "mean", append_cols = c("GISJOIN", "STATENAM"), progress = TRUE)
  
  return(stats)
}

convert_to_absolute <- function(zonal_stats_df, zones_sf) {
  zones_sf %>%
    left_join(zonal_stats_df, by = c("GISJOIN", "STATENAM")) %>%
    mutate(
      farmv_total = mean.farmv_total_density * SHAPE_AREA,
      improved = mean.improved_density * SHAPE_AREA,
      unimproved = mean.unimproved_density * SHAPE_AREA,
      enslaved = mean.enslaved_density * SHAPE_AREA,
      census_pop = mean.census_pop_density * SHAPE_AREA,
      state = STATENAM
    ) %>%
    select(-starts_with("mean."), -STATENAM)
}

# Calculate_derived_variables function
calculate_derived_variables <- function(df) {
  df %>%
    mutate(
      area = SHAPE_AREA / 2589988.11, # Convert square meters to square miles
      farmv = ifelse(improved + unimproved > 0, farmv_total / (improved + unimproved), NA),
      pc_enslaved = ifelse(census_pop > 0, (enslaved / census_pop) * 100, NA)
    )
}

# Process_year function
process_year <- function(year, census_data, zones_1900) {
  cat(sprintf("\nProcessing year: %s\n", year))
  
  if (year != "1900") {
    # Existing code for other years
    cat("Reading county shapefile...\n")
    counties <- st_read(paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp"), quiet = TRUE) %>%
      fix_geometries()
    
    cat("Joining with census data...\n")
    counties_with_data <- join_data(counties, census_data)
    
    cat("Filtering counties...\n")
    counties_filtered <- counties_with_data %>%
      filter(census_pop > 0)
    
    cat("Calculating densities...\n")
    counties_densities <- calculate_densities(counties_filtered)
    
    cat("Rasterizing data...\n")
    raster_data <- rasterize_data(counties_densities)
    
    cat("Performing zonal statistics...\n")
    zonal_results <- zonal_stats(raster_data, zones_1900)
    
    # After zonal statistics and converting to absolute values
    cat("Converting to absolute values...\n")
    final_results <- convert_to_absolute(zonal_results, zones_1900)
    
    # Ensure 'state' column is correctly named
    if ("STATENAM" %in% names(final_results) && !"state" %in% names(final_results)) {
      final_results <- final_results %>% rename(state = STATENAM)
    }
    
  } else {
    cat("Processing 1900 data without rasterization...\n")
    final_results <- zones_1900 %>%
      left_join(census_data, by = "GISJOIN")
    
    # Ensure 'state' column is correctly named
    if ("STATENAM" %in% names(final_results) && !"state" %in% names(final_results)) {
      final_results <- final_results %>% rename(state = STATENAM)
    } else if ("state" %in% names(final_results) && "STATENAM" %in% names(final_results)) {
      final_results <- final_results %>% select(-STATENAM)
    }
  }
  
  cat("Calculating derived variables...\n")
  final_results <- calculate_derived_variables(final_results)

  # Remove geometry column
  cat("Removing geometry column...\n")
  final_results <- st_drop_geometry(final_results)
  
  return(final_results)
}

# Function to perform processing for all years
perform_processing <- function() {
  # Read the 1900 county boundaries
  cat("Reading 1900 county boundaries...\n")
  zones_1900 <- st_read("Data/Counties/1900_US_county/US_county_1900.shp", quiet = TRUE) %>%
    fix_geometries()
  
  # Read the census data
  cat("Reading census data...\n")
  census_data <- read_csv("DiD/DiD_database.csv", show_col_types = FALSE)
  
  # Process all years including 1900
  for (year in years) {
    cat(sprintf("\n\n--- Processing year %d ---\n", year))
    
    result <- process_year(as.character(year), filter(census_data, year == !!year), zones_1900)
    
    # Save result for this year with simplified file name
    output_file <- sprintf("DiD/%d_normalized.csv", year)
    cat(sprintf("\nWriting results for year %d to %s\n", year, output_file))
    write_csv(result, output_file)
    
    cat(sprintf("\nFinished processing year %d\n", year))
  }
}

compile_panel_data <- function() {
  cat("Compiling panel data from 1850 to 1900...\n")
  
  panel_data <- map_dfr(years, function(year) {
    cat(sprintf("Processing year %d...\n", year))
    file_name <- sprintf("DiD/%d_normalized.csv", year)
    
    data <- read_csv(file_name, show_col_types = FALSE)
    
    # Add year column
    data <- data %>% mutate(year = year)
    
    return(data)
  })
  
  # Ensure consistent column order across all years
  all_columns <- unique(c("year", unlist(map(years, function(year) {
    file_name <- sprintf("DiD/%d_normalized.csv", year)
    names(read_csv(file_name, show_col_types = FALSE))
  }))))
  
  # Reorder columns and fill missing columns with NA
  panel_data <- panel_data %>%
    select(all_of(all_columns))
  
  # Write panel data to CSV
  output_file <- "DiD/DiD_panel_data.csv"
  write_csv(panel_data, output_file)
  cat(sprintf("Panel data compiled successfully. Output file: %s\n", output_file))
}

# Main function
main <- function() {
  cat("Starting the processing...\n")
  
  # Perform processing
  perform_processing()
  
  # Compile panel data
  compile_panel_data()
  
  # Read the compiled panel data
  panel_data <- read_csv("DiD/DiD_panel_data.csv", show_col_types = FALSE)

  cat("All operations completed successfully.\n")
}

# Run the main function
main()