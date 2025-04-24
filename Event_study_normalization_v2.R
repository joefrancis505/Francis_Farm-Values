# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "stars", "fasterize", "dplyr", "purrr", "readr", "exactextractr", "terra", "ipumsr", "stringr", "tidyselect", "cli")

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
    mutate(across(c(farmv_total, improved, unimproved, enslaved, census_pop, cotton, corn), 
                  ~ . / SHAPE_AREA, 
                  .names = "{.col}_density"))
}

# Modified function to find shapefile for a specific year
find_shapefile <- function(year) {
  year_str <- as.character(year)
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

# Rasterize_data function
rasterize_data <- function(sf_object, resolution = 200) {
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
                 "enslaved_density", "census_pop_density", "cotton_density", "corn_density")
  
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
      cotton = mean.cotton_density * SHAPE_AREA,
      corn = mean.corn_density * SHAPE_AREA,
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
      pc_enslaved = ifelse(census_pop > 0, (enslaved / census_pop) * 100, NA),
      cotton_pc = ifelse(census_pop > 0, cotton / census_pop, NA),
      ccratio = ifelse(corn > 0, cotton / corn, NA)
    )
}

# Process raw data for each year
process_raw_data <- function() {
  cat("\n=== Processing Raw Data ===\n")
  
  # Define years to process
  years <- seq(1850, 1900, by = 10)
  year_strings <- as.character(years)
  
  # Read census data
  cat("Reading census data from Data/census.csv\n")
  census_data <- read_csv("Data/census.csv", show_col_types = FALSE)
  
  # Process all years
  for (year_str in year_strings) {
    cat(sprintf("\nProcessing raw data for year: %s\n", year_str))
    
    # First check if we need to create the raw data file
    raw_file <- paste0("Event_study/", year_str, "_raw.csv")
    
    if (!file.exists(raw_file)) {
      cat(sprintf("Raw data file %s not found. Creating from data...\n", raw_file))
      
      # Find shapefile in directory
      shapefile_path <- find_shapefile(year_str)
      cat("Using shapefile:", shapefile_path, "\n")
      
      # Read shapefile
      counties_sf <- tryCatch({
        sf_obj <- st_read(shapefile_path, quiet = TRUE)
        sf_obj <- fix_geometries(sf_obj)
        
        # Rename columns and ensure GISJOIN is character
        sf_obj %>%
          rename(state = STATENAM) %>%
          mutate(GISJOIN = as.character(GISJOIN))
      }, error = function(e) {
        warning(sprintf("Error reading shapefile for year %s: %s", year_str, e$message))
        return(NULL)
      })
      
      if (is.null(counties_sf)) {
        next
      }
      
      # Filter census data for this year
      year_census_data <- census_data %>% 
        filter(year == as.integer(year_str))
      
      # Process combined data based on year
      counties_df <- NULL
      
      if (year_str == "1850") {
        counties_df <- counties_sf %>%
          st_drop_geometry() %>%
          left_join(year_census_data, by = "GISJOIN") %>%
          transmute(
            GISJOIN,
            state,
            census_pop = ADQ001,
            enslaved = AE6003,
            farmv_total = ADJ001,
            improved = ADI001,
            unimproved = ADI002,
            land = improved + unimproved,
            farmv = ifelse(land > 0, farmv_total / land, NA_real_),
            pc_enslaved = ifelse(census_pop > 0, enslaved / census_pop * 100, NA_real_),
            cotton = ADM007 * 400,
            corn = ADM003
          )
      } 
      else if (year_str == "1860") {
        counties_df <- counties_sf %>%
          st_drop_geometry() %>%
          left_join(year_census_data, by = "GISJOIN") %>%
          transmute(
            GISJOIN,
            state,
            census_pop = AG3001,
            enslaved = AH3003,
            farmv_total = AGV001,
            improved = AGP001,
            unimproved = AGP002,
            land = improved + unimproved,
            farmv = ifelse(land > 0, farmv_total / land, NA_real_),
            pc_enslaved = ifelse(census_pop > 0, enslaved / census_pop * 100, NA_real_),
            cotton = AGY007 * 400,
            corn = AGY003
          )
      }
      else if (year_str == "1870") {
        counties_df <- counties_sf %>%
          st_drop_geometry() %>%
          left_join(year_census_data, by = "GISJOIN") %>%
          transmute(
            GISJOIN,
            state,
            census_pop = AJR001,
            enslaved = 0,
            farmv_total = AJV001,
            improved = AJU001,
            unimproved = AJU002 + AJU003,
            land = improved + unimproved,
            farmv = ifelse(land > 0, farmv_total / land, NA_real_),
            pc_enslaved = 0,
            cotton = AJ1010 * 400,
            corn = AJ1004
          )
      }
      else if (year_str == "1880") {
        counties_df <- counties_sf %>%
          st_drop_geometry() %>%
          left_join(year_census_data, by = "GISJOIN") %>%
          transmute(
            GISJOIN,
            state,
            census_pop = AOB001,
            enslaved = 0,
            farmv_total = AOD001 + AOD002 + AOD003,
            improved = AOS001,
            unimproved = AOS002,
            land = improved + unimproved,
            farmv = ifelse(land > 0, farmv_total / land, NA_real_),
            pc_enslaved = 0,
            cotton = AOH011,
            corn = AOH007
          )
      }
      else if (year_str == "1890") {
        counties_df <- counties_sf %>%
          st_drop_geometry() %>%
          left_join(year_census_data, by = "GISJOIN") %>%
          transmute(
            GISJOIN,
            state,
            census_pop = ASW001,
            enslaved = 0,
            farmv_total = AUK001 + AUK002 + AUK003,
            improved = AUJ001,
            unimproved = AUJ002,
            land = improved + unimproved,
            farmv = ifelse(land > 0, farmv_total / land, NA_real_),
            pc_enslaved = 0,
            cotton = ATB007 * 477,
            corn = ATB003
          )
      }
      else if (year_str == "1900") {
        counties_df <- counties_sf %>%
          st_drop_geometry() %>%
          left_join(year_census_data, by = "GISJOIN") %>%
          transmute(
            GISJOIN,
            state,
            census_pop = AWS001,
            enslaved = 0,
            farmv_total = AWW001 + AWW002 + AWW003 + AWW004,
            improved = AWU001,
            unimproved = AWT001 - AWU001,
            land = improved + unimproved,
            farmv = ifelse(land > 0, farmv_total / land, NA_real_),
            pc_enslaved = 0,
            cotton = (AXO024 + AXO026) * 500,
            corn = AXO003
          )
      }
      
      if (is.null(counties_df)) {
        warning(sprintf("Failed to process data for year %s. Skipping.", year_str))
        next
      }
      
      # Add year column
      counties_df$year <- as.integer(year_str)
      
      # Calculate national average farmv for the year
      national_avg_farmv <- counties_df %>%
        filter(!(state %in% c("Alaska Territory", "Hawaii Territory"))) %>%
        summarize(total_farmv = sum(farmv_total, na.rm = TRUE),
                  total_land = sum(land, na.rm = TRUE),
                  national_avg_farmv = ifelse(total_land > 0, total_farmv / total_land, NA_real_))
      
      national_avg_farmv <- national_avg_farmv$national_avg_farmv
      
      # Calculate farmv_na
      counties_df <- counties_df %>%
        mutate(farmv_na = ifelse(!is.na(farmv) & !is.na(national_avg_farmv) & national_avg_farmv > 0,
                                 farmv / national_avg_farmv * 100, NA_real_))
      
      # Adjust cotton calculation for 1880
      if (year_str == "1880") {
        counties_df <- counties_df %>%
          mutate(cotton = ifelse(state %in% c("Texas", "Arkansas", "Missouri"), 
                                 cotton * 500/475, cotton))
      }
      
      # Save data for this year
      write_csv(counties_df, raw_file)
      cat("Raw data for year", year_str, "has been saved to", raw_file, "\n")
    } else {
      cat(sprintf("Raw data file %s already exists. Using existing file.\n", raw_file))
    }
  }
  
  # Create compiled database
  compile_event_study_database(year_strings)
}

# Compile event study database from raw files
compile_event_study_database <- function(years) {
  cat("\n=== Compiling Event Study Database ===\n")
  
  year_dataframes <- list()
  
  # Read raw data for each year
  for (yr in years) {
    raw_file <- paste0("Event_study/", yr, "_raw.csv")
    if (file.exists(raw_file)) {
      cat(sprintf("Reading raw data for year %s...\n", yr))
      year_dataframes[[yr]] <- read_csv(raw_file, show_col_types = FALSE)
    } else {
      warning(sprintf("Raw data file %s not found. Skipping.", raw_file))
    }
  }
  
  # Check if any years were processed
  if (length(year_dataframes) == 0) {
    stop("Failed to find any raw data files for Event Study database.")
  }
  
  # Combine all years
  cat("Combining all processed years...\n")
  all_counties <- bind_rows(year_dataframes)
  
  # Write the final database
  write_csv(all_counties, "Event_study/Event_study_database.csv")
  
  cat(sprintf("Event Study Database created with %d rows covering %d years.\n", 
              nrow(all_counties), length(unique(all_counties$year))))
  
  return(all_counties)
}

# Process year function
process_year <- function(year, census_data, zones_1900) {
  cat(sprintf("\nProcessing year: %s\n", year))
  
  if (year != "1900") {
    # Find shapefile
    shapefile_path <- find_shapefile(year)
    cat("Using shapefile:", shapefile_path, "\n")
    
    cat("Reading county shapefile...\n")
    counties <- st_read(shapefile_path, quiet = TRUE) %>%
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

# Modified function to perform processing for all years
perform_processing <- function() {
  # Make sure we have the Event_study_database.csv
  if (!file.exists("Event_study/Event_study_database.csv")) {
    cat("Event study database not found. Creating raw data and database...\n")
    process_raw_data()
  }
  
  # Define the years to process
  years <- seq(1850, 1900, by = 10)
  year_strings <- as.character(years)
  
  # Read the 1900 county boundaries
  shapefile_path_1900 <- find_shapefile("1900")
  cat("Reading 1900 county boundaries from:", shapefile_path_1900, "\n")
  zones_1900 <- st_read(shapefile_path_1900, quiet = TRUE) %>%
    fix_geometries()
  
  # Calculate centroids and convert to miles
  centroids_1900 <- st_centroid(zones_1900)
  coords_1900 <- st_coordinates(centroids_1900)
  coords_df <- data.frame(
    GISJOIN = zones_1900$GISJOIN, 
    longitude = coords_1900[,1] / 1609.34,  # Convert meters to miles
    latitude = coords_1900[,2] / 1609.34    # Convert meters to miles
  )
  
  # Read the census data
  cat("Reading census data...\n")
  census_data <- read_csv("Event_study/Event_study_database.csv", show_col_types = FALSE)
  
  # Process all years including 1900
  for (year in years) {
    cat(sprintf("\n\n--- Processing year %d ---\n", year))
    
    result <- process_year(as.character(year), filter(census_data, year == !!year), zones_1900)
    
    # Save result for this year with simplified file name
    output_file <- sprintf("Event_study/%d_normalized.csv", year)
    cat(sprintf("\nWriting results for year %d to %s\n", year, output_file))
    write_csv(result, output_file)
    
    cat(sprintf("\nFinished processing year %d\n", year))
  }
  
  return(coords_df)  # Return the coordinates dataframe
}

# Compile panel data - unchanged
compile_panel_data <- function() {
  cat("Compiling panel data from 1850 to 1900...\n")
  
  years <- seq(1850, 1900, by = 10)
  
  panel_data <- map_dfr(years, function(year) {
    cat(sprintf("Processing year %d...\n", year))
    file_name <- sprintf("Event_study/%d_normalized.csv", year)
    
    data <- read_csv(file_name, show_col_types = FALSE)
    
    # Add year column
    data <- data %>% mutate(year = year)
    
    return(data)
  })
  
  # Ensure consistent column order across all years
  all_columns <- unique(c("year", unlist(map(years, function(year) {
    file_name <- sprintf("Event_study/%d_normalized.csv", year)
    names(read_csv(file_name, show_col_types = FALSE))
  }))))
  
  # Reorder columns and fill missing columns with NA
  panel_data <- panel_data %>%
    select(all_of(all_columns))
  
  # Read geology data
  cat("Reading geology data...\n")
  elevation_data <- read_csv("Data/Geology/USGS_elevation/elevation_1900.csv", 
                             col_types = cols(GISJOIN = col_character(), 
                                              elevation_mean = col_double()))
  
  slope_data <- read_csv("Data/Geology/USGS_slope/slope_1900.csv", 
                         col_types = cols(GISJOIN = col_character(), 
                                          slope_mean = col_double()))
  
  geology_data <- elevation_data %>%
    left_join(slope_data, by = "GISJOIN") %>%
    select(GISJOIN, elevation_mean, slope_mean)
  
  # Add geology data to panel data
  cat("Adding geology data to panel data...\n")
  panel_data <- panel_data %>%
    left_join(geology_data, by = "GISJOIN")
  
  # Calculate and add longitude and latitude data
  cat("Calculating and adding longitude and latitude data...\n")
  
  # Read the 1900 county boundaries
  shapefile_path_1900 <- find_shapefile("1900")
  cat("Reading 1900 county boundaries from:", shapefile_path_1900, "\n")
  zones_1900 <- st_read(shapefile_path_1900, quiet = TRUE) %>%
    fix_geometries()
  
  centroids_1900 <- st_centroid(zones_1900)
  coords_1900 <- st_coordinates(centroids_1900)
  coords_df <- data.frame(
    GISJOIN = zones_1900$GISJOIN, 
    longitude = coords_1900[,1] / 1609.34,  # Convert meters to miles
    latitude = coords_1900[,2] / 1609.34    # Convert meters to miles
  )
  
  panel_data <- panel_data %>%
    left_join(coords_df, by = "GISJOIN")
  
  # Calculate national average farmv for each year
  cat("Calculating national average farmv for each year...\n")
  national_avg_farmv <- panel_data %>%
    filter(!(state %in% c("Alaska Territory", "Hawaii Territory"))) %>%
    group_by(year) %>%
    summarize(total_farmv = sum(farmv_total, na.rm = TRUE),
              total_land = sum(improved + unimproved, na.rm = TRUE),
              national_avg_farmv = total_farmv / total_land)
  
  # Add farmv_na (farmv relative to national average) to panel data
  cat("Adding farmv_na to panel data...\n")
  panel_data <- panel_data %>%
    left_join(national_avg_farmv %>% select(year, national_avg_farmv), by = "year") %>%
    mutate(farmv_na = farmv / national_avg_farmv * 100) %>%
    select(-national_avg_farmv)  # Remove the temporary national average column
  
  # Write panel data to CSV
  output_file <- "Event_study/Event_study_panel_data.csv"
  write_csv(panel_data, output_file)
  cat(sprintf("Panel data compiled successfully. Output file: %s\n", output_file))
  
  return(panel_data)
}

# Main function
main <- function() {
  cat("Starting the processing...\n")
  
  # Perform processing
  perform_processing()
  
  # Compile panel data
  panel_data <- compile_panel_data()
  
  cat("All operations completed successfully.\n")
}

# Run the main function
main()