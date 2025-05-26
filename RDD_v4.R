# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("sf", "rdrobust", "dplyr", "tidyr", "readr", "purrr", "progress")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Create necessary directories if they don't exist
for (dir_path in c("RDD/CSV", "RDD/Geopackage")) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

# Set robustness checks to always be included
include_robustness <- TRUE

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
generate_border_points <- function(border, n = 50) {
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

# Function to calculate distance to the Ohio-Virginia border
calculate_ohio_virginia_distance <- function(counties, ohio_virginia_border) {
  distances <- st_distance(counties, ohio_virginia_border, by_element = FALSE)
  apply(distances, 1, min)
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

# Function for 50-point analysis
perform_rdd_50points <- function(data, point, include_geology = FALSE, exclude_border = FALSE, include_enslaved = FALSE, include_ph = FALSE, include_pc_black = FALSE, year) {
  result_df <- data.frame(point_id = point$point_id,
                          coef = NA, se = NA, p_value = NA,
                          n_treat = NA, n_control = NA,
                          bw_left = NA, bw_right = NA)
  
  tryCatch({
    if (exclude_border) {
      data <- data %>% filter(border_county != 1)
    }
    
    distances <- st_distance(data, point)
    data$scaled_dist <- as.numeric(distances) / 1609.344  # Convert meters to miles
    data$scaled_dist[data$treatment == 0] <- -data$scaled_dist[data$treatment == 0]
    
    n_treat <- sum(data$scaled_dist >= 0, na.rm = TRUE)
    n_control <- sum(data$scaled_dist < 0, na.rm = TRUE)
    
    if (n_treat == 0 || n_control == 0) {
      warning(paste("Not enough observations on both sides of the cutoff for point", point$point_id))
      return(result_df)
    }
    
    covs <- NULL
    if (include_geology) {
      covs <- cbind(slope = data$slope, elevation = data$elevation)
    }
    if (include_ph) {
      covs <- if (is.null(covs)) data$ph else cbind(covs, ph = data$ph)
    }
    if (include_enslaved) {
      covs <- if (is.null(covs)) data$pc_enslaved else cbind(covs, pc_enslaved = data$pc_enslaved)
    }
    if (include_pc_black) {
      covs <- if (is.null(covs)) data$pc_black else cbind(covs, pc_black = data$pc_black)
    }
    
    rdd_result <- rdrobust(y = data$log_farmv, x = data$scaled_dist, c = 0, covs = covs)
    
    result_df$coef <- rdd_result$coef[2, 1]
    result_df$se <- rdd_result$se[2, 1]
    result_df$p_value <- rdd_result$pv[2, 1]
    result_df$n_treat <- rdd_result$N_h[2]
    result_df$n_control <- rdd_result$N_h[1]
    result_df$bw_left <- rdd_result$bws[1, 1]
    result_df$bw_right <- rdd_result$bws[1, 2]
    
    # Add covariate coefficients
    if (!is.null(covs)) {
      for (i in 1:ncol(covs)) {
        result_df[[paste0(colnames(covs)[i], "_coef")]] <- rdd_result$beta_covs[i, 1]
      }
    }
    
    return(result_df)
  }, error = function(e) {
    warning(paste("Error in perform_rdd_50points for point", point$point_id, ":", conditionMessage(e)))
    return(result_df)
  })
}

# Function for robustness checks
perform_rdd_robustness <- function(data, include_geology = FALSE, exclude_border = FALSE, include_enslaved = FALSE,
                                   include_ph = FALSE, include_pc_black = FALSE, bw_method = "mserd", kernel = "triangular", 
                                   exclude_ohio_virginia = FALSE, year) {
  result_df <- data.frame(coef = NA, se = NA, p_value = NA,
                          n_treat = NA, n_control = NA,
                          bw_left = NA, bw_right = NA)
  
  tryCatch({
    if (exclude_border) {
      data <- data %>% filter(!(border_county == 1))
    }
    
    if (exclude_ohio_virginia) {
      data <- data %>% filter(ohio_virginia_dist > 50 * 1609.344)  # 50 miles in meters
    }
    
    data$scaled_dist <- data$border_dist / 1609.344  # Convert meters to miles
    data$scaled_dist[data$treatment == 0] <- -data$scaled_dist[data$treatment == 0]
    
    n_treat <- sum(data$scaled_dist >= 0, na.rm = TRUE)
    n_control <- sum(data$scaled_dist < 0, na.rm = TRUE)
    
    if (n_treat == 0 || n_control == 0) {
      warning("Not enough observations on both sides of the cutoff")
      return(result_df)
    }
    
    covs <- NULL
    if (include_geology) {
      covs <- cbind(slope = data$slope, elevation = data$elevation)
    }
    if (include_ph) {
      covs <- if (is.null(covs)) data$ph else cbind(covs, ph = data$ph)
    }
    if (include_enslaved) {
      covs <- if (is.null(covs)) data$pc_enslaved else cbind(covs, pc_enslaved = data$pc_enslaved)
    }
    if (include_pc_black) {
      covs <- if (is.null(covs)) data$pc_black else cbind(covs, pc_black = data$pc_black)
    }
    
    rdd_result <- rdrobust(y = data$log_farmv, x = data$scaled_dist, c = 0, covs = covs, 
                           bwselect = bw_method, kernel = kernel)
    
    result_df$coef <- rdd_result$coef[2, 1]
    result_df$se <- rdd_result$se[2, 1]
    result_df$p_value <- rdd_result$pv[2, 1]
    result_df$n_treat <- rdd_result$N_h[2]
    result_df$n_control <- rdd_result$N_h[1]
    result_df$bw_left <- rdd_result$bws[1, 1]
    result_df$bw_right <- rdd_result$bws[1, 2]
    
    # Add covariate coefficients
    if (!is.null(covs)) {
      for (i in 1:ncol(covs)) {
        result_df[[paste0(colnames(covs)[i], "_coef")]] <- rdd_result$beta_covs[i, 1]
      }
    }
    
    return(result_df)
  }, error = function(e) {
    warning(paste("Error in perform_rdd_robustness:", conditionMessage(e)))
    return(result_df)
  })
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

# Main analysis function
run_analysis <- function(year, ipums_dir = "Data/IPUMS") {
  # Find border files
  default_border_file <- "Data/Border/1820_border/1820_border.shp"
  border_file <- find_border_file(default_border_file, "1820.*border.*\\.shp$")
  
  default_ohio_virginia_border_file <- "Data/Border/1820_border_Ohio_Virginia/1820_border.shp"
  ohio_virginia_border_file <- find_border_file(default_ohio_virginia_border_file, 
                                                "1820.*border.*Ohio.*Virginia.*\\.shp$|Ohio.*Virginia.*border.*\\.shp$")
  
  border <- load_and_fix_shapefile(border_file, paste("Border", year))
  border <- st_cast(border, "LINESTRING")
  
  # Load Ohio-Virginia border
  ohio_virginia_border <- load_and_fix_shapefile(ohio_virginia_border_file, "Ohio-Virginia Border")
  ohio_virginia_border <- st_cast(ohio_virginia_border, "LINESTRING")
  
  border_points <- generate_border_points(border, n = 50)
  
  # Check if RDD database exists
  rdd_database_path <- "RDD/RDD_database.csv"
  if (!file.exists(rdd_database_path)) {
    stop("RDD database not found at ", rdd_database_path, 
         ". Please run Databases_v1.R first to create it.")
  }
  
  data <- read_csv(rdd_database_path, show_col_types = FALSE)
  
  data_year <- data %>%
    filter(year == !!year) %>%
    filter(!(state %in% c("Kansas Territory", "Nebraska Territory", "Indian Territory", "Unorganized Territory", "Kansas", "Nebraska"))) %>%
    mutate(treatment = slavery_legal)
  
  # Find county shapefile
  county_shapefile <- find_county_shapefile(year, ipums_dir)
  
  # Create centroids
  counties_sf <- st_read(county_shapefile, quiet = TRUE) %>%
    st_transform(st_crs(border))  # Ensure CRS matches
  
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
  
  # Calculate distance to nearest border point
  data_year_sf$border_dist <- calculate_border_distance(data_year_sf, border)
  
  # Calculate distance to Ohio-Virginia border
  data_year_sf$ohio_virginia_dist <- calculate_ohio_virginia_distance(data_year_sf, ohio_virginia_border)
  
  specifications <- list(
    list(include_geology = FALSE, exclude_border = FALSE, include_enslaved = FALSE, include_ph = FALSE, include_pc_black = FALSE, suffix = ""),
    list(include_geology = TRUE, exclude_border = FALSE, include_enslaved = FALSE, include_ph = TRUE, include_pc_black = FALSE, suffix = "_with_geography"),
    list(include_geology = TRUE, exclude_border = TRUE, include_enslaved = FALSE, include_ph = TRUE, include_pc_black = FALSE, suffix = "_with_geography_no_border")
  )
  
  # Add enslaved specification only for years before 1870
  if (year < 1870) {
    specifications <- c(specifications, 
                        list(list(include_geology = TRUE, exclude_border = FALSE, include_enslaved = TRUE, include_ph = TRUE, include_pc_black = FALSE, suffix = "_with_geography_enslaved"))
    )
  }
  
  # Add black specification only for years 1870 and after
  if (year >= 1870) {
    specifications <- c(specifications, 
                        list(list(include_geology = TRUE, exclude_border = FALSE, include_enslaved = FALSE, include_ph = TRUE, include_pc_black = TRUE, suffix = "with_geography_black"))
    )
  }
  
  # 50-point analysis
  for (spec in specifications) {
    pb <- progress_bar$new(
      format = paste("Analyzing", year, "log_farmv", spec$suffix, "[:bar] :percent eta: :eta"),
      total = nrow(border_points), clear = FALSE, width = 60
    )
    
    results <- map_dfr(1:nrow(border_points), function(i) {
      result <- perform_rdd_50points(data_year_sf, border_points[i,],
                                     include_geology = spec$include_geology, 
                                     exclude_border = spec$exclude_border,
                                     include_enslaved = spec$include_enslaved,
                                     include_ph = spec$include_ph,
                                     include_pc_black = spec$include_pc_black,
                                     year = year)
      pb$tick()
      return(result)
    })
    
    results_sf <- border_points %>%
      left_join(results, by = "point_id")
    
    output_csv_path <- paste0("RDD/CSV/", year, "_farmv", spec$suffix, ".csv")
    output_gpkg_path <- paste0("RDD/Geopackage/", year, "_farmv", spec$suffix, ".gpkg")
    
    write_csv(results, output_csv_path)
    cat("Saved results to", output_csv_path, "\n")
    
    st_write(results_sf, output_gpkg_path, delete_layer = TRUE)
    cat("Saved spatial results to", output_gpkg_path, "\n")
  }
  
  # Always run robustness checks
  cat("\nRunning robustness checks for year", year, "\n")
  
  bw_methods <- c("mserd", "msetwo", "msesum", "cerrd", "certwo", "cersum")
  kernels <- c("triangular", "uniform", "epanechnikov")
  
  robustness_results <- expand.grid(
    spec = 1:length(specifications),
    bw_method = bw_methods,
    kernel = kernels,
    exclude_ohio_virginia = c(FALSE, TRUE)
  ) %>%
    mutate(
      year = year,
      coef = NA, se = NA, p_value = NA,
      n_treat = NA, n_control = NA,
      bw_left = NA, bw_right = NA
    )
  
  pb <- progress_bar$new(
    format = paste("Analyzing", year, "robustness checks [:bar] :percent eta: :eta"),
    total = nrow(robustness_results), clear = FALSE, width = 60
  )
  
  for (i in 1:nrow(robustness_results)) {
    spec <- specifications[[robustness_results$spec[i]]]
    
    result <- perform_rdd_robustness(
      data_year_sf,
      include_geology = spec$include_geology,
      exclude_border = spec$exclude_border,
      include_enslaved = spec$include_enslaved,
      include_ph = spec$include_ph,
      include_pc_black = spec$include_pc_black,
      bw_method = robustness_results$bw_method[i],
      kernel = robustness_results$kernel[i],
      exclude_ohio_virginia = robustness_results$exclude_ohio_virginia[i],
      year = year
    )
    
    # Update robustness_results with all columns from result
    robustness_results[i, names(result)] <- result
    
    pb$tick()
  }
  
  # Save robustness results
  write_csv(robustness_results %>% filter(!exclude_ohio_virginia), 
            paste0("RDD/CSV/", year, "_robustness.csv"))
  write_csv(robustness_results %>% filter(exclude_ohio_virginia),
            paste0("RDD/CSV/", year, "_robustness_no_Ohio_Virginia.csv"))
  
  cat("Robustness checks for year", year, "completed and saved\n")
}

# Main execution block
main <- function() {
  # Define years and IPUMS directory
  years <- c(1850, 1860, 1870, 1880, 1890, 1900)
  ipums_dir <- "Data/IPUMS"
  
  # Check if the IPUMS directory exists
  if (!dir.exists(ipums_dir)) {
    warning("IPUMS data directory not found at ", ipums_dir, 
            ". Using traditional file paths instead.")
  }
  
  # Run analysis for specified years
  for (year in years) {
    cat("\n=== Starting analysis for year", year, "===\n")
    run_analysis(year, ipums_dir)
    cat("\n=== Completed analysis for year", year, "===\n")
  }
  
  cat("\nAll analyses completed successfully!\n")
}

# Run the main function
main()