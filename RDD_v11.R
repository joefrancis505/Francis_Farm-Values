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

# Clear console
cat("\014")

# Ask user if they want to include robustness checks
include_robustness <- tolower(readline("Would you like to include the robustness checks (y/n)? ")) == "y"

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

# Function to perform RDD at a single point
perform_rdd <- function(data, point, include_slope = FALSE, exclude_border = FALSE, include_enslaved = FALSE,
                        bw_method = "mserd", kernel = "triangular") {
  result_df <- data.frame(point_id = point$point_id,
                          coef = NA, se = NA, p_value = NA,
                          n_treat = NA, n_control = NA,
                          bw_left = NA, bw_right = NA,
                          enslaved_share_coef = NA)
  
  tryCatch({
    if (exclude_border) {
      data <- data %>% filter(border_county != 1)
    }
    
    if ("point_id" %in% names(point) && point$point_id == 1) {
      # For robustness checks, use pre-calculated border_dist
      data$scaled_dist <- data$border_dist / 1609.344  # Convert meters to miles
      data$scaled_dist[data$treatment == 0] <- -data$scaled_dist[data$treatment == 0]
    } else {
      distances <- st_distance(data, point)
      data$scaled_dist <- as.numeric(distances) / 1609.344  # Convert meters to miles
      data$scaled_dist[data$treatment == 0] <- -data$scaled_dist[data$treatment == 0]
    }
    
    n_treat <- sum(data$scaled_dist >= 0, na.rm = TRUE)
    n_control <- sum(data$scaled_dist < 0, na.rm = TRUE)
    
    if (n_treat == 0 || n_control == 0) {
      warning(paste("Not enough observations on both sides of the cutoff for point", point$point_id))
      return(result_df)
    }
    
    covs <- NULL
    covariate_names <- c()
    
    if (include_slope) {
      covs <- cbind(slope = data$slope, elevation = data$elevation)
      covariate_names <- c(covariate_names, "Slope", "Elevation")
    }
    if (include_enslaved) {
      covs <- if (is.null(covs)) data$enslaved_share else cbind(covs, enslaved_share = data$enslaved_share)
      covariate_names <- c(covariate_names, "EnslavedShare")
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
    
    if (include_enslaved) {
      result_df$enslaved_share_coef <- rdd_result$beta_covs[nrow(rdd_result$beta_covs), 1]
    }
    
    return(result_df)
  }, error = function(e) {
    warning(paste("Error in perform_rdd for point", point$point_id, ":", conditionMessage(e)))
    return(result_df)
  })
}

# Function to calculate distance to the Ohio-Virginia border
calculate_ohio_virginia_distance <- function(counties, ohio_virginia_border) {
  distances <- st_distance(counties, ohio_virginia_border, by_element = FALSE)
  apply(distances, 1, min)
}

# Main analysis function
run_analysis <- function(year) {
  border_file <- "Data/Border/1820_border/1820_border.shp"
  border <- load_and_fix_shapefile(border_file, paste("Border", year))
  border <- st_cast(border, "LINESTRING")
  
  # Load Ohio-Virginia border
  ohio_virginia_border_file <- "Data/Border/1820_border_Ohio_Virginia/1820_border.shp"
  ohio_virginia_border <- load_and_fix_shapefile(ohio_virginia_border_file, "Ohio-Virginia Border")
  ohio_virginia_border <- st_cast(ohio_virginia_border, "LINESTRING")
  
  data <- read_csv("RDD_database.csv", show_col_types = FALSE)
  
  data_year <- data %>%
    filter(year == !!year) %>%
    filter(!(state %in% c("Kansas Territory", "Nebraska Territory", "Unorganized Territory"))) %>%
    mutate(treatment = slavery_legal)
  
  # Create centroids from longitude and latitude
  counties_sf <- st_read(paste0("Data/Counties/", year, "_US_county/US_county_", year, ".shp"), quiet = TRUE) %>%
    st_transform(st_crs(border))  # Ensure CRS matches
  
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
    list(include_slope = FALSE, exclude_border = FALSE, include_enslaved = FALSE, suffix = ""),
    list(include_slope = TRUE, exclude_border = FALSE, include_enslaved = FALSE, suffix = "_with_slope"),
    list(include_slope = TRUE, exclude_border = TRUE, include_enslaved = FALSE, suffix = "_with_slope_no_border"),
    list(include_slope = TRUE, exclude_border = TRUE, include_enslaved = TRUE, suffix = "_enslaved_no_border")
  )
  
  if (include_robustness) {
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
        coef = NA,
        se = NA,
        p_value = NA,
        n_treat = NA,
        n_control = NA,
        bw_left = NA,
        bw_right = NA,
        enslaved_share_coef = NA
      )
    
    pb <- progress_bar$new(
      format = paste("Analyzing", year, "robustness checks [:bar] :percent eta: :eta"),
      total = nrow(robustness_results), clear = FALSE, width = 60
    )
    
    for (i in 1:nrow(robustness_results)) {
      spec <- specifications[[robustness_results$spec[i]]]
      data_filtered <- data_year_sf
      
      if (robustness_results$exclude_ohio_virginia[i]) {
        data_filtered <- data_filtered %>% filter(ohio_virginia_dist > 50 * 1609.344)  # 50 miles in meters
      }
      
      result <- perform_rdd(data_filtered, 
                            point = data.frame(point_id = 1),  # Dummy point, using border_dist
                            include_slope = spec$include_slope,
                            exclude_border = spec$exclude_border,
                            include_enslaved = spec$include_enslaved,
                            bw_method = robustness_results$bw_method[i],
                            kernel = robustness_results$kernel[i])
      
      robustness_results[i, c("coef", "se", "p_value", "n_treat", "n_control", "bw_left", "bw_right", "enslaved_share_coef")] <- 
        result[, c("coef", "se", "p_value", "n_treat", "n_control", "bw_left", "bw_right", "enslaved_share_coef")]
      
      pb$tick()
    }
    
    # Save original robustness results
    write_csv(robustness_results %>% filter(!exclude_ohio_virginia), 
              paste0("Results/SpatialRDD/", year, "_farmv_robustness.csv"))
    
    # Save new robustness results (excluding Ohio-Virginia border counties)
    write_csv(robustness_results %>% filter(exclude_ohio_virginia), 
              paste0("Results/SpatialRDD/", year, "_farmv_robustness_no_Ohio_Virginia.csv"))
  }
  
  # Main analysis
  border_points <- st_line_sample(border, n = 50) %>%
    st_cast("POINT") %>%
    st_sf() %>%
    mutate(point_id = 1:50)
  
  for (spec in specifications) {
    pb <- progress_bar$new(
      format = paste("Analyzing", year, "log_farmv", spec$suffix, "[:bar] :percent eta: :eta"),
      total = nrow(border_points), clear = FALSE, width = 60
    )
    
    results <- map_dfr(1:nrow(border_points), function(i) {
      result <- perform_rdd(data_year_sf, border_points[i,],
                            include_slope = spec$include_slope, 
                            exclude_border = spec$exclude_border,
                            include_enslaved = spec$include_enslaved)
      pb$tick()
      return(result)
    })
    
    results_sf <- border_points %>%
      left_join(results, by = "point_id")
    
    file_prefix <- paste0("Results/SpatialRDD/", year, "_farmv", spec$suffix)
    write_csv(results, paste0(file_prefix, ".csv"))
    st_write(results_sf, paste0(file_prefix, ".gpkg"), delete_layer = TRUE)
  }
}

# Run analysis for 1850 and 1860
years <- c(1850, 1860)

for (year in years) {
  run_analysis(year)
}