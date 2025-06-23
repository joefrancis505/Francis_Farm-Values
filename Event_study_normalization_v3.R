setwd(getSrcDirectory(function(dummy) {dummy}))
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load packages
packages <- c("sf", "stars", "fasterize", "dplyr", "readr", "exactextractr", "terra")
install.packages(packages[!packages %in% rownames(installed.packages())])
invisible(lapply(packages, library, character.only = TRUE))

# Create output directory
dir.create("Event_study", showWarnings = FALSE)

# Helper functions
fix_geometries <- function(sf_object) {
  sf_object %>% st_make_valid() %>% st_buffer(0)
}

calculate_densities <- function(sf_object) {
  sf_object %>%
    mutate(across(c(farmv_total, improved, unimproved, enslaved, census_pop, cotton, corn), 
                  ~ . / SHAPE_AREA, 
                  .names = "{.col}_density"))
}

rasterize_data <- function(sf_object, resolution = 200) {
  bbox <- st_bbox(sf_object)
  crs_proj4 <- st_crs(sf_object)$proj4string
  
  raster_template <- rast(xmin=bbox["xmin"], xmax=bbox["xmax"], 
                          ymin=bbox["ymin"], ymax=bbox["ymax"], 
                          resolution=resolution)
  crs(raster_template) <- crs_proj4
  
  variables <- c("farmv_total_density", "improved_density", "unimproved_density", 
                 "enslaved_density", "census_pop_density", "cotton_density", "corn_density")
  
  raster_list <- map(variables, function(variable) {
    if (variable %in% names(sf_object)) {
      sf_object[[variable]] <- as.numeric(sf_object[[variable]])
      terra::rasterize(vect(sf_object), raster_template, field = variable, fun = "sum")
    }
  })
  
  raster_list <- raster_list[!sapply(raster_list, is.null)]
  if (length(raster_list) > 0) {
    raster_stack <- do.call(c, raster_list)
    names(raster_stack) <- variables[sapply(raster_list, function(x) !is.null(x))]
    return(raster_stack)
  }
  stop("No variables were successfully rasterized.")
}

zonal_stats <- function(raster_stack, zones_sf) {
  exact_extract(raster_stack, zones_sf, fun = "mean", append_cols = c("GISJOIN", "STATENAM"))
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

calculate_derived_variables <- function(df) {
  df %>%
    mutate(
      area = SHAPE_AREA / 2589988.11,
      farmv = ifelse(improved + unimproved > 0, farmv_total / (improved + unimproved), NA),
      pc_enslaved = ifelse(census_pop > 0, (enslaved / census_pop) * 100, NA),
      cotton_pc = ifelse(census_pop > 0, cotton / census_pop, NA),
      ccratio = ifelse(corn > 0, cotton / corn, NA)
    )
}

# Process year function
process_year <- function(year, census_data, zones_1900) {
  if (year != "1900") {
    # Read and process historical counties
    shp_dir <- paste0("Data/Shapefiles/", year)
    shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)
    
    counties <- st_read(shp_files[1], quiet = TRUE) %>%
      fix_geometries() %>%
      left_join(census_data, by = "GISJOIN") %>%
      filter(census_pop > 0) %>%
      calculate_densities()
    
    # Rasterize and perform zonal statistics
    raster_data <- rasterize_data(counties)
    zonal_results <- zonal_stats(raster_data, zones_1900)
    final_results <- convert_to_absolute(zonal_results, zones_1900)
  } else {
    # Use 1900 data directly
    final_results <- zones_1900 %>%
      left_join(census_data, by = "GISJOIN") %>%
      mutate(state = STATENAM) %>%
      select(-STATENAM)
  }
  
  # Calculate derived variables and remove geometry
  final_results %>%
    calculate_derived_variables() %>%
    st_drop_geometry()
}

# Main processing
years <- seq(1850, 1900, by = 10)

# Read 1900 county boundaries
zones_1900 <- st_read(list.files("Data/Shapefiles/1900", pattern = "\\.shp$", full.names = TRUE)[1], quiet = TRUE) %>%
  fix_geometries()

# Read census data
census_data <- read_csv("Event_study/Event_study_database.csv", show_col_types = FALSE)

# Process all years
for (year in years) {
  result <- process_year(as.character(year), filter(census_data, year == !!year), zones_1900)
  output_file <- sprintf("Event_study/%d_normalized.csv", year)
  write_csv(result, output_file)
}

# Compile panel data
panel_data <- map_dfr(years, function(year) {
  file_name <- sprintf("Event_study/%d_normalized.csv", year)
  read_csv(file_name, show_col_types = FALSE) %>% mutate(year = year)
})

# Calculate national averages and add relative farm values
national_avg_farmv <- panel_data %>%
  filter(!(state %in% c("Alaska Territory", "Hawaii Territory"))) %>%
  group_by(year) %>%
  summarize(total_farmv = sum(farmv_total, na.rm = TRUE),
            total_land = sum(improved + unimproved, na.rm = TRUE),
            national_avg_farmv = total_farmv / total_land)

panel_data <- panel_data %>%
  left_join(national_avg_farmv %>% select(year, national_avg_farmv), by = "year") %>%
  mutate(farmv_na = farmv / national_avg_farmv * 100) %>%
  select(-national_avg_farmv)

# Add coordinates
centroids_1900 <- st_centroid(zones_1900)
coords_1900 <- st_coordinates(centroids_1900)
coords_df <- data.frame(
  GISJOIN = zones_1900$GISJOIN, 
  longitude = coords_1900[,1] / 1609.34,
  latitude = coords_1900[,2] / 1609.34
)

panel_data <- panel_data %>% left_join(coords_df, by = "GISJOIN")

# Save final panel data
write_csv(panel_data, "Event_study/Event_study_panel_data.csv")