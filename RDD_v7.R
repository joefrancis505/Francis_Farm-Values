setwd(getSrcDirectory(function(dummy) {dummy}))
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load packages
packages <- c("sf", "rd2d", "dplyr", "readr")
install.packages(packages[!packages %in% rownames(installed.packages())])
invisible(lapply(packages, library, character.only = TRUE))

# Create output directories
dir.create("RDD/Plots", recursive = TRUE, showWarnings = FALSE)
dir.create("RDD/Tables", recursive = TRUE, showWarnings = FALSE)

# Helper functions
load_and_fix_shapefile <- function(path) {
  st_read(path, quiet = TRUE) %>%
    st_make_valid() %>%
    st_cast("MULTILINESTRING")
}

generate_border_points <- function(border, n = 100) {
  border_line <- if (st_geometry_type(border, by_geometry = FALSE) == "MULTILINESTRING") {
    st_cast(border, "LINESTRING")
  } else {
    border
  }
  
  border_points <- st_line_sample(border_line, n = n, type = "regular")
  border_points <- st_cast(border_points, "POINT")
  border_points_sf <- st_sf(point_id = 1:n, geometry = border_points)
  
  coords <- st_coordinates(border_points_sf)
  border_matrix <- data.frame(b1 = coords[, "X"], b2 = coords[, "Y"])
  
  list(points_sf = border_points_sf, coords_matrix = border_matrix)
}

calculate_border_distance <- function(counties, border) {
  if (st_geometry_type(counties, by_geometry = FALSE) != "POINT") {
    counties <- st_centroid(counties)
  }
  
  distances <- st_distance(counties, border, by_element = FALSE)
  apply(distances, 1, min)
}

# RD2D analysis function
perform_rd2d_analysis <- function(data_sf, border_coords, exclude_border = FALSE) {
  analysis_data <- data_sf
  if (exclude_border) {
    analysis_data <- analysis_data %>% filter(border_county != 1)
  }
  
  analysis_data <- analysis_data %>%
    filter(!is.na(log_farmv) & is.finite(log_farmv))
  
  Y <- analysis_data$log_farmv
  X <- st_coordinates(analysis_data)
  t <- analysis_data$treatment
  
  border_coords <- as.matrix(border_coords)
  
  rd2d(Y = Y, X = X, t = t, b = border_coords)
}

# Extract AATE from rd2d results
extract_aate_direct <- function(rd2d_result, aate_weights) {
  aate_weights <- aate_weights / sum(aate_weights)
  
  est_q <- rd2d_result$results$Est.q
  cov_q <- rd2d_result$cov.q
  
  aate_est_q <- sum(aate_weights * est_q)
  se_aate <- sqrt(as.numeric(aate_weights %*% cov_q %*% aate_weights))
  z_stat <- aate_est_q / se_aate
  p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
  
  alpha <- 0.05
  z_val <- qnorm(1 - alpha/2)
  ci_lower <- aate_est_q - z_val * se_aate
  ci_upper <- aate_est_q + z_val * se_aate
  
  list(
    coefficient_q = aate_est_q,
    se = se_aate,
    z = z_stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

# Plot function
create_coefficient_plot <- function(rd2d_result, aate_value, border_distances, year, output_path, y_range) {
  plot_width <- 9.2
  plot_height <- 5.8
  
  pdf(output_path, width = plot_width, height = plot_height)
  
  par(pin = c(plot_width - 2.4, plot_height - 1.0))
  par(mai = c(0.9, 1.2, 0.2, 1.2))
  par(family = "sans", cex = 1.2, cex.axis = 1.2, cex.lab = 1.2, tck = 0.01, lwd = 0.8, las = 1, mgp = c(2.5, 0.8, 0))
  
  results_df <- rd2d_result$results
  coefs <- results_df$Est.q
  lower_ci <- results_df$CI.lower
  upper_ci <- results_df$CI.upper
  
  valid_idx <- !is.na(coefs) & !is.na(lower_ci) & !is.na(upper_ci)
  coefs <- coefs[valid_idx]
  lower_ci <- lower_ci[valid_idx]
  upper_ci <- upper_ci[valid_idx]
  border_distances <- border_distances[valid_idx]
  
  x_padding <- 80
  x_range <- c(-x_padding, 2000 + x_padding)
  
  plot(border_distances, coefs, 
       type = "n",
       xlab = "Miles along border (west to east)",
       ylab = "",
       xlim = x_range,
       ylim = y_range,
       xaxs = "i",
       yaxs = "i",
       axes = FALSE)
  
  mtext("Coefficient", side = 2, line = 3.5, cex = 1.4, las = 0)
  
  x_ticks <- seq(0, 2000, by = 500)
  axis(1, at = x_ticks, labels = x_ticks, lwd = 0, lwd.ticks = 0.8, padj = -0.1)
  
  y_ticks <- pretty(y_range, n = 6)
  y_labels <- sprintf("%.1f", y_ticks)
  axis(2, at = y_ticks, labels = y_labels, lwd = 0, lwd.ticks = 0.8, padj = 0.4)
  
  arrows(border_distances, lower_ci, border_distances, upper_ci,
         length = 0.02, angle = 90, code = 3, col = "gray60", lwd = 0.8)
  
  points(border_distances, coefs, pch = 21, bg = "black", col = "white", cex = 0.6, lwd = 1)
  
  if (!is.null(aate_value) && !is.na(aate_value)) {
    abline(h = aate_value, lty = 1, col = "black", lwd = 3)
  }
  
  abline(h = 0, lty = 1, col = "black", lwd = 0.8)
  box(lwd = 0.8)
  
  dev.off()
}

# Main analysis function
run_analysis <- function(year) {
  # Load border
  border_file <- "Data/Border/1820_border/1820_border.shp"
  if (!file.exists(border_file)) {
    border_files <- list.files("Data", pattern = "1820.*border.*\\.shp$", full.names = TRUE, recursive = TRUE)
    if (length(border_files) == 0) stop("Border file not found")
    border_file <- border_files[1]
  }
  
  border <- load_and_fix_shapefile(border_file) %>% st_cast("LINESTRING")
  
  # Load RDD database
  if (!file.exists("RDD/RDD_database.csv")) {
    stop("RDD database not found. Please run RDD_database.R first.")
  }
  
  data <- read_csv("RDD/RDD_database.csv", show_col_types = FALSE)
  
  data_year <- data %>%
    filter(year == !!year) %>%
    filter(!(state %in% c("Kansas Territory", "Nebraska Territory", "Indian Territory", "Unorganized Territory", "Kansas", "Nebraska"))) %>%
    mutate(treatment = slavery_legal)
  
  # Load county shapefile
  county_shapefile <- list.files(paste0("Data/Shapefiles/", year), pattern = "\\.shp$", full.names = TRUE)[1]
  counties_sf <- st_read(county_shapefile, quiet = TRUE) %>%
    st_transform(st_crs(border))
  
  if ("STATENAM" %in% names(counties_sf) && !"state" %in% names(counties_sf)) {
    counties_sf <- counties_sf %>% rename(state = STATENAM)
  }
  
  data_year_sf <- counties_sf %>%
    st_centroid() %>%
    left_join(data_year, by = "GISJOIN") %>%
    st_as_sf() %>%
    mutate(log_farmv = ifelse(farmv > 0, log(farmv), NA)) %>%
    filter(!is.na(log_farmv) & is.finite(log_farmv))
  
  data_year_sf$border_dist <- calculate_border_distance(data_year_sf, border)
  
  # Generate border points
  border_100 <- generate_border_points(border, n = 100)
  
  # Define specifications
  specifications <- list(
    list(exclude_border = FALSE, suffix = "_base", name = "Base", y_range = c(-1.5, 1.5)),
    list(exclude_border = TRUE, suffix = "_no_border", name = "No Border Counties", y_range = c(-8, 8))
  )
  
  results_table <- list()
  
  # Run analysis for each specification
  for (spec in specifications) {
    result_100 <- perform_rd2d_analysis(data_year_sf, border_100$coords_matrix, exclude_border = spec$exclude_border)
    
    # Calculate border distances
    total_border_length <- as.numeric(st_length(border))
    n_points_100 <- nrow(border_100$coords_matrix)
    border_distances_100 <- seq(0, total_border_length, length.out = n_points_100) / 1609.344
    
    # Extract AATE
    num_points_100 <- nrow(border_100$coords_matrix)
    aate_weights <- rep(1 / num_points_100, num_points_100)
    aate_result <- extract_aate_direct(result_100, aate_weights)
    
    # Store results
    results_table[[paste0(year, spec$suffix)]] <- list(
      year = year,
      specification = spec$name,
      suffix = spec$suffix,
      aate = aate_result$coefficient_q,
      se = aate_result$se,
      pvalue = aate_result$p_value
    )
    
    # Create plot
    plot_output_path <- paste0("RDD/Plots/", year, "_rd2d_plot", spec$suffix, ".pdf")
    create_coefficient_plot(result_100, aate_result$coefficient_q, border_distances_100, 
                            year, plot_output_path, spec$y_range)
  }
  
  return(results_table)
}

# Significance stars function
add_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("")
}

# Create results table
create_results_table <- function(all_results) {
  spec_order <- c("_base", "_no_border")
  spec_names <- c("Base", "No Border Counties")
  
  table_data <- data.frame(
    Specification = spec_names,
    `1850` = character(2),
    `1860` = character(2),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(spec_order)) {
    suffix <- spec_order[i]
    
    # 1850 data
    key_1850 <- paste0("1850", suffix)
    if (key_1850 %in% names(all_results)) {
      result_1850 <- all_results[[key_1850]]
      aate_1850 <- result_1850$aate
      se_1850 <- result_1850$se
      p_1850 <- result_1850$pvalue
      stars_1850 <- add_significance_stars(p_1850)
      
      if (!is.na(aate_1850) && !is.na(se_1850)) {
        table_data$X1850[i] <- paste0(sprintf("%.3f", aate_1850), stars_1850, "\n(", sprintf("%.3f", se_1850), ")")
      } else {
        table_data$X1850[i] <- "N/A"
      }
    }
    
    # 1860 data
    key_1860 <- paste0("1860", suffix)
    if (key_1860 %in% names(all_results)) {
      result_1860 <- all_results[[key_1860]]
      aate_1860 <- result_1860$aate
      se_1860 <- result_1860$se
      p_1860 <- result_1860$pvalue
      stars_1860 <- add_significance_stars(p_1860)
      
      if (!is.na(aate_1860) && !is.na(se_1860)) {
        table_data$X1860[i] <- paste0(sprintf("%.3f", aate_1860), stars_1860, "\n(", sprintf("%.3f", se_1860), ")")
      } else {
        table_data$X1860[i] <- "N/A"
      }
    }
  }
  
  return(table_data)
}

# Main execution
years <- c(1850, 1860)
all_results <- list()

for (year in years) {
  year_results <- run_analysis(year)
  all_results <- c(all_results, year_results)
}

# Create and save results table
results_table <- create_results_table(all_results)
write.csv(results_table, "RDD/Tables/RDD_Results.csv", row.names = FALSE)

# Save formatted results
sink("RDD/Tables/RDD_Results.txt")
cat("RDD Analysis Results: 1850 and 1860\n")
cat("===================================\n\n")
cat("Average Treatment Effects with Standard Errors\n")
cat("Note: * p < 0.05, ** p < 0.01, *** p < 0.001\n\n")

cat(sprintf("%-20s %-15s %-15s\n", "Specification", "1850", "1860"))
cat(paste(rep("-", 50), collapse = ""), "\n")

for (i in 1:nrow(results_table)) {
  cat(sprintf("%-20s %-15s %-15s\n", 
              results_table$Specification[i],
              gsub("\n", "\n                     ", results_table$X1850[i]),
              gsub("\n", "\n                     ", results_table$X1860[i])))
  cat("\n")
}
sink()