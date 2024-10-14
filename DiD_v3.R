# Options
sample_choice <- 1 # Choose 1 for all slave states, 2 for Upper South states, 3 for Deep South states
use_weights <- TRUE # Set to TRUE to use weights, FALSE to run unweighted regressions
include_Texas <- TRUE # Set to FALSE to exclude Texas from all calculations

# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("dplyr", "lmtest", "sandwich", "fixest", "readODS")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Clear console
cat("\014")

# Read the CSV file
data <- read.csv("DiD/DiD_panel_data.csv")

# Filter for years 1850 onward
data <- data %>% filter(year >= 1850)

# Calculate farmv_normalized
data <- data %>%
  group_by(year) %>%
  mutate(
    national_farmv = sum(farmv_total, na.rm = TRUE) / sum(improved + unimproved, na.rm = TRUE),
    farmv_normalized = (farmv / national_farmv) * 100
  ) %>%
  ungroup()

# Treatment_groups definition
treatment_groups <- list(
  group1 = list(
    states_to_include = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", 
                          "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kentucky", "Louisiana", 
                          "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                          "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", 
                          "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", 
                          "Virginia", "West Virginia", "Wisconsin"),
    slavery_states = c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky", "Louisiana", 
                       "Maryland", "Mississippi", "Missouri", "North Carolina", "South Carolina", 
                       "Tennessee", "Texas", "Virginia", "West Virginia", "District of Columbia")
  ),
  group2 = list(
    states_to_include = c("Arkansas", "Connecticut", "Delaware", "District of Columbia", 
                          "Illinois", "Indiana", "Iowa", "Kentucky",  
                          "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                          "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", 
                          "Pennsylvania", "Rhode Island", "Tennessee", "Vermont", 
                          "Virginia", "West Virginia", "Wisconsin"),
    slavery_states = c("Arkansas", "Delaware", "Kentucky", 
                       "Maryland", "Missouri", "North Carolina", 
                       "Tennessee", "Virginia", "West Virginia", "District of Columbia")
  ),
  group3 = list(
    states_to_include = c("Alabama", "Connecticut", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Louisiana", 
                          "Maine", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                          "New Hampshire", "New Jersey", "New York", "Ohio", 
                          "Pennsylvania", "Rhode Island", "South Carolina", "Texas", "Vermont", 
                          "Wisconsin"),
    slavery_states = c("Alabama", "Florida", "Georgia", "Louisiana", 
                       "Mississippi", "South Carolina", "Texas")
  )
)

# Function to remove Texas from a list of states
remove_texas <- function(states) {
  states[states != "Texas"]
}

# Function to set up plot parameters
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, left_margin = 1.2, right_margin = 1.2) {
  width_pt <- width * 72
  height_pt <- height * 72
  par(pin = c(width - left_margin - right_margin, height - top_margin - bottom_margin))
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  par(family = "sans", cex = 1.2, cex.axis = 1.2, cex.lab = 1.2, tck = 0.01, lwd = 0.8, las = 1, mgp = c(3, 0.8, 0))
}

# Function to format labels
format_labels <- function(x) {
  gsub("-", "\uad", format(x, scientific = FALSE, trim = TRUE))
}

# Function to run the analysis
run_analysis <- function(group) {
  # Apply Texas exclusion if include_Texas is FALSE
  if (!include_Texas) {
    group$states_to_include <- remove_texas(group$states_to_include)
    group$slavery_states <- remove_texas(group$slavery_states)
  }
  
  # Filter data for included states
  data <- data %>% 
    filter(state %in% group$states_to_include)
  
  # Transform dependent variable
  data <- data %>%
    mutate(Yit = log(farmv_normalized)) %>%
    filter(!is.na(Yit) & is.finite(Yit))
  
  # Create variables
  data <- data %>%
    mutate(
      slavery = ifelse(state %in% group$slavery_states, 1, 0),
      post_1865 = ifelse(year > 1860, 1, 0),
      weight = improved + unimproved
    )
  
  # Use 1860 pc_enslaved for all subsequent years
  data <- data %>%
    group_by(GISJOIN) %>%
    mutate(pc_enslaved = ifelse(year <= 1860, pc_enslaved, pc_enslaved[year == 1860])) %>%
    ungroup()
  
  # 1. Basic DiD Model
  if (use_weights) {
    did_model <- lm(Yit ~ slavery + post_1865 + slavery:post_1865, data = data, weights = weight)
  } else {
    did_model <- lm(Yit ~ slavery + post_1865 + slavery:post_1865, data = data)
  }
  clustered_se <- coeftest(did_model, vcov = vcovCL(did_model, cluster = data$state))
  cat("\n--- 1. Basic DiD Model Results ---\n")
  print(clustered_se, digits = 3)
  cat("\nAdjusted R-squared:", summary(did_model)$adj.r.squared)
  cat("\nNumber of observations:", nobs(did_model), "\n")
  
  # 2. DiD model with pc_enslaved as covariate
  if (use_weights) {
    pc_enslaved_model <- lm(Yit ~ slavery + pc_enslaved + post_1865 + slavery:post_1865 + pc_enslaved:post_1865, 
                            data = data, weights = weight)
  } else {
    pc_enslaved_model <- lm(Yit ~ slavery + pc_enslaved + post_1865 + slavery:post_1865 + pc_enslaved:post_1865, 
                            data = data)
  }
  pc_enslaved_se <- coeftest(pc_enslaved_model, vcov = vcovCL(pc_enslaved_model, cluster = data$state))
  cat("\n--- 2. DiD Model with pc_enslaved Results ---\n")
  print(pc_enslaved_se, digits = 3)
  cat("\nAdjusted R-squared:", summary(pc_enslaved_model)$adj.r.squared)
  cat("\nNumber of observations:", nobs(pc_enslaved_model), "\n")
  
  # 3. Parallel Trends Test
  pre_trend_data <- data %>% filter(year <= 1860)
  if (use_weights) {
    pre_trend_model <- lm(Yit ~ slavery * year, data = pre_trend_data, weights = weight)
  } else {
    pre_trend_model <- lm(Yit ~ slavery * year, data = pre_trend_data)
  }
  pre_trend_test <- coeftest(pre_trend_model, vcov = vcovCL(pre_trend_model, cluster = pre_trend_data$state))
  cat("\n--- 3. Parallel Trends Test ---\n")
  print(pre_trend_test, digits = 3)
  cat("\nAdjusted R-squared:", summary(pre_trend_model)$adj.r.squared)
  cat("\nNumber of observations:", nobs(pre_trend_model), "\n")
  
  # 4. Trend-corrected DiD
  pre_trend_diff <- coef(pre_trend_model)["slavery:year"]
  data <- data %>%
    mutate(adjusted_Yit = ifelse(slavery == 1, 
                                 Yit - pre_trend_diff * (year - 1860), 
                                 Yit))
  if (use_weights) {
    trend_corrected_did <- lm(adjusted_Yit ~ slavery + post_1865 + slavery:post_1865, data = data, weights = weight)
  } else {
    trend_corrected_did <- lm(adjusted_Yit ~ slavery + post_1865 + slavery:post_1865, data = data)
  }
  trend_corrected_se <- coeftest(trend_corrected_did, vcov = vcovCL(trend_corrected_did, cluster = data$state))
  cat("\n--- 4. Trend-Corrected DiD Results ---\n")
  print(trend_corrected_se, digits = 3)
  cat("\nAdjusted R-squared:", summary(trend_corrected_did)$adj.r.squared)
  cat("\nNumber of observations:", nobs(trend_corrected_did), "\n")
  
  # 5. TWFE Model
  data$year_factor <- as.factor(data$year)
  twfe_formula <- "Yit ~ slavery:post_1865 | GISJOIN + year_factor"
  if (use_weights) {
    twfe_model <- feols(as.formula(twfe_formula), data = data, weights = ~ weight)
  } else {
    twfe_model <- feols(as.formula(twfe_formula), data = data)
  }
  clustered_summary <- summary(twfe_model, cluster = ~ state)
  cat("\n--- 5. TWFE Model Results ---\n")
  print(clustered_summary, digited = 3)
  cat("\nAdjusted R-squared:", r2(twfe_model, "ar2"))
  cat("\nNumber of observations:", nobs(twfe_model), "\n")
  
  
  # 6. TWFE Model with pc_enslaved
  data$year_factor <- as.factor(data$year)
  twfe_formula <- "Yit ~ slavery:post_1865 + pc_enslaved + pc_enslaved:post_1865| GISJOIN + year_factor"
  if (use_weights) {
    twfe_model <- feols(as.formula(twfe_formula), data = data, weights = ~ weight)
  } else {
    twfe_model <- feols(as.formula(twfe_formula), data = data)
  }
  clustered_summary <- summary(twfe_model, cluster = ~ state)
  cat("\n--- 6. TWFE Model Results with pc_enslaved ---\n")
  print(clustered_summary, digited = 3)
  cat("\nAdjusted R-squared:", r2(twfe_model, "ar2"))
  cat("\nNumber of observations:", nobs(twfe_model), "\n")
  
  # Plotting
  mean_data <- data %>%
    group_by(year, slavery) %>%
    summarize(mean_Yit = if (use_weights) {
      weighted.mean(Yit, w = weight, na.rm = TRUE)
    } else {
      mean(Yit, na.rm = TRUE)
    })
  
  mean_data <- as.data.frame(mean_data)
  
  plot_width <- 9.2  # inches
  plot_height <- 5.5  # inches
  
  setup_plot(plot_width, plot_height)
  
  year_range <- range(data$year)
  year_padding <- 2
  
  plot(mean_data$year[mean_data$slavery == 0], 
       mean_data$mean_Yit[mean_data$slavery == 0],
       type = "n",
       xlab = " ",
       ylab = "Log relative farm values per acre",
       xlim = c(year_range[1] - year_padding, year_range[2] + year_padding),
       ylim = c(2, 6),
       xaxs = "i",
       yaxs = "i",
       axes = FALSE)
  
  x_ticks <- seq(year_range[1], year_range[2], by = 10)
  
  axis(1, at = x_ticks, 
       labels = format_labels(x_ticks), 
       lwd = 0, lwd.ticks = 0.8, padj = -0.1)
  axis(2, at = seq(floor(min(mean_data$mean_Yit)), ceiling(max(mean_data$mean_Yit)), by = 1), 
       labels = format_labels(seq(floor(min(mean_data$mean_Yit)), ceiling(max(mean_data$mean_Yit)), by = 1)), 
       lwd = 0, lwd.ticks = 0.8, padj = 0.4)
  
  lines(mean_data$year[mean_data$slavery == 0], 
        mean_data$mean_Yit[mean_data$slavery == 0], 
        col = "black", lwd = 2)
  lines(mean_data$year[mean_data$slavery == 1], 
        mean_data$mean_Yit[mean_data$slavery == 1], 
        col = "black", lwd = 1)
  
  points(mean_data$year[mean_data$slavery == 0], 
         mean_data$mean_Yit[mean_data$slavery == 0], 
         pch = 21, bg = "black", col = "white", cex = 1.5, lwd = 5)
  points(mean_data$year[mean_data$slavery == 1], 
         mean_data$mean_Yit[mean_data$slavery == 1], 
         pch = 21, bg = "black", col = "white", cex = 1.5, lwd = 5)
  
  abline(v = 1860, lty = 2, col = "black", lwd = 0.5)
  
  box(lwd = 0.8)
  
  free_label <- "Free"
  south_label <- switch(as.character(sample_choice),
                        "1" = "Slave",
                        "2" = "Upper South",
                        "3" = "Deep South")
  
  # Dynamic label positioning
  free_y <- mean_data$mean_Yit[mean_data$year == 1870 & mean_data$slavery == 0] + 0.3
  south_y <- mean_data$mean_Yit[mean_data$year == 1870 & mean_data$slavery == 1] - 0.3
  
  text(1870, free_y, free_label, adj = c(0.5, 0.5), cex = 1.2)
  text(1870, south_y, south_label, adj = c(0.5, 0.5), cex = 1.2)
  
  # Generate file name based on chosen options
  file_name <- paste0(
    switch(as.character(sample_choice),
           "1" = "All_slave_states",
           "2" = "Upper_South",
           "3" = "Deep_South"),
    "_",
    if(use_weights) "weights" else "no_weights",
    "_",
    if(include_Texas) "Texas" else "no_Texas",
    ".pdf"
  )
  
  dev.copy(pdf, paste0("DiD/", file_name), width = plot_width, height = plot_height)
  dev.off()
}

# Print chosen options
cat("\nChosen options:\n")
cat("Sample: ", c("All states", "Upper South states", "Deep South states")[sample_choice], "\n")
cat("Using weights: ", if(use_weights) "Yes" else "No", "\n")
cat("Including Texas: ", if(include_Texas) "Yes" else "No", "\n\n")

# Run the analysis with chosen options
run_analysis(treatment_groups[[sample_choice]])