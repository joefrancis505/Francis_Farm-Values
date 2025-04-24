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

# Read the CSV file
data <- read.csv("Event_study/Event_study_panel_data.csv")

# Filter for years 1850 onward
data <- data %>% filter(year >= 1850)

# Full South sample definition
full_south <- list(
  states_to_include = c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", 
                        "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kentucky", "Louisiana", 
                        "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                        "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", 
                        "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", 
                        "Virginia", "West Virginia", "Wisconsin"),
  slavery_states = c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky", "Louisiana", 
                     "Maryland", "Mississippi", "Missouri", "North Carolina", "South Carolina", 
                     "Tennessee", "Texas", "Virginia", "West Virginia", "District of Columbia")
)

# Function to set up plot parameters
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, left_margin = 1.2, right_margin = 1.2) {
  width_pt <- width * 72
  height_pt <- height * 72
  par(pin = c(width - left_margin - right_margin, height - top_margin - bottom_margin))
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  par(family = "sans", cex = 1.2, cex.axis = 1.2, cex.lab = 1.2, tck = 0.01, lwd = 0.8, las = 1, mgp = c(3.5, 0.8, 0))
}

# Function to format labels
format_labels <- function(x) {
  gsub("-", "\uad", format(x, scientific = FALSE, trim = TRUE))
}

# Function to replace beta with the Greek symbol
replace_beta <- function(text) {
  gsub("beta", "\u03B2", text, fixed = TRUE)
}

# Function to run the analysis
run_analysis <- function(group, group_name) {
  # Filter data for included states
  data <- data %>% 
    filter(state %in% group$states_to_include)
  
  # Use farmv_na as the dependent variable
  data <- data %>%
    mutate(farmv = log(farmv_na)) %>%
    filter(!is.na(farmv) & is.finite(farmv))
  
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
  
  # Define years to include
  years_to_include <- c(1850, 1860, 1870, 1880, 1890, 1900)
  
  # Event Study Model
  event_study_data <- data %>%
    filter(year %in% years_to_include) %>%
    mutate(event_time = year - 1860)
  event_study_data <- event_study_data %>%
    mutate(cotton_pc = cotton_pc / 400)
  
  # Event Study Model (Model 1: slavery only)
  event_study_formula <- "farmv ~ i(year, slavery, ref = 1860) | GISJOIN + year"

  event_study_model <- feols(as.formula(event_study_formula), data = event_study_data, weights = ~ weight)
  
  clustered_summary <- summary(event_study_model, cluster = ~ state)
  cat("\n--- 1. Event Study Model Results (slavery only) for", group_name, "---\n")
  print(clustered_summary, digits = 3)
  cat("\nAdjusted R-squared:", r2(event_study_model, "ar2"))
  cat("\nNumber of observations:", nobs(event_study_model), "\n")
  
  # Event Study Model with pc_enslaved only (Model 2)
  event_study_formula_enslaved <- "farmv ~ i(year, pc_enslaved, ref = 1860) | GISJOIN + year"

  event_study_model_enslaved <- feols(as.formula(event_study_formula_enslaved), data = event_study_data, weights = ~ weight)
  
  clustered_summary_enslaved <- summary(event_study_model_enslaved, cluster = ~ state)
  cat("\n--- 2. Event Study Model Results (pc_enslaved only) for", group_name, "---\n")
  print(clustered_summary_enslaved, digits = 3)
  cat("\nAdjusted R-squared:", r2(event_study_model_enslaved, "ar2"))
  cat("\nNumber of observations:", nobs(event_study_model_enslaved), "\n")
  
  # Event Study Model with both slavery and pc_enslaved (Model 3)
  event_study_formula_both <- "farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) | GISJOIN + year"

  event_study_model_both <- feols(as.formula(event_study_formula_both), data = event_study_data, weights = ~ weight)
  
  clustered_summary_both <- summary(event_study_model_both, cluster = ~ state)
  cat("\n--- 3. Event Study Model Results (both slavery and pc_enslaved) for", group_name, "---\n")
  print(clustered_summary_both, digits = 3)
  cat("\nAdjusted R-squared:", r2(event_study_model_both, "ar2"))
  cat("\nNumber of observations:", nobs(event_study_model_both), "\n")
  
  # Event Study Model with slavery, pc_enslaved, and cotton_pc (Model 4)
  event_study_formula_cotton <- "farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) + i(year, cotton_pc, ref = 1860) | GISJOIN + year"

  event_study_model_cotton <- feols(as.formula(event_study_formula_cotton), data = event_study_data, weights = ~ weight)
  
  clustered_summary_cotton <- summary(event_study_model_cotton, cluster = ~ state)
  cat("\n--- 4. Event Study Model Results (slavery, pc_enslaved, and cotton_pc) for", group_name, "---\n")
  print(clustered_summary_cotton, digits = 3)
  cat("\nAdjusted R-squared:", r2(event_study_model_cotton, "ar2"))
  cat("\nNumber of observations:", nobs(event_study_model_cotton), "\n")
  
  # Event Study Model with slavery, pc_enslaved, and ccratio (Model 5)
  event_study_formula_ccratio <- "farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) + i(year, ccratio, ref = 1860) | GISJOIN + year"

  event_study_model_ccratio <- feols(as.formula(event_study_formula_ccratio), data = event_study_data, weights = ~ weight)
  
  clustered_summary_ccratio <- summary(event_study_model_ccratio, cluster = ~ state)
  cat("\n--- 5. Event Study Model Results (slavery, pc_enslaved, and ccratio) for", group_name, "---\n")
  print(clustered_summary_ccratio, digits = 3)
  cat("\nAdjusted R-squared:", r2(event_study_model_ccratio, "ar2"))
  cat("\nNumber of observations:", nobs(event_study_model_ccratio), "\n")
  
  # Fix cotton_pc and ccratio at 1860 levels for 1870 onwards
  event_study_data <- event_study_data %>%
    group_by(GISJOIN) %>%
    mutate(
      cotton_pc_fixed = ifelse(year <= 1860, cotton_pc, cotton_pc[year == 1860]) * 400,
      ccratio_fixed = ifelse(year <= 1860, ccratio, ccratio[year == 1860])
    ) %>%
    ungroup()
  
  # Event Study Model with slavery, pc_enslaved, and fixed cotton_pc (Model 6)
  event_study_formula_cotton_fixed <- "farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) + i(year, cotton_pc_fixed, ref = 1860) | GISJOIN + year"

  event_study_model_cotton_fixed <- feols(as.formula(event_study_formula_cotton_fixed), data = event_study_data, weights = ~ weight)
  
  clustered_summary_cotton_fixed <- summary(event_study_model_cotton_fixed, cluster = ~ state)
  cat("\n--- 6. Event Study Model Results (slavery, pc_enslaved, and fixed cotton_pc) for", group_name, "---\n")
  print(clustered_summary_cotton_fixed, digits = 3)
  cat("\nAdjusted R-squared:", r2(event_study_model_cotton_fixed, "ar2"))
  cat("\nNumber of observations:", nobs(event_study_model_cotton_fixed), "\n")
  
  # Event Study Model with slavery, pc_enslaved, and fixed ccratio (Model 7)
  event_study_formula_ccratio_fixed <- "farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) + i(year, ccratio_fixed, ref = 1860) | GISJOIN + year"

  event_study_model_ccratio_fixed <- feols(as.formula(event_study_formula_ccratio_fixed), data = event_study_data, weights = ~ weight)
  
  clustered_summary_ccratio_fixed <- summary(event_study_model_ccratio_fixed, cluster = ~ state)
  cat("\n--- 7. Event Study Model Results (slavery, pc_enslaved, and fixed ccratio) for", group_name, "---\n")
  print(clustered_summary_ccratio_fixed, digits = 3)
  cat("\nAdjusted R-squared:", r2(event_study_model_ccratio_fixed, "ar2"))
  cat("\nNumber of observations:", nobs(event_study_model_ccratio_fixed), "\n")
  
  
  # Event Study Plot for slavery (using Model 1)
  tryCatch({
    event_study_coef <- coef(event_study_model)
    event_study_se <- sqrt(diag(vcov(event_study_model, cluster = "state")))
    event_times <- as.numeric(gsub("year::([0-9]+):slavery", "\\1", names(event_study_coef)))
    event_times <- event_times[!is.na(event_times)]
    
    clean_data <- function(x, y) {
      valid <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
      list(x = x[valid], y = y[valid])
    }
    
    clean <- clean_data(event_times, event_study_coef[!is.na(event_times)])
    event_times <- clean$x
    event_study_coef <- clean$y
    event_study_se <- event_study_se[!is.na(event_times)]
    
    if(length(event_times) > 0) {
      plot_width <- 9.2  # inches
      plot_height <- 5.5  # inches
      setup_plot(plot_width, plot_height)
      
      year_range <- range(event_times, na.rm = TRUE)
      year_padding <- 2
      
      plot(event_times, event_study_coef, 
           type = "n", 
           xlab = " ", 
           ylab = "Coefficient",
           xlim = c(year_range[1] - year_padding, year_range[2] + year_padding),
           ylim = c(-1.5,0.5),
           xaxs = "i", yaxs = "i",
           axes = FALSE)
      
      x_ticks <- seq(1850, 1900, by = 10)
      axis(1, at = x_ticks, 
           labels = format_labels(x_ticks), 
           lwd = 0, lwd.ticks = 0.8, padj = -0.1)
      y_ticks <- seq(-1.5, 0.5, by = 0.5)
      y_labels <- format(y_ticks, scientific = FALSE, trim = TRUE)
      y_labels <- gsub("-", "\uad", y_labels)
      axis(2, at = y_ticks, labels = y_labels, lwd = 0, lwd.ticks = 0.8, padj = 0.4)
      
      abline(h = 0, lty = 2, col = "gray")
      abline(v = 1860, lty = 2, col = "gray")
      
      arrows(event_times, event_study_coef - 1.96 * event_study_se,
             event_times, event_study_coef + 1.96 * event_study_se,
             length = 0.05, angle = 90, code = 3, col = "black", lwd = 0.8)
      
      points(event_times, event_study_coef, pch = 21, bg = "black", col = "white", cex = 1.5, lwd = 5)
      points(1860, 0, pch = 21, bg = "black", col = "white", cex = 1.5, lwd = 5)
      
      box(lwd = 0.8)
      
      file_name <- "1.pdf"
      dev.copy(pdf, paste0("Event_study/event_study_", file_name), width = plot_width, height = plot_height)
      dev.off()
    } else {
      warning("Insufficient data for Event Study plot")
    }
  }, error = function(e) {
    warning("Error in Event Study plot: ", e$message)
  })
  
  # Event Study Plot for pc_enslaved (using Model 2)
  tryCatch({
    event_study_coef <- coef(event_study_model_enslaved)
    event_study_se <- sqrt(diag(vcov(event_study_model_enslaved, cluster = "state")))
    
    # Extract coefficients and standard errors for pc_enslaved
    pc_enslaved_coef <- event_study_coef[grep("year::[0-9]+:pc_enslaved", names(event_study_coef))]
    pc_enslaved_se <- event_study_se[grep("year::[0-9]+:pc_enslaved", names(event_study_coef))]
    event_times <- as.numeric(gsub("year::([0-9]+):pc_enslaved", "\\1", names(pc_enslaved_coef)))
    
    clean_data <- function(x, y) {
      valid <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
      list(x = x[valid], y = y[valid])
    }
    
    clean <- clean_data(event_times, pc_enslaved_coef)
    event_times <- clean$x
    pc_enslaved_coef <- clean$y
    pc_enslaved_se <- pc_enslaved_se[!is.na(event_times)]
    
    if(length(event_times) > 0) {
      plot_width <- 9.2  # inches
      plot_height <- 5.5  # inches
      setup_plot(plot_width, plot_height)
      
      year_range <- range(event_times, na.rm = TRUE)
      year_padding <- 2
      
      plot(event_times, pc_enslaved_coef, 
           type = "n", 
           xlab = " ", 
           ylab = "Coefficient",
           xlim = c(year_range[1] - year_padding, year_range[2] + year_padding),
           ylim = c(-0.03, 0.01),
           xaxs = "i", yaxs = "i",
           axes = FALSE)
      
      x_ticks <- seq(1850, 1900, by = 10)
      axis(1, at = x_ticks, 
           labels = format_labels(x_ticks), 
           lwd = 0, lwd.ticks = 0.8, padj = -0.1)
      y_ticks <- seq(-0.03, 0.01, by = 0.01)
      y_labels <- sprintf("%.2f", y_ticks)
      y_labels <- gsub("-", "\uad", y_labels)
      axis(2, at = y_ticks, 
           labels = y_labels,
           lwd = 0, lwd.ticks = 0.8, padj = 0.4)
      
      abline(h = 0, lty = 2, col = "gray")
      abline(v = 1860, lty = 2, col = "gray")
      
      arrows(event_times, pc_enslaved_coef - 1.96 * pc_enslaved_se,
             event_times, pc_enslaved_coef + 1.96 * pc_enslaved_se,
             length = 0.05, angle = 90, code = 3, col = "black", lwd = 0.8)
      
      points(event_times, pc_enslaved_coef, pch = 21, bg = "black", col = "white", cex = 1.5, lwd = 5)
      points(1860, 0, pch = 21, bg = "black", col = "white", cex = 1.5, lwd = 5)
      
      box(lwd = 0.8)
      
      file_name <- "2.pdf"
      dev.copy(pdf, paste0("Event_study/event_study_", file_name), width = plot_width, height = plot_height)
      dev.off()
    } else {
      warning("Insufficient data for pc_enslaved Event Study plot")
    }
  }, error = function(e) {
    warning("Error in pc_enslaved Event Study plot: ", e$message)
  })
}

# Redirect output to file
sink("Event_study/Results.txt")

# Run the analysis for the full South sample
run_analysis(full_south, "Full South")

# Close the output file
sink()