setwd(getSrcDirectory(function(dummy) {dummy}))
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load packages
packages <- c("dplyr", "fixest")
install.packages(packages[!packages %in% rownames(installed.packages())])
invisible(lapply(packages, library, character.only = TRUE))

# Read and prepare data
data <- read.csv("Event_study/Event_study_panel_data.csv") %>%
  filter(year >= 1850)

# Define sample
states_to_include <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia", 
                       "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kentucky", "Louisiana", 
                       "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                       "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", 
                       "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", 
                       "Virginia", "West Virginia", "Wisconsin")

slavery_states <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky", "Louisiana", 
                    "Maryland", "Mississippi", "Missouri", "North Carolina", "South Carolina", 
                    "Tennessee", "Texas", "Virginia", "West Virginia", "District of Columbia")

# Filter and prepare data
data <- data %>% 
  filter(state %in% states_to_include) %>%
  mutate(
    farmv = log(farmv_na),
    slavery = ifelse(state %in% slavery_states, 1, 0),
    weight = improved + unimproved,
    cotton_pc = cotton_pc / 400
  ) %>%
  filter(!is.na(farmv) & is.finite(farmv))

# Fix pc_enslaved at 1860 levels for post-1860 years
data <- data %>%
  group_by(GISJOIN) %>%
  mutate(pc_enslaved = ifelse(year <= 1860, pc_enslaved, pc_enslaved[year == 1860])) %>%
  ungroup()

# Define years and create event study data
years_to_include <- c(1850, 1860, 1870, 1880, 1890, 1900)
event_study_data <- data %>%
  filter(year %in% years_to_include) %>%
  mutate(event_time = year - 1860)

# Fix cotton and corn ratios at 1860 levels for post-1860 years
event_study_data <- event_study_data %>%
  group_by(GISJOIN) %>%
  mutate(
    cotton_pc_fixed = ifelse(year <= 1860, cotton_pc, cotton_pc[year == 1860]) * 400,
    ccratio_fixed = ifelse(year <= 1860, ccratio, ccratio[year == 1860])
  ) %>%
  ungroup()

# Setup plot parameters
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, left_margin = 1.2, right_margin = 1.2) {
  par(pin = c(width - left_margin - right_margin, height - top_margin - bottom_margin))
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  par(family = "sans", cex = 1.2, cex.axis = 1.2, cex.lab = 1.2, tck = 0.01, lwd = 0.8, las = 1, mgp = c(3.5, 0.8, 0))
}

format_labels <- function(x) {
  gsub("-", "\uad", format(x, scientific = FALSE, trim = TRUE))
}

# Run event study regressions
models <- list(
  slavery = feols(farmv ~ i(year, slavery, ref = 1860) | GISJOIN + year, 
                  data = event_study_data, weights = ~ weight),
  
  pc_enslaved = feols(farmv ~ i(year, pc_enslaved, ref = 1860) | GISJOIN + year, 
                      data = event_study_data, weights = ~ weight),
  
  both = feols(farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) | GISJOIN + year, 
               data = event_study_data, weights = ~ weight),
  
  cotton = feols(farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) + i(year, cotton_pc, ref = 1860) | GISJOIN + year, 
                 data = event_study_data, weights = ~ weight),
  
  ccratio = feols(farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) + i(year, ccratio, ref = 1860) | GISJOIN + year, 
                  data = event_study_data, weights = ~ weight),
  
  cotton_fixed = feols(farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) + i(year, cotton_pc_fixed, ref = 1860) | GISJOIN + year, 
                       data = event_study_data, weights = ~ weight),
  
  ccratio_fixed = feols(farmv ~ i(year, slavery, ref = 1860) + i(year, pc_enslaved, ref = 1860) + i(year, ccratio_fixed, ref = 1860) | GISJOIN + year, 
                        data = event_study_data, weights = ~ weight)
)

# Print results
sink("Event_study/Results.txt")
for (i in seq_along(models)) {
  cat("\n", rep("=", 50), "\n")
  cat("Model", i, ":", names(models)[i], "\n")
  cat(rep("=", 50), "\n")
  print(summary(models[[i]], cluster = ~ state), digits = 3)
  cat("\nAdjusted R-squared:", r2(models[[i]], "ar2"))
  cat("\nObservations:", nobs(models[[i]]), "\n")
}
sink()

# Create plots for main models
create_event_plot <- function(model, coef_pattern, title, ylim_range, filename) {
  event_study_coef <- coef(model)
  event_study_se <- sqrt(diag(vcov(model, cluster = "state")))
  
  coef_indices <- grep(coef_pattern, names(event_study_coef))
  if (length(coef_indices) == 0) return()
  
  selected_coef <- event_study_coef[coef_indices]
  selected_se <- event_study_se[coef_indices]
  
  event_times <- as.numeric(gsub(".*::([0-9]+):.*", "\\1", names(selected_coef)))
  
  valid_idx <- !is.na(event_times) & !is.na(selected_coef) & is.finite(selected_coef)
  event_times <- event_times[valid_idx]
  selected_coef <- selected_coef[valid_idx]
  selected_se <- selected_se[valid_idx]
  
  if (length(event_times) == 0) return()
  
  plot_width <- 9.2
  plot_height <- 5.5
  
  pdf(paste0("Event_study/", filename), width = plot_width, height = plot_height)
  setup_plot(plot_width, plot_height)
  
  year_range <- range(event_times, na.rm = TRUE)
  year_padding <- 2
  
  plot(event_times, selected_coef, 
       type = "n", 
       xlab = " ", 
       ylab = "Coefficient",
       xlim = c(year_range[1] - year_padding, year_range[2] + year_padding),
       ylim = ylim_range,
       xaxs = "i", yaxs = "i",
       axes = FALSE)
  
  x_ticks <- seq(1850, 1900, by = 10)
  axis(1, at = x_ticks, labels = format_labels(x_ticks), lwd = 0, lwd.ticks = 0.8, padj = -0.1)
  
  y_ticks <- pretty(ylim_range, n = 6)
  y_labels <- format_labels(y_ticks)
  axis(2, at = y_ticks, labels = y_labels, lwd = 0, lwd.ticks = 0.8, padj = 0.4)
  
  abline(h = 0, lty = 2, col = "gray")
  abline(v = 1860, lty = 2, col = "gray")
  
  arrows(event_times, selected_coef - 1.96 * selected_se,
         event_times, selected_coef + 1.96 * selected_se,
         length = 0.05, angle = 90, code = 3, col = "black", lwd = 0.8)
  
  points(event_times, selected_coef, pch = 21, bg = "black", col = "white", cex = 1.5, lwd = 5)
  points(1860, 0, pch = 21, bg = "black", col = "white", cex = 1.5, lwd = 5)
  
  box(lwd = 0.8)
  dev.off()
}

# Generate plots
create_event_plot(models$slavery, "year::[0-9]+:slavery", "Slavery Effect", c(-1.5, 0.5), "event_study_1.pdf")
create_event_plot(models$pc_enslaved, "year::[0-9]+:pc_enslaved", "Enslaved Population Effect", c(-0.03, 0.01), "event_study_2.pdf")