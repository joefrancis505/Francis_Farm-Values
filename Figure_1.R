# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Load required libraries
library(dplyr)

# Read the CSV file
data <- read.csv("Event_study/Event_study_panel_data.csv")

# Define slavery states for each group
slavery_states <- list(
  upper_south = c("Arkansas", "Delaware", "Kentucky", 
                  "Maryland", "Missouri", "North Carolina", 
                  "Tennessee", "Virginia", "West Virginia", "District of Columbia"),
  deep_south = c("Alabama", "Florida", "Georgia", "Louisiana", 
                 "Mississippi", "South Carolina", "Texas")
)

# Calculate national average farm value per acre for each year
national_avg <- data %>%
  filter(!state %in% c("Hawaii Territory", "Alaska Territory")) %>%
  group_by(year) %>%
  summarize(nat_avg = weighted.mean(farmv_na, w = improved + unimproved, na.rm = TRUE))

# Function to process data for a group
process_group_data <- function(states, group_name) {
  group_data <- data %>%
    filter(state %in% states) %>%
    left_join(national_avg, by = "year") %>%
    mutate(percent_of_national = (farmv_na / nat_avg) * 100) %>%
    group_by(year) %>%
    summarize(mean_percent = weighted.mean(percent_of_national, w = improved + unimproved, na.rm = TRUE)) %>%
    mutate(region = group_name)
  
  return(group_data)
}

# Process data for each group
upper_south_data <- process_group_data(slavery_states$upper_south, "Upper South")
deep_south_data <- process_group_data(slavery_states$deep_south, "Deep South")

# Combine Upper and Deep South data
combined_south_data <- bind_rows(upper_south_data, deep_south_data)

# Function to set up plot parameters (from the longer script)
setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6, left_margin = 1.2, right_margin = 1.2) {
  width_pt <- width * 72
  height_pt <- height * 72
  par(pin = c(width - left_margin - right_margin, height - top_margin - bottom_margin))
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  par(family = "sans", cex = 1.2, cex.axis = 1.2, cex.lab = 1.2, tck = 0.01, lwd = 0.8, las = 1, mgp = c(3.5, 0.8, 0))
}

# Function to format labels (from the longer script)
format_labels <- function(x) {
  gsub("-", "\uad", format(x, scientific = FALSE, trim = TRUE))
}

# Function to create and save combined plot
create_combined_plot <- function(combined_data) {
  plot_width <- 9.2  # inches
  plot_height <- 5.5  # inches
  
  plot_function <- function() {
    setup_plot(plot_width, plot_height)
    
    year_range <- range(combined_data$year)
    year_padding <- 2
    
    plot(range(combined_data$year), range(combined_data$mean_percent), 
         type = "n",
         xlab = " ",
         ylab = "% of national average",
         xlim = c(year_range[1] - year_padding, year_range[2] + year_padding),
         ylim = c(0, 100),
         xaxs = "i",
         yaxs = "i",
         axes = FALSE)
    
    x_ticks <- seq(year_range[1], year_range[2], by = 10)
    
    axis(1, at = x_ticks, 
         labels = format_labels(x_ticks), 
         lwd = 0, lwd.ticks = 0.8, padj = -0.1)
    axis(2, at = seq(0, 100, by = 20), 
         labels = format_labels(seq(0, 100, by = 20)), 
         lwd = 0, lwd.ticks = 0.8, padj = 0.4)
    
    # Plot Upper South
    upper_south <- combined_data %>% filter(region == "Upper South")
    lines(upper_south$year, upper_south$mean_percent, col = "black", lwd = 2)
    
    # Plot Deep South
    deep_south <- combined_data %>% filter(region == "Deep South")
    lines(deep_south$year, deep_south$mean_percent, col = "black", lwd = 1)
    
    box(lwd = 0.8)
    
    # Add labels
    upper_y <- max(upper_south$mean_percent[upper_south$year == 1860]) + 8
    deep_y <- min(deep_south$mean_percent[deep_south$year == 1870]) - 8
    
    text(1860, upper_y, "Upper South", adj = c(0.5, 0.5), cex = 1.2)
    text(1870, deep_y, "Deep South", adj = c(0.5, 0.5), cex = 1.2)
  }
  
  # Display plot in console
  plot_function()
  
  # Save plot as PDF
  pdf("Figure_1.pdf", width = plot_width, height = plot_height)
  plot_function()
  dev.off()
}

# Create and save only the combined plot
create_combined_plot(combined_south_data)
print("The combined plot has been created and saved as a PDF in the working directory.")