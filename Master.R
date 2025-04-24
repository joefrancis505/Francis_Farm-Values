# Set the working directory to the script’s location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Clear console
cat("\014")

source("Download_IPUMS.R")
source("RDD_database_v3.R")
source("Event_study_database_v3.R")
source("Event_study_normalization_v2.R")
source("Figure_1.R")
source("Event_study_v4.R")
source("RDD_v4.R")