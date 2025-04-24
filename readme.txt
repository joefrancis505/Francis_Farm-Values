This repository contains data and scripts for the paper "Did Slavery Impede the Growth of American Capitalism? Two Natural Experiments Using Farm Values per Acre" by Joseph Francis. It contains the following files and folders:

The Master.R script will run the whole replication and supplementary analysis package, although you will need an IPUMS API key from https://account.ipums.org/api_keys.

/Data
The raw data used in the various scripts.

Download_IPUMS.R
This downloads the necessary files from IPUMS.

/Event_study
The results of the Event_study scripts.

Event_study_database_v3.R
The script that compiles /Event_study/Event_study_database.csv. 
Event_study_normalization_v2.R
The script that processes /Event_study/Event_study_database.csv to make /Event_study/Event_study_panel_data.csv, in which the data are normalized to the 1900 county map. This script will take some time to run. If you want to speed it up, you can adjust this in line 70: "rasterize_data <- function(sf_object, resolution = 200)". If you put a higher resolution, it will run faster, although the accuracy of the normalized data will be reduced.
Event_study_v4.R
The script that runs the event studying using /Event_study/Event_study_panel_data.csv, with the results shown in the console and saved as the two pdf figures in /Event_study.
Figure_1.pdf
Figure 1 from the paper, which shows farm values per acre in the Upper South and the Deep South as percentages of the national average.
Figure_1.R
The script that makes Figure 1, using /Event_study/Event_study_panel_data.csv.
Francis_Farm_Values.pdf
The working paper.
/RDD
The results of the RDD scripts. The /CSV folder contains the results of each spatial RDD, as well as robustness checks for each year. The robustness checks consist of a border-wide RDD using the various different settings from the rdrobust package and with all the different specifications described in the working paper. The /Geopackage folder contains the results of each spatial RDD as files that can be used in GIS software.
RDD_database_v3.R
The script that creates the database /RDD/RDD_database.csv.
RDD_v4.R
The scripts that runs the spatial RDD.
