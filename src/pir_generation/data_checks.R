
# Evaluate the consistency of the ICP Forests solid soil data
# and create "Partner Inconsistency Reports" (PIRs)

# Prepare packages ----

# Update the CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Define required packages
stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"))

# Place the downloaded zipped raw data files on Google Drive
# (in folder "./data/raw_data/download_[download_date]/")

# Save the raw data locally ----

source("./src/functions/sync_local_data.R")
sync_local_data(list_subfolders_data = "raw_data",
                list_subfolders_output = FALSE)

# Specify date on which 'layer 0' data were downloaded ----
# from ICP Forests website

source("./src/functions/get_date_local.R")
download_date <- get_date_local(path = "./data/raw_data/",
                                save_to_env = TRUE,
                                collapsed = TRUE)
download_date_pir <- as.Date(parsedate::parse_iso_8601(download_date))

# Import data ----
# First read "y1" and "si", as their coordinates are used to list coordinates
# of "s1" and "so"

source("./src/functions/read_raw.R")
read_raw("y1", save_to_env = TRUE)
read_raw("si", save_to_env = TRUE)
read_raw("s1", save_to_env = TRUE)
read_raw("so", save_to_env = TRUE)
read_raw("sw", save_to_env = TRUE)



# Merge duplicate records in "so_som" ----

source("./src/functions/merge_duplicate_records.R")
merge_duplicate_records("so_som",
                        merge = TRUE,
                        save_to_env = TRUE)


# Get inconsistencies in primary keys (survey_year, code_layer) ----

  # The argument "solve = TRUE" tells
  # the function to rename "code_layer" in "s1_som"
  # in case of ambiguous (non-unique) code_layers

source("./src/functions/get_primary_inconsistencies.R")
get_primary_inconsistencies("y1", save_to_env = TRUE)
get_primary_inconsistencies("s1", solve = TRUE, save_to_env = TRUE)
get_primary_inconsistencies("si", save_to_env = TRUE)
get_primary_inconsistencies("so", solve = TRUE, save_to_env = TRUE)
get_primary_inconsistencies("sw", save_to_env = TRUE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_primary_inconsistencies", save_to_env = TRUE)
View(list_primary_inconsistencies)




# Get inconsistencies in coordinates ----

  # Retrieve surveys which have coordinates


# "y1_pl1" "s1_pls" "s1_prf" "si_plt" "so_pls" "so_prf"


  # Get inconsistencies

source("./src/functions/get_coordinate_inconsistencies.R")

get_coordinate_inconsistencies(boundary_buffer_meter = 3000,
                               save_to_env = TRUE)
View(list_coordinate_inconsistencies)




# Get inconsistencies in soil layers? ----
# "solve" indicates whether the obvious mistakes can be solved

source("./src/functions/get_layer_inconsistencies.R")
get_layer_inconsistencies("so_som", solve = TRUE, save_to_env = TRUE)
get_layer_inconsistencies("s1_som", solve = TRUE, save_to_env = TRUE)
get_layer_inconsistencies("so_pfh", solve = TRUE, save_to_env = TRUE)
get_layer_inconsistencies("s1_pfh", solve = TRUE, save_to_env = TRUE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_layer_inconsistencies", save_to_env = TRUE)
View(list_layer_inconsistencies)




# Get inconsistencies in presence and range of data ----
# "solve = TRUE" converts data in the wrong units to the correct units

source("./src/functions/get_range_inconsistencies.R")

get_range_inconsistencies("s1_som", solve = TRUE, save_to_env = TRUE)
get_range_inconsistencies("s1_pfh", solve = TRUE, save_to_env = TRUE)
get_range_inconsistencies("so_som", solve = TRUE, save_to_env = TRUE)
get_range_inconsistencies("so_pfh", solve = TRUE, save_to_env = TRUE)
get_range_inconsistencies("sw_swc", solve = TRUE, save_to_env = TRUE)
get_range_inconsistencies("so_prf", save_to_env = TRUE)
get_range_inconsistencies("s1_prf", save_to_env = TRUE)
get_range_inconsistencies("so_pls", save_to_env = TRUE)
get_range_inconsistencies("s1_pls", save_to_env = TRUE)
get_range_inconsistencies("si_sta", save_to_env = TRUE)
get_range_inconsistencies("y1_st1", save_to_env = TRUE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_range_inconsistencies", save_to_env = TRUE)
View(list_range_inconsistencies)



# Get inconsistencies in derived variables ----

source("./src/functions/get_derived_variable_inconsistencies.R")

get_derived_variable_inconsistencies("so_som", save_to_env = TRUE)
get_derived_variable_inconsistencies("so_pfh", save_to_env = TRUE)
get_derived_variable_inconsistencies("s1_som", save_to_env = TRUE)
get_derived_variable_inconsistencies("s1_pfh", save_to_env = TRUE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_derived_inconsistencies", save_to_env = TRUE)
View(list_derived_inconsistencies)




# Export PIR as Excel ----

  # Bind lists with inconsistencies
inconsistency_report <- rbind(list_primary_inconsistencies,
                              list_coordinate_inconsistencies,
                              list_layer_inconsistencies,
                              list_range_inconsistencies,
                              list_derived_inconsistencies)

  # Save a copy with the download_date attached to the object name
object_name <- paste0("inconsistency_report_",
                      download_date)

assign(object_name,
       inconsistency_report,
       envir = globalenv())

  # Export the PIR

  # Set ignore_checked_inconsistencies to FALSE
  # to take inconsistencies which were previously confirmed by partners
  # into account

source("./src/functions/export_pir.R")
export_pir("inconsistency_report", per_partner = TRUE, per_level = TRUE,
           ignore_checked_inconsistencies = TRUE)


# Export the processed survey forms ----

source("./src/functions/save_to_google_drive.R")
save_to_google_drive(path_name = "0_01")

# Sync local data

source("./src/functions/sync_local_data.R")
sync_local_data()

# Import the processed survey forms ----

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)


