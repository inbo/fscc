
# Transformation of "layer 1" ICP Forests solid soil data to obtain "layer 1+"
# ---------------------------------------------------------------------------

# Script initiation date: 22 March 2024

# Details: In this script, corrected and internally gap-filled "layer 1" data
# are processed to produce "layer 1+", which basically further gap-fills the
# "layer 1" based on rougher predictions (e.g. mainly pedotransfer functions).


# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O

# Attention: in order to be able to run the script, a number of
# additional data forms (which are not part of this GitHub repository
# or the raw "layer 0" data) are required.
# These can be requested from FSCC.

# Status:
# - Almost complete for Level II (so) (complete for variables required
#   for carbon stock calculations)
# - Almost complete for Level I (s1) (complete for variables required
#   for carbon stock calculations)



# 1. Prepare packages ----

# Define required packages
stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"),
          require("aqp"))


# 2. Import 'layer 1" data ----

source("./src/functions/read_processed.R")
read_processed(path_name = "./data/layer1_data/",
               save_to_env = TRUE)



# Input level ----

level <- "LI"
level <- "LII"



# 3. Gap-fill bulk densities for mineral layers ----

source("./src/functions/gapfill_with_predictions.R")

if (level == "LI") {

  s1_som <- gapfill_with_predictions(survey_form = "s1_som",
                                     data_frame = s1_som,
                                     save_to_env = FALSE)

  s1_pfh <- gapfill_with_predictions(survey_form = "s1_pfh",
                                     data_frame = s1_pfh,
                                     save_to_env = FALSE)
}


if (level == "LII") {

  so_som <- gapfill_with_predictions(survey_form = "so_som",
                                     data_frame = so_som,
                                     save_to_env = FALSE)

  so_pfh <- gapfill_with_predictions(survey_form = "so_pfh",
                                     data_frame = so_pfh,
                                     save_to_env = FALSE)
}




# 4. Tidy up dataframes ----

source("./src/functions/tidy.R")

if (level == "LI") {

  s1_som <- tidy(survey_form = "s1_som",
                 data_frame = s1_som,
                 save_to_env = FALSE)

  s1_pfh <- tidy(survey_form = "s1_pfh",
                 data_frame = s1_pfh,
                 save_to_env = FALSE)
}


if (level == "LII") {

  so_som <- tidy(survey_form = "so_som",
                 data_frame = so_som,
                 save_to_env = FALSE)

  so_pfh <- tidy(survey_form = "so_pfh",
                 data_frame = so_pfh,
                 save_to_env = FALSE)
}




# 5. Get stratifiers ----

source("./src/functions/get_stratifiers.R")

if (level == "LI") {

  s1_strat <- get_stratifiers(level = "LI")

  write.table(s1_strat,
              file = paste0("./data/additional_data/s1_strat.csv"),
              row.names = FALSE,
              na = "",
              sep = ";",
              dec = ".")
}

if (level == "LII") {

  so_strat <- get_stratifiers(level = "LII")

  write.table(so_strat,
              file = paste0("./data/additional_data/so_strat.csv"),
              row.names = FALSE,
              na = "",
              sep = ";",
              dec = ".")
}





# 5. Export the processed survey forms ----

## 5.1. Save processed survey forms to Google Drive (layer 1) ----

source("./src/functions/save_to_google_drive.R")
save_to_google_drive(objects_to_save = c("si", "so", "sw"),
                     path_name = "layer1_data")

source("./src/functions/save_to_google_drive.R")
save_to_google_drive(objects_to_save = c("y1", "s1"),
                     path_name = "layer1_data")

source("./src/functions/save_to_google_drive.R")
save_to_google_drive(path_name = "layer1_data")



## 5.2. Sync local data with Google Drive ----

source("./src/functions/sync_local_data.R")
sync_local_data(list_subfolders_data = "layer1_data",
                list_subfolders_output = FALSE)








