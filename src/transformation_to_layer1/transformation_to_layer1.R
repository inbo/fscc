
# Transformation of "layer 0" ICP Forests solid soil data to obtain "layer 1"
# ---------------------------------------------------------------------------

# Script initiation date: 18 Oct 2023

# Details: In this script, raw "layer 0" data are processed to produce
# "layer 1" using different functions. The detailed steps taken in each
# function can be found within the "Roxygen" documentation of the function,
# i.e. on top of the function scripts (in "./src/functions/" folder).

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



# 2. Save the raw data ----

# Firstly: place the downloaded zipped raw data files on Google Drive
# (in folder "./data/raw_data/download_[download_date]/")

# Secondly: synchronise your local project folders with Google Drive
# using this function.

source("./src/functions/sync_local_data.R")
sync_local_data(list_subfolders_data = "raw_data",
                list_subfolders_output = FALSE)

# If not possible to get access to Google Drive: manually unzip data folders
# and place them in "./data/raw_data/" folder. Data forms should be grouped
# in subfolders with the survey code as name (e.g. in "./data/raw_data/so/").




# Input level ----

# Only run the line of the ICP Forests level for which you would like
# to run the script

level <- "LI"
level <- "LII"




# 3. Import data ----

## 3.1. Use read_raw function ----
# Firstly read "y1" and "si", as their coordinates are used to list coordinates
# of "s1" and "so"

source("./src/functions/read_raw.R")

if (level == "LI") {

read_raw("y1", save_to_env = TRUE)
read_raw("s1", save_to_env = TRUE)
}

if (level == "LII") {

read_raw("si", save_to_env = TRUE)
read_raw("so", save_to_env = TRUE)
read_raw("sw", save_to_env = TRUE)
}



## 3.2. Solve issues with duplicate records ----

source("./src/functions/solve_record_inconsistencies.R")

if (level == "LI") {

  s1_som <- solve_record_inconsistencies(survey_form = "s1_som",
                                         data_frame = s1_som,
                                         solve = TRUE,
                                         save_to_env = FALSE)
  s1_pfh <- solve_record_inconsistencies(survey_form = "s1_pfh",
                                         data_frame = s1_pfh,
                                         solve = TRUE,
                                         save_to_env = FALSE)
}


if (level == "LII") {

  so_som <- solve_record_inconsistencies(survey_form = "so_som",
                                       data_frame = so_som,
                                       solve = TRUE,
                                       save_to_env = FALSE)

  so_pfh <- solve_record_inconsistencies(survey_form = "so_pfh",
                                         data_frame = so_pfh,
                                         solve = TRUE,
                                         save_to_env = FALSE)

}






# 4. Gap-filling from external data sources ----

## 4.1. Gap-fill using new data from PIRs ----

# Apply gapfill_from_pir scripts

source("./src/functions/gapfill_from_pir.R")


if (level == "LI") {

gapfill_from_pir(code_survey = "y1",
                 save_to_env = TRUE)
gapfill_from_pir(code_survey = "s1",
                 save_to_env = TRUE)
}

if (level == "LII") {

gapfill_from_pir(code_survey = "si",
                 save_to_env = TRUE)
gapfill_from_pir(code_survey = "so",
                 save_to_env = TRUE)
gapfill_from_pir(code_survey = "sw",
                 save_to_env = TRUE)
}

# TO DO: initiate a column for each parameter to indicate the sources of the
# data (e.g. "layer 0", "partner communication", ...)

# In the previous step, "pir_applied" objects are generated for each survey
# form, which contains the new data from the pirs, and the action that was
# taken with these data (i.e. whether or not it was still necessary to add
# them to the survey forms). Combine the different pir_applied dataframes
# into one dataframe.

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with(object_name_start = "pir_applied",
                           object_type = "pir_applied",
                           save_to_env = TRUE)

# Save the pir_applied dataframe

wb <- createWorkbook()
addWorksheet(wb, "Inconsistency report")
writeData(wb, 1, pir_applied)
addFilter(wb, 1, row = 1, cols = seq_len(ncol(pir_applied)))
freezePane(wb, 1, firstActiveRow = 2, firstActiveCol = 1)
addCreator(wb, "ICP Forests - FSCC")
openxlsx::saveWorkbook(wb,
                       file = paste0("./output/gap_filling_details/",
                                     "20230302_applied_pirs_",
                                     tolower(level),
                                     ".xlsx"),
                       overwrite = TRUE)







## 4.2. Gap-fill using old database sources (e.g. AFSCDB.LII) ----
# Note: only focussing on parameters for C stock calculations
# To do: expand for other parameters
# To do: expand for LI

source("./src/functions/gapfill_from_old_data.R")

if (level == "LI") {

  s1_som <- gapfill_from_old_data(survey_form = "s1_som",
                                  data_frame = s1_som,
                                  save_to_env = FALSE)
}

if (level == "LII") {

  so_som <- gapfill_from_old_data(survey_form = "so_som",
                                  data_frame = so_som,
                                  save_to_env = FALSE)
}




## 4.3. Gap-fill "prf" using manually harmonised profile data Nathalie ----

source("./src/functions/harmonise_prf.R")

if (level == "LI") {

  s1_prf <- harmonise_prf(survey_form = "s1_prf",
                          data_frame = s1_prf,
                          save_to_env = FALSE)
}

if (level == "LII") {

  so_prf <- harmonise_prf(survey_form = "so_prf",
                          data_frame = so_prf,
                          save_to_env = FALSE)
}






# 5. Automated data corrections using inconsistency-generating functions ----

# Note: at the moment, these functions also generate PIRs, but this is not
# relevant to the current aim of this script, i.e. data processing to layer 1.



## 5.1. Inconsistencies in primary keys (survey_year, code_layer) ----

# The argument "solve = TRUE" tells
# the function to rename "code_layer" in "s1_som"
# in case of ambiguous (non-unique) code_layers

source("./src/functions/get_primary_inconsistencies.R")

if (level == "LI") {

get_primary_inconsistencies(code_survey = "y1",
                            save_to_env = TRUE)
get_primary_inconsistencies(code_survey = "s1", solve = TRUE,
                            save_to_env = TRUE)

s1_pfh1 <- s1_pfh
s1_som1 <- s1_som
}

if (level == "LII") {

get_primary_inconsistencies(code_survey = "si",
                            save_to_env = TRUE)
get_primary_inconsistencies(code_survey = "so", solve = TRUE,
                            save_to_env = TRUE)
get_primary_inconsistencies(code_survey = "sw",
                            save_to_env = TRUE)

so_pfh1 <- so_pfh
so_som1 <- so_som
}

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_primary_inconsistencies", save_to_env = TRUE)



## 5.2. Inconsistencies in range/presence of data ----
# "solve = TRUE" converts data in the wrong units to the correct units

source("./src/functions/get_range_inconsistencies.R")

if (level == "LI") {

  s1_som <- get_range_inconsistencies("s1_som", s1_som,
                                      solve = TRUE, save_to_env = FALSE)
  s1_pfh <- get_range_inconsistencies("s1_pfh", s1_pfh,
                                      solve = TRUE, save_to_env = FALSE)
  s1_prf <- get_range_inconsistencies("s1_prf", s1_prf,
                                      save_to_env = FALSE)
  s1_pls <- get_range_inconsistencies("s1_pls", s1_pls,
                                      save_to_env = FALSE)
  y1_st1 <- get_range_inconsistencies("y1_st1", y1_st1,
                                      save_to_env = FALSE)
  s1_pfh2 <- s1_pfh
  s1_som2 <- s1_som
}


if (level == "LII") {

  so_som <- get_range_inconsistencies("so_som", so_som,
                                      solve = TRUE, save_to_env = FALSE)
  so_pfh <- get_range_inconsistencies("so_pfh", so_pfh,
                                      solve = TRUE, save_to_env = FALSE)
  sw_swc <- get_range_inconsistencies("sw_swc", sw_swc,
                                      solve = TRUE, save_to_env = FALSE)
  so_prf <- get_range_inconsistencies("so_prf", so_prf,
                                      save_to_env = FALSE)
  so_pls <- get_range_inconsistencies("so_pls", so_pls,
                                      save_to_env = FALSE)
  si_sta <- get_range_inconsistencies("si_sta", si_sta,
                                      save_to_env = FALSE)
  so_pfh2 <- so_pfh
  so_som2 <- so_som
}




source("./src/functions/harmonise_below_loqs.R")

if (level == "LI") {

  s1_som <- harmonise_below_loqs(survey_form = "s1_som",
                                 data_frame = s1_som)

  s1_pfh <- harmonise_below_loqs(survey_form = "s1_pfh",
                                 data_frame = s1_pfh)
}

if (level == "LII") {

  so_som <- harmonise_below_loqs(survey_form = "so_som",
                                 data_frame = so_som)

  so_pfh <- harmonise_below_loqs(survey_form = "so_pfh",
                                 data_frame = so_pfh)
}




## 5.3. Inconsistencies in soil layers ----
# "solve" indicates whether the obvious mistakes can be solved

# Attention: make sure that "pfh" survey forms are processed first
# to facilitate gap-filling of "som" based on "pfh"
# (e.g. layer limits forest floor)

source("./src/functions/get_layer_inconsistencies.R")

if (level == "LI") {

s1_pfh <- get_layer_inconsistencies(survey_form = "s1_pfh",
                                    data_frame = s1_pfh,
                                    solve = TRUE,
                                    save_to_env = FALSE)
s1_som <- get_layer_inconsistencies(survey_form = "s1_som",
                                    data_frame = s1_som,
                                    solve = TRUE,
                                    save_to_env = FALSE)
s1_pfh3 <- s1_pfh
s1_som3 <- s1_som
}

if (level == "LII") {

so_pfh <- get_layer_inconsistencies(survey_form = "so_pfh",
                                    data_frame = so_pfh,
                                    solve = TRUE,
                                    save_to_env = FALSE)
so_som <- get_layer_inconsistencies(survey_form = "so_som",
                                    data_frame = so_som,
                                    solve = TRUE,
                                    save_to_env = FALSE)
so_pfh3 <- so_pfh
so_som3 <- so_som
}





# At this stage, link forest floor layers in "som" with those of "pfh"
# in the same survey.
# This can be used to gap-fill forest floor layer limits as well as bulk
# densities












## 5.4. Inconsistencies in derived variables ----
# TO DO: update to integrate LOQs better

source("./src/functions/get_derived_variable_inconsistencies.R")

if (level == "LI") {
  s1_som <- get_derived_variable_inconsistencies("s1_som", s1_som,
                                                 save_to_env = FALSE)
  s1_pfh <- get_derived_variable_inconsistencies("s1_pfh", s1_pfh,
                                                 save_to_env = FALSE)
}

if (level == "LII") {

  so_som <- get_derived_variable_inconsistencies("so_som", so_som,
                                                 save_to_env = FALSE)
  so_pfh <- get_derived_variable_inconsistencies("so_pfh", so_pfh,
                                                 save_to_env = FALSE)

}

# source("./src/functions/bind_objects_starting_with.R")
# bind_objects_starting_with("list_derived_inconsistencies", save_to_env = TRUE)





# 6. Additional manual corrections ----

# Some issues have not been picked up during the automated corrections

# Add "additional_manual_corrections_fscc"

source("./src/functions/apply_additional_manual_corr.R")

if (level == "LI") {

  s1_som <- apply_additional_manual_corr(survey_form = "s1_som",
                                       data_frame = s1_som)
  s1_pfh <- apply_additional_manual_corr(survey_form = "s1_pfh",
                                         data_frame = s1_pfh)
}

if (level == "LII") {

  so_som <- apply_additional_manual_corr(survey_form = "so_som",
                                         data_frame = so_som)
  so_pfh <- apply_additional_manual_corr(survey_form = "so_pfh",
                                         data_frame = so_pfh)

}









# 7. Internal gap-filling ----

# Gap-fill "som": parameters for C stock calculations
# Note: At the moment, focus on parameters for C stock calculations only
# To do: expand to other parameters


source("./src/functions/gapfill_internally.R")

if (level == "LI") {

s1_som <- gapfill_internally(survey_form = "s1_som",
                           data_frame = s1_som,
                           save_to_env = FALSE)


s1_pfh <- gapfill_internally(survey_form = "s1_pfh",
                             data_frame = s1_pfh,
                             save_to_env = FALSE)

s1_pfh4 <- s1_pfh
s1_som4 <- s1_som

}



if (level == "LII") {

so_som <- gapfill_internally(survey_form = "so_som",
                             data_frame = so_som,
                             save_to_env = FALSE)


so_pfh <- gapfill_internally(survey_form = "so_pfh",
                             data_frame = so_pfh,
                             save_to_env = FALSE)

so_pfh4 <- so_pfh
so_som4 <- so_som

}









# Add uncertainty ranges for chemical parameters
# Function currently only includes TOC

source("./src/functions/add_uncertainties_chem.R")

if (level == "LI") {

  s1_som <- add_uncertainties_chem(survey_form = "s1_som",
                                   data_frame = s1_som,
                                   save_to_env = FALSE)


  s1_pfh <- add_uncertainties_chem(survey_form = "s1_pfh",
                                   data_frame = s1_pfh,
                                   save_to_env = FALSE)

}



if (level == "LII") {

  so_som <- add_uncertainties_chem(survey_form = "so_som",
                                   data_frame = so_som,
                                   save_to_env = FALSE)


  so_pfh <- add_uncertainties_chem(survey_form = "so_pfh",
                                   data_frame = so_pfh,
                                   save_to_env = FALSE)

}





# 8. Export the processed survey forms ----

## 8.1. Save processed survey forms to Google Drive (layer 1) ----

source("./src/functions/save_to_google_drive.R")
save_to_google_drive(objects_to_save = c("si", "so", "sw"),
                     path_name = "layer1_data")


source("./src/functions/save_to_google_drive.R")
save_to_google_drive(objects_to_save = c("y1", "s1"),
                     path_name = "layer1_data")

source("./src/functions/save_to_google_drive.R")
save_to_google_drive(path_name = "layer1_data")

## 8.2. Sync local data with Google Drive ----

source("./src/functions/sync_local_data.R")
sync_local_data(list_subfolders_data = "layer1_data",
                list_subfolders_output = FALSE)




## 8.3. For further processing: import the processed survey forms ----

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)
read_processed(survey_forms = c("si", "so", "sw"),
               save_to_env = TRUE)
read_processed(survey_forms = c("y1", "s1"),
               path_name = "./data/layer1_data/",
               save_to_env = TRUE)

