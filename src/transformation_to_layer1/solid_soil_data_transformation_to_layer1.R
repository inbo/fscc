
# Transformation of "layer 0" ICP Forests solid soil data to obtain "layer 1"
# ---------------------------------------------------------------------------

# Script initiation date: 18 Oct 2023

# Details: In this script, raw "layer 0" data are processed to produce
# "layer 1" using different functions. The detailed steps taken in each
# function can be found within the "Roxygen" documentation of the function,
# i.e. on top of the function scripts (in "./src/functions/" folder).

# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O



# 1. Prepare packages ----

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

# Retrieve the date on which 'layer 0' data were downloaded
# from ICP Forests website

source("./src/functions/get_date_local.R")
download_date <- get_date_local(path = "./data/raw_data/",
                                save_to_env = TRUE,
                                collapsed = TRUE)
download_date_pir <- as.Date(parsedate::parse_iso_8601(download_date))



# 3. Import data ----

## 3.1. Use read_raw function ----
# Firstly read "y1" and "si", as their coordinates are used to list coordinates
# of "s1" and "so"

source("./src/functions/read_raw.R")
read_raw("y1", save_to_env = TRUE)
read_raw("si", save_to_env = TRUE)
read_raw("s1", save_to_env = TRUE)
read_raw("so", save_to_env = TRUE)
read_raw("sw", save_to_env = TRUE)

## 3.2. Merge duplicate records in "so_som" ----

source("./src/functions/merge_duplicate_records.R")
merge_duplicate_records("so_som",
                        merge = TRUE,
                        save_to_env = TRUE)



# 4. Gap-filling ----

## 4.1. Gap-fill using new data from PIRs ----

# Apply gapfill_from_pir scripts

source("./src/functions/gapfill_from_pir.R")
gapfill_from_pir(name_survey_form = "y1",
                 save_to_env = TRUE)
gapfill_from_pir(name_survey_form = "si",
                 save_to_env = TRUE)
gapfill_from_pir(name_survey_form = "s1",
                 save_to_env = TRUE)
gapfill_from_pir(name_survey_form = "so",
                 save_to_env = TRUE)
gapfill_from_pir(name_survey_form = "sw",
                 save_to_env = TRUE)

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
addFilter(wb, 1, row = 1, cols = 1:ncol(pir_applied))
freezePane(wb, 1, firstActiveRow = 2, firstActiveCol = 1)
addCreator(wb, "ICP Forests - FSCC")
openxlsx::saveWorkbook(wb,
                       file = paste0("./data/additional_data/",
                                     "20230302_applied_pirs.xlsx"))



## 4.2. Gap-fill "so_prf" using manually harmonised profile data Nathalie ----

  assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                             "SO_PRF_ADDS.xlsx")),
                          msg = paste0("'./data/additional_data/",
                                       "SO_PRF_ADDS.xlsx' ",
                                       "does not exist."))
  
  so_prf_adds <-
    openxlsx::read.xlsx(paste0("./data/additional_data/",
                               "SO_PRF_ADDS.xlsx"),
                        sheet = 2) %>%
    mutate(unique_survey_profile =
             paste0(code_country, "_",
                    survey_year, "_",
                    code_plot, "_",
                    profile_pit_id)) %>%
    mutate(unique_survey =
             paste0(code_country, "_",
                    survey_year, "_",
                    code_plot)) %>%
    mutate(unique_profile =
             paste0(code_country, "_",
                    code_plot, "_",
                    profile_pit_id)) %>%
    rename(bs_class = "BS.(high/low)")


  so_prf2 <- so_prf %>%
    mutate(profile_pit_id = case_when(
      !(code_country == 2 & code_plot >= 11) ~ as.character(profile_pit_id),
      TRUE ~ "p")) %>%
    mutate(unique_profile =
             paste0(code_country, "_",
                    code_plot, "_",
                    profile_pit_id)) %>%
    left_join(select(so_prf_adds,
                     RSGu, QUALu, SPECu, METHOD_RSGu, DEPTHSTOCK,
                     bs_class, remark, unique_profile),
              by = "unique_profile") %>%
    rename(code_wrb_soil_group_orig = code_wrb_soil_group,
           code_wrb_qualifier_1_orig = code_wrb_qualifier_1,
           code_wrb_spezifier_1_orig = code_wrb_spezifier_1,
           eff_soil_depth_orig = eff_soil_depth) %>%
    rename(code_wrb_soil_group = RSGu,
           code_wrb_qualifier_1 = QUALu,
           code_wrb_spezifier_1 = SPECu,
           method_rsg = METHOD_RSGu,
           eff_soil_depth = DEPTHSTOCK,
           remark_harmonisation = remark)
  
 
  
  so_prf2 %>% select(country, unique_survey_profile, code_wrb_soil_group,
                     code_wrb_soil_group_orig, method_rsg, eff_soil_depth,
                     remark_harmonisation) %>% View
  

# TO COMPLETE! (aggregate per plot before joining) 

  

# Note: At the moment, focus on parameters for C stock calculations only
  
## 4.3. Gap-fill "som": total organic carbon ----
  
  
  
## 4.4. Gap-fill "som": bulk density ----
  
  
  
## 4.5. Gap-fill "som": coarse fragments ----
  
  
  
## 4.6. Gap-fill "som": effective soil depth ----
  
  
  
## 4.7. Gap-fill "som": clay ----
  
  
  
  
## 4.8. Gap-fill using additional data sources (e.g. AFSCDB.LII) ----
  
  
  
  
  
  
  
  
  
  
  
# 5. Automated data corrections using inconsistency-generating functions ----

# Note: at the moment, these functions also generate PIRs, but this is not
# relevant to the current aim of this script, i.e. data processing to layer 1.
  
  
  
## 5.1. Inconsistencies in primary keys (survey_year, code_layer) ----

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




## 5.2. Inconsistencies in coordinates ----

# Surveys with coordinates:
# "y1_pl1" "s1_pls" "s1_prf" "si_plt" "so_pls" "so_prf"


# Get inconsistencies

source("./src/functions/get_coordinate_inconsistencies.R")

get_coordinate_inconsistencies(boundary_buffer_meter = 3000,
                               save_to_env = TRUE)
View(list_coordinate_inconsistencies)




## 5.3. Inconsistencies in soil layers ----
# "solve" indicates whether the obvious mistakes can be solved

source("./src/functions/get_layer_inconsistencies.R")
get_layer_inconsistencies("so_som", solve = TRUE, save_to_env = TRUE)
get_layer_inconsistencies("s1_som", solve = TRUE, save_to_env = TRUE)
get_layer_inconsistencies("so_pfh", solve = TRUE, save_to_env = TRUE)
get_layer_inconsistencies("s1_pfh", solve = TRUE, save_to_env = TRUE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_layer_inconsistencies", save_to_env = TRUE)
View(list_layer_inconsistencies)




## 5.4. Inconsistencies in range/presence of data ----
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



## 5.5. Inconsistencies in derived variables ----

source("./src/functions/get_derived_variable_inconsistencies.R")

get_derived_variable_inconsistencies("so_som", save_to_env = TRUE)
get_derived_variable_inconsistencies("so_pfh", save_to_env = TRUE)
get_derived_variable_inconsistencies("s1_som", save_to_env = TRUE)
get_derived_variable_inconsistencies("s1_pfh", save_to_env = TRUE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_derived_inconsistencies", save_to_env = TRUE)
View(list_derived_inconsistencies)






# 6. Export the processed survey forms ----

## 6.1. Save processed survey forms to Google Drive (layer 1) ----

source("./src/functions/save_to_google_drive.R")
save_to_google_drive(path_name = "0_01")

## 6.2. Sync local data with Google Drive ----

source("./src/functions/sync_local_data.R")
sync_local_data()

## 6.3. For further processing: import the processed survey forms ----

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)




