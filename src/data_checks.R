
# Evaluate the consistency of the ICP Forests solid soil data
# and create "Partner Inconsistency Reports" (PIRs)

# Prepare packages ----

# Update the CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Define packages to install
packages_fscc <- c("sf", 
                   "tidyverse", 
                   "openxlsx", 
                   "parsedate",
                   "googlesheets4",
                   "googledrive",
                   "assertthat")

# Install all packages that are not already installed
install.packages(setdiff(packages_fscc, rownames(installed.packages())))

# Load packages
sapply(packages_fscc, library, character.only = TRUE)



# Specify date on which 'layer 0' data were downloaded from ICP Forests website ----
download_date <- "20230605"
download_date_pir <- as.Date(parsedate::parse_iso_8601(download_date)) 

# Import data ----
# First read "y1" and "si", as their coordinates are used to list coordinates
# of "s1" and "so"
source("./src/functions/read_icpforests_csv.R")
read_icpforests_csv("y1", download_date)
read_icpforests_csv("si", download_date)
read_icpforests_csv("s1", download_date)
read_icpforests_csv("so", download_date)
read_icpforests_csv("sw", download_date)



# Merge duplicate records in "so_som" ----

source("./src/functions/merge_duplicate_records.R")
merge_duplicate_records("so_som", merge = TRUE)


# Get inconsistencies in primary keys (survey_year, code_layer) ----

  # The argument "solve = TRUE" tells
  # the function to rename "code_layer" in "s1_som"
  # in case of ambiguous (non-unique) code_layers

source("./src/functions/get_primary_inconsistencies.R")
get_primary_inconsistencies("y1")
get_primary_inconsistencies("s1", solve = TRUE) 
get_primary_inconsistencies("si")
get_primary_inconsistencies("so", solve = TRUE)
get_primary_inconsistencies("sw")

list_primary_inconsistencies <- rbind(list_primary_inconsistencies_y1,
                                      list_primary_inconsistencies_s1,
                                      list_primary_inconsistencies_si,
                                      list_primary_inconsistencies_so,
                                      list_primary_inconsistencies_sw)

View(list_primary_inconsistencies)

rm(list_primary_inconsistencies_y1,
   list_primary_inconsistencies_s1,
   list_primary_inconsistencies_si,
   list_primary_inconsistencies_so,
   list_primary_inconsistencies_sw)



# Get inconsistencies in coordinates ----

  # Retrieve surveys which have coordinates

surveys <- c("y1_pl1","y1_st1","s1_lqa","s1_pfh","s1_pls","s1_prf","s1_som",
             "si_eve","si_plt","si_sta","si_tco","so_lqa","so_pfh","so_pls",
             "so_prf","so_som","sw_swa","sw_swc")

surveys_with_coordinates <- NULL

for (i in 1:length(surveys)) {
  
  if (surveys[i] %in% ls(envir = .GlobalEnv)) {
  if (!identical(get(surveys[i], envir = .GlobalEnv)$latitude_dec, NULL) &
      !identical(get(surveys[i], envir = .GlobalEnv)$longitude_dec, NULL)) {
    
    surveys_with_coordinates <- c(surveys_with_coordinates,
                                  surveys[i])}}}

surveys_with_coordinates
# "y1_pl1" "s1_pls" "s1_prf" "si_plt" "so_pls" "so_prf"


  # Get inconsistencies

source("./src/functions/get_coordinate_inconsistencies.R")

get_coordinate_inconsistencies(surveys_with_coordinates, 3000) 
View(list_coordinate_inconsistencies)




# Get inconsistencies in soil layers? ----
# "solve" indicates whether the obvious mistakes can be solved

source("./src/functions/get_layer_inconsistencies.R")
get_layer_inconsistencies("so_som", solve = TRUE) 
get_layer_inconsistencies("s1_som", solve = TRUE) 
get_layer_inconsistencies("so_pfh", solve = TRUE)
get_layer_inconsistencies("s1_pfh", solve = TRUE)

list_layer_inconsistencies <- rbind(list_layer_inconsistencies_s1_som,
                                    list_layer_inconsistencies_s1_pfh,
                                    list_layer_inconsistencies_so_som,
                                    list_layer_inconsistencies_so_pfh)

View(list_layer_inconsistencies) 

rm(list_layer_inconsistencies_s1_som,
   list_layer_inconsistencies_s1_pfh,
   list_layer_inconsistencies_so_som,
   list_layer_inconsistencies_so_pfh)




# Get inconsistencies in presence and range of data ----
# "solve = TRUE" converts data in the wrong units to the correct units

source("./src/functions/get_range_inconsistencies.R")

get_range_inconsistencies("so_som", solve = TRUE) 
get_range_inconsistencies("so_pfh", solve = TRUE) 
get_range_inconsistencies("s1_som", solve = TRUE) 
get_range_inconsistencies("s1_pfh", solve = TRUE) 
get_range_inconsistencies("sw_swc", solve = TRUE) 
get_range_inconsistencies("so_prf")
get_range_inconsistencies("s1_prf")
get_range_inconsistencies("so_pls")
get_range_inconsistencies("s1_pls")
get_range_inconsistencies("si_sta")
get_range_inconsistencies("y1_st1")


list_range_inconsistencies <- rbind(list_range_inconsistencies_s1_som,
                                    list_range_inconsistencies_s1_pfh, 
                                    list_range_inconsistencies_s1_prf,
                                    list_range_inconsistencies_s1_pls,
                                    list_range_inconsistencies_y1_st1,
                                    list_range_inconsistencies_so_som,
                                    list_range_inconsistencies_so_pfh,
                                    list_range_inconsistencies_so_prf,
                                    list_range_inconsistencies_so_pls,
                                    list_range_inconsistencies_si_sta,
                                    list_range_inconsistencies_sw_swc)

View(list_range_inconsistencies)

rm(list_range_inconsistencies_s1_som,
   list_range_inconsistencies_s1_pfh, 
   list_range_inconsistencies_s1_prf,
   list_range_inconsistencies_s1_pls,
   list_range_inconsistencies_y1_st1,
   list_range_inconsistencies_so_som,
   list_range_inconsistencies_so_pfh,
   list_range_inconsistencies_so_prf,
   list_range_inconsistencies_so_pls,
   list_range_inconsistencies_si_sta,
   list_range_inconsistencies_sw_swc)


# Get inconsistencies in derived variables ----

source("./src/functions/get_derived_variables.R")

get_derived_variables("so_som")
get_derived_variables("so_pfh")
get_derived_variables("s1_som")
get_derived_variables("s1_pfh")

list_derived_inconsistencies <- rbind(list_derived_inconsistencies_s1_som,
                                      list_derived_inconsistencies_s1_pfh, 
                                      list_derived_inconsistencies_so_som,
                                      list_derived_inconsistencies_so_pfh)

View(list_derived_inconsistencies) 

rm(list_derived_inconsistencies_s1_som,
   list_derived_inconsistencies_s1_pfh, 
   list_derived_inconsistencies_so_som,
   list_derived_inconsistencies_so_pfh)



# Export PIR as Excel ----

  # Bind lists with inconsistencies
inconsistency_report <- rbind(list_primary_inconsistencies,
                              list_coordinate_inconsistencies,
                              list_layer_inconsistencies,
                              list_range_inconsistencies,
                              list_derived_inconsistencies)

  # Save a copy with the download_date attached to the object name
assign(paste0("inconsistency_report_", download_date), 
       inconsistency_report, 
       envir=globalenv())

  # Export the PIR

  # "ignore_checked_inconsistencies = FALSE"
  # to take inconsistencies which were previously confirmed by partners
  # into account

source("./src/functions/export_pir.R")
export_pir("inconsistency_report", per_partner = TRUE, per_level = TRUE,
           ignore_checked_inconsistencies = FALSE) 
      

# Export the processed survey forms ----

source("./src/functions/save_intermediate.R")
save_intermediate()


# Import the processed survey forms ----

source("./src/functions/read_intermediate.R")
read_intermediate()











