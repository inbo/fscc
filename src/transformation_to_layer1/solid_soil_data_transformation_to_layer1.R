
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



# 4. Gap-filling from external data sources ----

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
                       file = paste0("./output/gap_filling_details/",
                                     "20230302_applied_pirs.xlsx"),
                       overwrite = TRUE)







## 4.2. Gap-fill using old database sources (e.g. AFSCDB.LII) ----
# Note: only focussing on parameters for C stock calculations

### 4.2.1. Gap-filling existing records ----

# Import afscdb so_som data

file_path <- paste0("./data/additional_data/afscdb_LII_2_2/",
                    "AFSCDB_LII_2_2_080515_som.csv")

assertthat::assert_that(file.exists(file_path),
                        msg = paste0("'", file_path, "' ",
                                     "does not exist."))

so_som_afscdb <- read.csv(file_path,
                          sep = ";", na.strings = "") %>%
  # No repetition data
  mutate(unique_survey_layer = paste0(code_country, "_",
                                      survey_year, "_",
                                      code_plot, "_",
                                      code_layer)) %>%
  # unique_survey
  mutate(unique_survey = paste0(code_country, "_",
                                survey_year, "_",
                                code_plot)) %>%
  # plot_id
  mutate(plot_id = paste0(code_country, "_",
                          code_plot)) %>%
  # Convert coarse_fragment_mass to "coarse_fragment_vol_from_mass"
  mutate(coarse_fragment_aid =
           ifelse(!is.na(bulk_density) & !is.na(coarse_fragment_mass),
                  (.data$bulk_density *
                     (.data$coarse_fragment_mass /
                        (100 - .data$coarse_fragment_mass))) / 2650,
                  NA)) %>%
  mutate(coarse_fragment_vol_from_mass =
           ifelse(!is.na(.data$coarse_fragment_aid),
                  as.numeric((.data$coarse_fragment_aid /
                                (1 + .data$coarse_fragment_aid)) * 100),
                  NA)) %>%
  select(-coarse_fragment_aid) %>%
  mutate(coarse_fragment_vol =
           ifelse(!is.na(.data$coarse_fragment_vol),
                  .data$coarse_fragment_vol,
                  .data$coarse_fragment_vol_from_mass))


  assertthat::assert_that(length(unique(so_som_afscdb$unique_survey_layer)) ==
                          nrow(so_som_afscdb))

# Add data to so_som

if (!"bulk_density_orig" %in% names(so_som)) {
  so_som$bulk_density_orig <- so_som$bulk_density
}

if (!"organic_carbon_total_orig" %in% names(so_som)) {
  so_som$organic_carbon_total_orig <- so_som$organic_carbon_total
}

if (!"organic_layer_weight_orig" %in% names(so_som)) {
  so_som$organic_layer_weight_orig <- so_som$organic_layer_weight
}

if (!"coarse_fragment_vol_orig" %in% names(so_som)) {
  so_som$coarse_fragment_vol_orig <- so_som$coarse_fragment_vol
}

if (!"part_size_clay_orig" %in% names(so_som)) {
  so_som$part_size_clay_orig <- so_som$part_size_clay
}

# Add "afscdb" data to so_som

so_som <- so_som %>%
  left_join(so_som_afscdb %>%
              select(unique_survey_layer,
                     bulk_density,
                     organic_carbon_total,
                     organic_layer_weight,
                     coarse_fragment_vol,
                     part_size_clay) %>%
              rename(bulk_density_afscdb = bulk_density,
                     organic_carbon_total_afscdb = organic_carbon_total,
                     organic_layer_weight_afscdb = organic_layer_weight,
                     coarse_fragment_vol_afscdb = coarse_fragment_vol,
                     part_size_clay_afscdb = part_size_clay),
            by = "unique_survey_layer")
  
# Merge the columns

so_som <- so_som %>%
  # Bulk density
  mutate(bulk_density =
           ifelse(!is.na(.data$bulk_density),
                  .data$bulk_density,
                  .data$bulk_density_afscdb)) %>%
  select(-bulk_density_afscdb) %>%
  # Organic carbon total
  mutate(organic_carbon_total =
           ifelse(!is.na(.data$organic_carbon_total),
                  .data$organic_carbon_total,
                  .data$organic_carbon_total_afscdb)) %>%
  select(-organic_carbon_total_afscdb) %>%
  # Organic layer weight
  mutate(organic_layer_weight =
           ifelse(!is.na(.data$organic_layer_weight),
                  .data$organic_layer_weight,
                  .data$organic_layer_weight_afscdb)) %>%
  select(-organic_layer_weight_afscdb) %>%
  # Coarse fragments
  mutate(coarse_fragment_vol =
           ifelse(!is.na(.data$coarse_fragment_vol),
                  .data$coarse_fragment_vol,
                  .data$coarse_fragment_vol_afscdb)) %>%
  select(-coarse_fragment_vol_afscdb) %>%
  # Clay
  mutate(part_size_clay =
           ifelse(!is.na(.data$part_size_clay),
                  .data$part_size_clay,
                  .data$part_size_clay_afscdb)) %>%
  select(-part_size_clay_afscdb)
  


### 4.2.2. Adding missing records ----


# Check whether there are any "unique_survey" data in so_som_afscdb
# that are absent in so_som

# Make a vector with the unique surveys in so_som, and also the three years
# before and after, for in case only the survey year was corrected in so_som
# versus so_som_afscdb

unique_surveys_so_som <- unique(so_som$unique_survey)

# Extract the year
years <- gsub(".*_(\\d{4}).*", "\\1", unique_surveys_so_som)

# Replace each year one by one
unique_surveys <- NULL
for (i in 1:length(unique_surveys_so_som)) {
  
  unique_surveys <- c(
    unique_surveys,
    # Add unique_survey
    unique_surveys_so_som[i],
    # Add survey_year - 1
    gsub("_(\\d{4})_",
         paste0("_", as.character(as.numeric(years) - 1)[i], "_"),
         unique_surveys_so_som[i]),
    # Add survey_year - 2
    gsub("_(\\d{4})_",
         paste0("_", as.character(as.numeric(years) - 2)[i], "_"),
         unique_surveys_so_som[i]),
    # Add survey_year - 3
    gsub("_(\\d{4})_",
         paste0("_", as.character(as.numeric(years) - 3)[i], "_"),
         unique_surveys_so_som[i]),
    # Add survey_year + 1
    gsub("_(\\d{4})_",
         paste0("_", as.character(as.numeric(years) + 1)[i], "_"),
         unique_surveys_so_som[i]),
    # Add survey_year + 2
    gsub("_(\\d{4})_",
         paste0("_", as.character(as.numeric(years) + 2)[i], "_"),
         unique_surveys_so_som[i]),
    # Add survey_year + 3
    gsub("_(\\d{4})_",
         paste0("_", as.character(as.numeric(years) + 3)[i], "_"),
         unique_surveys_so_som[i]))
}


# Identify which unique surveys are missing in so_som

unique_surveys_missing <-
  unique(so_som_afscdb$unique_survey[which(!so_som_afscdb$unique_survey %in%
                                             unique_surveys)])

# If there are any unique surveys in afscdb which are missing in so_som

if (!identical(unique_surveys_missing, character(0))) {

# Create a list of tables with the partner codes for each plot_id

diff_partner_codes <- so_som %>%
  distinct(partner_code, .keep_all = TRUE) %>%
  filter(.data$partner_code != .data$code_country) %>%
  distinct(code_country) %>%
  pull(code_country)

partner_codes <- so_som %>%
  filter(!partner_code %in% diff_partner_codes) %>%
  filter(code_country %in% diff_partner_codes) %>%
  distinct(plot_id, .keep_all = TRUE) %>%
  select(plot_id, partner_code)

# Harmonise the data

so_som_afscdb_to_add <- so_som_afscdb %>%
  # Filter the missing unique surveys
  filter(.data$unique_survey %in% unique_surveys_missing) %>%
  mutate(date_labor_analyses =
           as.character(as.Date(.data$date_labor_analyses)),
         download_date = NA,
         layer_type = ifelse(.data$laytype == "Min",
                             "mineral",
                             ifelse(.data$laytype == "FF",
                                    "forest_floor",
                                    ifelse(.data$laytype == "Pea",
                                           "peat",
                                           NA))),
         repetition = 1,
         plot_id = paste0(code_country, "_",
                          code_plot),
         unique_survey = paste0(code_country, "_",
                                survey_year, "_",
                                code_plot),
         unique_survey_repetition = paste0(code_country, "_",
                                           survey_year, "_",
                                           code_plot, "_",
                                           repetition),
         unique_survey_layer = paste0(code_country, "_",
                                      survey_year, "_",
                                      code_plot, "_",
                                      code_layer),
         unique_layer_repetition = paste0(code_country, "_",
                                          survey_year, "_",
                                          code_plot, "_",
                                          code_layer, "_",
                                          repetition),
         unique_layer = paste0(code_country, "_",
                               code_plot, "_",
                               code_layer),
         change_date = as.character(as.Date("2008-05-15")),
         code_line = NA,
         code_plot_orig = code_plot,
         bulk_density_orig = bulk_density,
         organic_carbon_total_orig = organic_carbon_total,
         organic_layer_weight_orig = organic_layer_weight,
         coarse_fragment_vol_orig = coarse_fragment_vol,
         part_size_clay_orig = part_size_clay,
         code_soil_horizon_sample_c = NA,
         elec_cond = NA,
         line_nr = NA,
         ni = NA,
         origin = NA,
         origin_merge_info = NA,
         origin_merged = NA,
         p_ox = NA,
         q_flag = NA,
         qif_key = NA,
         subsamples = NA) %>%
  left_join(partner_codes,
            by = "plot_id") %>%
  mutate(partner_code = ifelse(!is.na(.data$partner_code),
                               .data$partner_code,
                               .data$code_country)) %>%
  left_join(d_country[, c("code", "lib_country")],
            by = join_by(code_country == code)) %>%
  rename(country = lib_country) %>%
  left_join(d_partner[, c("code", "desc_short", "description")],
            by = join_by(partner_code == code)) %>%
  rename(partner_short = desc_short) %>%
  rename(partner = description) %>%
  mutate(country = as.factor(country)) %>%
  mutate(partner_short = as.factor(partner_short)) %>%
  mutate(partner = as.factor(partner)) %>%
  rename(base_saturation = bs) %>%
  # Replace empty strings with NA
  mutate_all(~ replace(., . == "", NA)) %>%
  select(
    country, partner_short, partner, survey_year, code_country,
    code_plot, code_layer, repetition,
    layer_limit_superior, layer_limit_inferior, subsamples,
    date_labor_analyses, moisture_content,
    part_size_clay, part_size_silt, part_size_sand,
    code_texture_class, bulk_density, coarse_fragment_vol,
    organic_layer_weight, ph_cacl2, ph_h2o, organic_carbon_total,
    n_total, carbonates, exch_acidiy,
    exch_al, exch_ca, exch_fe, exch_k, exch_mg, exch_mn,
    exch_na, free_h, extrac_al, extrac_ca,
    extrac_cd, extrac_cr, extrac_cu, extrac_fe, extrac_hg,
    extrac_k, extrac_mg, extrac_mn, extrac_na,
    extrac_ni, extrac_p, extrac_pb, extrac_s, extrac_zn,
    tot_al, tot_ca, tot_fe, tot_k, tot_mg,
    tot_mn, tot_na, rea_al, rea_fe, exch_bce, exch_ace,
    exch_cec, elec_cond, ni, base_saturation,
    origin, code_soil_horizon_sample_c, p_ox, other_obs,
    partner_code, q_flag, change_date, code_line,
    line_nr, qif_key, download_date, layer_type,
    code_plot_orig, plot_id, unique_survey, unique_survey_repetition,
    unique_survey_layer, unique_layer_repetition, unique_layer,
    origin_merged, origin_merge_info, bulk_density_orig,
    organic_carbon_total_orig, organic_layer_weight_orig,
    coarse_fragment_vol_orig, part_size_clay_orig)

assertthat::assert_that(all(names(so_som) == names(so_som_afscdb_to_add)))

so_som <- rbind(so_som,
                so_som_afscdb_to_add)

} # End of adding afscdb records




## 4.3. Gap-fill "so_prf" using manually harmonised profile data Nathalie ----

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
  rename(bs_class = "BS.(high/low)",
         plot_id = PLOT_ID)

so_prf_adds_agg <- so_prf_adds %>%
  mutate(soil_wrb = paste0(RSGu, "_",
                           QUALu, "_",
                           SPECu, "_",
                           METHOD_RSGu, "_",
                           DEPTHSTOCK, "_",
                           bs_class, "_",
                           remark)) %>%
  group_by(plot_id) %>%
  # Sometimes there are different options, e.g. plot_id 60_9
  # No good way to solve this - we just have to pick one
  summarise(soil_wrb =
              names(which.max(table(soil_wrb[!is.na(soil_wrb)])))) %>%
  # Split the data back into the original columns
  separate(soil_wrb,
           into = c("code_wrb_soil_group",
                    "code_wrb_qualifier_1",
                    "code_wrb_spezifier_1",
                    "method_wrb_harmonisation_fscc",
                    "eff_soil_depth",
                    "bs_class",
                    "remark_harmonisation_fscc"),
           sep = "_") %>%
  mutate(eff_soil_depth = as.numeric(eff_soil_depth))

so_prf_adds_agg[so_prf_adds_agg == "NA"] <- NA
so_prf_adds_agg[so_prf_adds_agg == ""] <- NA


so_prf <- so_prf %>%
  rename(code_wrb_soil_group_orig = code_wrb_soil_group,
         code_wrb_qualifier_1_orig = code_wrb_qualifier_1,
         code_wrb_spezifier_1_orig = code_wrb_spezifier_1,
         eff_soil_depth_orig = eff_soil_depth) %>%
  left_join(so_prf_adds_agg,
            by = "plot_id")







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
so_som <- get_layer_inconsistencies("so_som", so_som,
                                    solve = TRUE, save_to_env = FALSE)
s1_som <- get_layer_inconsistencies("s1_som", s1_som,
                                    solve = TRUE, save_to_env = FALSE)
so_pfh <- get_layer_inconsistencies("so_pfh", so_pfh,
                                    solve = TRUE, save_to_env = FALSE)
s1_pfh <- get_layer_inconsistencies("s1_pfh", s1_pfh,
                                    solve = TRUE, save_to_env = FALSE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_layer_inconsistencies", save_to_env = TRUE)
View(list_layer_inconsistencies)



# TO DO: adjust layer depths in profiles that contain peat/mineral layers
# with negative depths!!!




## 5.4. Inconsistencies in range/presence of data ----
# "solve = TRUE" converts data in the wrong units to the correct units

source("./src/functions/get_range_inconsistencies.R")

s1_som <- get_range_inconsistencies("s1_som", s1_som,
                                    solve = TRUE, save_to_env = FALSE)
s1_pfh <- get_range_inconsistencies("s1_pfh", s1_pfh,
                                    solve = TRUE, save_to_env = FALSE)
so_som <- get_range_inconsistencies("so_som", so_som,
                                    solve = TRUE, save_to_env = FALSE)
so_pfh <- get_range_inconsistencies("so_pfh", so_pfh,
                                    solve = TRUE, save_to_env = FALSE)
sw_swc <- get_range_inconsistencies("sw_swc", sw_swc,
                                    solve = TRUE, save_to_env = FALSE)
so_prf <- get_range_inconsistencies("so_prf", so_prf,
                                    save_to_env = FALSE)
s1_prf <- get_range_inconsistencies("s1_prf", s1_prf,
                                    save_to_env = FALSE)
so_pls <- get_range_inconsistencies("so_pls", so_pls,
                                    save_to_env = FALSE)
s1_pls <- get_range_inconsistencies("s1_pls", s1_pls,
                                    save_to_env = FALSE)
si_sta <- get_range_inconsistencies("si_sta", si_sta,
                                    save_to_env = FALSE)
y1_st1 <- get_range_inconsistencies("y1_st1", y1_st1,
                                    save_to_env = FALSE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_range_inconsistencies", save_to_env = TRUE)
View(list_range_inconsistencies)


## 5.5. Harmonise data below LOQ ----

source("./src/functions/harmonise_below_loqs.R")

so_som <- harmonise_below_loqs(survey_form = "so_som",
                               data_frame = so_som)

so_pfh <- harmonise_below_loqs(survey_form = "so_pfh",
                               data_frame = so_pfh,
                               parameters = c("horizon_clay",
                                              "horizon_silt",
                                              "horizon_sand",
                                              "horizon_c_organic_total",
                                              "horizon_n_total"))



## 5.6. Inconsistencies in derived variables ----

# TO DO: update to integrate LOQs better
source("./src/functions/get_derived_variable_inconsistencies.R")

so_som <- get_derived_variable_inconsistencies("so_som", so_som,
                                               save_to_env = FALSE)
so_pfh <- get_derived_variable_inconsistencies("so_pfh", so_pfh,
                                               save_to_env = FALSE)
s1_som <- get_derived_variable_inconsistencies("s1_som", s1_som,
                                               save_to_env = FALSE)
s1_pfh <- get_derived_variable_inconsistencies("s1_pfh", s1_pfh,
                                               save_to_env = FALSE)

source("./src/functions/bind_objects_starting_with.R")
bind_objects_starting_with("list_derived_inconsistencies", save_to_env = TRUE)
View(list_derived_inconsistencies)




# 6. Internal gap-filling ----

  ## To do: Gap-fill effective soil depth Bruno?
  
# Gap-fill "som": parameters for C stock calculations
# Note: At the moment, focus on parameters for C stock calculations only
  
  # Firstly apply the "get_layer_inconsistencies" function
  # which already gap-fills layer limits
  
  # source("./src/functions/get_layer_inconsistencies.R")
  # so_som <- get_layer_inconsistencies("so_som", so_som,
  #                                     solve = TRUE, save_to_env = FALSE)
  
## 6.1. Source 1: "sw_swc" ----
  
  # Import additional sw_swc file with corresponding fixed-depth layers
  # by Nathalie (manually created)
  
  # TO DO: automate the assignment of corresponding fixed-depth layers
  
  file_path <- "./data/additional_data/sw_swc/SW_SWC_code_layer_SOM.csv"
  
  assertthat::assert_that(file.exists(file_path),
                          msg = paste0("'", file_path, "' ",
                                       "does not exist."))
  
  sw_swc_adds <- read.csv2(file_path)
  
  # Preprocess file
  
  sw_swc_adds_sameyear <- sw_swc_adds %>%
    rename(code_layer_som = code_layer_SOM) %>%
    rename(plot_id = PLOTID) %>%
    # Remove records without corresponding fixed-depth layer
    filter(code_layer_som %in%
             c("O", "OH", "OFH",
               "M05", "M51", "M01", "M12", "M24", "M48")) %>%
    # Create unique_survey_layer
    mutate(unique_survey_layer =
             paste0(code_country, "_",
                    survey_year, "_",
                    code_plot, "_",
                    code_layer_som)) %>%
    # Create unique_layer
    mutate(unique_layer =
             paste0(code_country, "_",
                    code_plot, "_",
                    code_layer_som)) %>%
    # Aggregate over replicates
    group_by(unique_survey_layer, unique_layer) %>%
    summarise(bulk_density =
                mean(bulk_density, na.rm = TRUE),
              .groups = "drop")
  
  
  # Aggregate different survey years per unique layer (plot_id x code_layer)
  # To gap-fill data not from the same survey_year
  
  sw_swc_adds_otheryear <- sw_swc_adds_sameyear %>%
    group_by(unique_layer) %>%
    summarise(bulk_density =
                mean(bulk_density, na.rm = TRUE),
              .groups = "drop")
  
  
  
  
  
  
## 6.2. Source 2: "so_pfh" ----
  
  # Redundant layers do already need to be removed from so_pfh before
  # being able to harmonise the layers into pre-defined depth intervals
  
  # source("./src/functions/get_layer_inconsistencies.R")
  # so_pfh_harmonised_layers <- get_layer_inconsistencies("so_pfh", so_pfh,
  #                                                       solve = TRUE,
  #                                                       save_to_env = FALSE)
  
  # This function converts random (e.g. pedogenic) depth layers
  # into a dataframe with pre-defined fixed-depth layers
  
  assertthat::assert_that("layer_number" %in% names(so_pfh))
  
  source("./src/functions/harmonise_into_fixed_depth_layers.R")
  
  so_pfh_fixed <-
    harmonise_into_fixed_depth_layers(survey_form = so_pfh)
  
  # The function harmonise_into_fixed_depth_layers automatically makes a
  # column "bulk_density" which contains values for "horizon_bulk_dens_measure"
  # if this exists, else "horizon_bulk_dens_est".
  
  # Convert coarse fragments to the right units (volume %)
  
  d_soil_coarse_fragments <-
    read.csv2("./data/additional_data/d_soil_coarse_fragments.csv") %>%
    select(code, coarse_fragment_vol_avg)
  
  so_pfh_fixed <- so_pfh_fixed %>%
    # Convert volumetric coarse fragment codes to actual average vol %
    left_join(d_soil_coarse_fragments,
              by = join_by(code_horizon_coarse_vol == code)) %>%
    # Convert weight percentages to volumetric percentages:
    # Imagine: 1 m³ of fine earth contains
    # e.g. 1300 kg fine earth (bulk density).
    # Then, imagine the weight percentage of coarse fragments
    # from that soil is 11 %.
    # That means that there is 1300 kg * 11/89 = 160.7 kg of coarse fragments
    # for 1 m³ of fine earth in this soil.
    # Imagine the coarse fragments have a particle density of 2650 kg per m³.
    # Then, we can calculate that this 160.7 kg of coarse fragments occupies
    # 160.7/2650 = 0.061 m³.
    # As such, the vol % of coarse fragments will be 0.061 / (1 + 0.061)
    mutate(coarse_fragment_aid =
             ifelse(!is.na(bulk_density) & !is.na(horizon_coarse_weight),
                    (.data$bulk_density *
                      (.data$horizon_coarse_weight /
                         (100 - .data$horizon_coarse_weight))) / 2650,
                    NA)) %>%
    mutate(coarse_fragment_vol_converted =
             ifelse(!is.na(.data$coarse_fragment_aid),
                    as.numeric((.data$coarse_fragment_aid /
                       (1 + .data$coarse_fragment_aid)) * 100),
                    NA)) %>%
    select(-coarse_fragment_aid) %>%
    mutate(coarse_fragment_vol =
             # If both volumetric codes and weight % are available:
             ifelse(!is.na(.data$coarse_fragment_vol_avg) &
                      !is.na(.data$coarse_fragment_vol_converted),
                    # Better not to take the average in that case,
                    # because the converted weight % seem more reliable
                    # (volumetric classes were broad)
                    # Priority: converted weight %
                    .data$coarse_fragment_vol_converted,
                    # Else, take whichever measure for coarse fragments
                    # that is available
                    ifelse(!is.na(.data$coarse_fragment_vol_converted),
                           .data$coarse_fragment_vol_converted,
                           .data$coarse_fragment_vol_avg)))
  

  
  # Aggregate different profiles per plot (per survey layer)
  
  so_pfh_fixed_depths_agg_prof <- so_pfh_fixed %>%
    mutate(unique_survey_layer = paste0(code_country, "_",
                                        survey_year, "_",
                                        code_plot, "_",
                                        code_layer)) %>%
    mutate(unique_layer = paste0(code_country, "_",
                                 code_plot, "_",
                                 code_layer)) %>%
    group_by(unique_survey_layer, unique_layer,
             code_country, survey_year, code_plot, code_layer) %>%
    summarise(layer_limit_superior =
                mean(layer_limit_superior, na.rm = TRUE),
              layer_limit_inferior =
                mean(layer_limit_inferior, na.rm = TRUE),
              horizon_c_organic_total =
                mean(horizon_c_organic_total, na.rm = TRUE),
              horizon_clay =
                mean(horizon_clay, na.rm = TRUE),
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    as.data.frame %>%
    mutate(layer_limit_superior = ifelse(is.nan(layer_limit_superior),
                                         NA, layer_limit_superior),
           layer_limit_inferior = ifelse(is.nan(layer_limit_inferior),
                                         NA, layer_limit_inferior),
           horizon_c_organic_total = ifelse(is.nan(horizon_c_organic_total),
                                            NA, horizon_c_organic_total),
           horizon_clay = ifelse(is.nan(horizon_clay),
                                 NA, horizon_clay),
           bulk_density = ifelse(is.nan(bulk_density),
                                 NA, bulk_density),
           coarse_fragment_vol = ifelse(is.nan(coarse_fragment_vol),
                                        NA, coarse_fragment_vol))
  
  # Export so_pfh_fixed
  
  write.csv2(so_pfh_fixed_depths_agg_prof,
             paste0("./output/gap_filling_details/",
                    "20231020_so_pfh_fixed_depths_agg_prof.csv"),
             row.names = FALSE,
             na = "")
  
  # Aggregate different survey years per unique layer (plot_id x code_layer)
  # To gap-fill data not from the same survey_year
  
  so_pfh_fixed_otheryear <- so_pfh_fixed %>%
    mutate(unique_layer = paste0(code_country, "_",
                                 code_plot, "_",
                                 code_layer)) %>%
    group_by(unique_layer,
             code_country, code_plot, code_layer) %>%
    summarise(layer_limit_superior =
                mean(layer_limit_superior, na.rm = TRUE),
              layer_limit_inferior =
                mean(layer_limit_inferior, na.rm = TRUE),
              horizon_clay =
                mean(horizon_clay, na.rm = TRUE),
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    as.data.frame %>%
    mutate(layer_limit_superior = ifelse(is.nan(layer_limit_superior),
                                         NA, layer_limit_superior),
           layer_limit_inferior = ifelse(is.nan(layer_limit_inferior),
                                         NA, layer_limit_inferior),
           horizon_clay = ifelse(is.nan(horizon_clay),
                                 NA, horizon_clay),
           bulk_density = ifelse(is.nan(bulk_density),
                                 NA, bulk_density),
           coarse_fragment_vol = ifelse(is.nan(coarse_fragment_vol),
                                        NA, coarse_fragment_vol))
  
  # horizon_c_organic_total should not be assessed based on values from
  # other survey years
  
  
  

  
  
  
 
  
## 6.3. Source 3: "so_som" (other survey years) ----
  
  so_som_otheryear <- so_som %>%
    group_by(unique_layer,
             code_country, code_plot, code_layer) %>%
    summarise(layer_limit_superior =
                mean(layer_limit_superior, na.rm = TRUE),
              layer_limit_inferior =
                mean(layer_limit_inferior, na.rm = TRUE),
              part_size_clay =
                mean(part_size_clay, na.rm = TRUE),
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    as.data.frame %>%
    mutate(layer_limit_superior = ifelse(is.nan(layer_limit_superior),
                                         NA, layer_limit_superior),
           layer_limit_inferior = ifelse(is.nan(layer_limit_inferior),
                                         NA, layer_limit_inferior),
           part_size_clay = ifelse(is.nan(part_size_clay),
                                   NA, part_size_clay),
           bulk_density = ifelse(is.nan(bulk_density),
                                 NA, bulk_density),
           coarse_fragment_vol = ifelse(is.nan(coarse_fragment_vol),
                                        NA, coarse_fragment_vol))
  
  # organic_layer_weight and organic_carbon_total should not be assessed
  # based on other survey years
  
  
  
  
  
  
  
  
## 6.4. Compile ----
  
  # Add data to so_som
  
  so_som <- so_som %>%
    # sw_swc same year
    left_join(sw_swc_adds_sameyear %>%
                select(-unique_layer) %>%
                rename(bulk_density_sw_swc_sameyear =
                         bulk_density),
              by = "unique_survey_layer") %>%
    # so_pfh same year
    left_join(so_pfh_fixed_depths_agg_prof %>%
                rename(organic_carbon_total_so_pfh_sameyear =
                         horizon_c_organic_total,
                       bulk_density_so_pfh_sameyear =
                         bulk_density,
                       coarse_fragment_vol_so_pfh_sameyear =
                         coarse_fragment_vol,
                       part_size_clay_so_pfh_sameyear =
                         horizon_clay) %>%
                select(unique_survey_layer,
                       organic_carbon_total_so_pfh_sameyear,
                       bulk_density_so_pfh_sameyear,
                       coarse_fragment_vol_so_pfh_sameyear,
                       part_size_clay_so_pfh_sameyear),
              by = "unique_survey_layer") %>%
    # so_som other year
    left_join(so_som_otheryear %>%
                rename(bulk_density_so_som_otheryear =
                         bulk_density,
                       coarse_fragment_vol_so_som_otheryear =
                         coarse_fragment_vol,
                       part_size_clay_so_som_otheryear =
                         part_size_clay) %>%
                select(unique_layer,
                       bulk_density_so_som_otheryear,
                       coarse_fragment_vol_so_som_otheryear,
                       part_size_clay_so_som_otheryear),
              by = "unique_layer") %>%
    # sw_swc other year
    left_join(sw_swc_adds_otheryear %>%
                rename(bulk_density_sw_swc_otheryear =
                         bulk_density),
              by = "unique_layer") %>%
    # so_pfh other year
    left_join(so_pfh_fixed_otheryear %>%
                rename(bulk_density_so_pfh_otheryear =
                         bulk_density,
                       coarse_fragment_vol_so_pfh_otheryear =
                         coarse_fragment_vol,
                       part_size_clay_so_pfh_otheryear =
                         horizon_clay) %>%
                select(unique_layer,
                       bulk_density_so_pfh_otheryear,
                       coarse_fragment_vol_so_pfh_otheryear,
                       part_size_clay_so_pfh_otheryear),
              by = "unique_layer")
  
  
  # Bulk density: combine columns
  
  if (!"bulk_density_orig" %in% names(so_som)) {
    so_som$bulk_density_orig <- so_som$bulk_density
  }
  
  so_som <- so_som %>%
    mutate(bulk_density_source =
             # Priority 1: so_som data from same year
             ifelse(!is.na(.data$bulk_density),
                    "so_som (same year)",
              # Priority 2: sw_swc data from same year
              ifelse(!is.na(.data$bulk_density_sw_swc_sameyear),
                     "sw_swc (same year)",
               # Priority 3: so_pfh data from same year
               ifelse(!is.na(.data$bulk_density_so_pfh_sameyear),
                      "so_pfh (same year)",
                # Priority 4: so_som data from other year
                ifelse(!is.na(.data$bulk_density_so_som_otheryear),
                       "so_som (other year)",
                 # Priority 5: sw_swc data from other year
                 ifelse(!is.na(.data$bulk_density_sw_swc_otheryear),
                        "sw_swc (other year)",
                  # Priority 6: so_pfh data from other year
                  ifelse(!is.na(.data$bulk_density_so_pfh_otheryear),
                         "so_pfh (other year)",
                         NA)))))),
           bulk_density =
             # Priority 1: so_som data from same year
             ifelse(!is.na(.data$bulk_density),
                    .data$bulk_density,
              # Priority 2: sw_swc data from same year
              ifelse(!is.na(.data$bulk_density_sw_swc_sameyear),
                     .data$bulk_density_sw_swc_sameyear,
               # Priority 3: so_pfh data from same year
               ifelse(!is.na(.data$bulk_density_so_pfh_sameyear),
                      .data$bulk_density_so_pfh_sameyear,
                # Priority 4: so_som data from other year
                ifelse(!is.na(.data$bulk_density_so_som_otheryear),
                       .data$bulk_density_so_som_otheryear,
                 # Priority 5: sw_swc data from other year
                 ifelse(!is.na(.data$bulk_density_sw_swc_otheryear),
                        .data$bulk_density_sw_swc_otheryear,
                  # Priority 6: so_pfh data from other year
                  .data$bulk_density_so_pfh_otheryear)))))) %>%
    select(-bulk_density_sw_swc_sameyear,
           -bulk_density_so_pfh_sameyear,
           -bulk_density_so_som_otheryear,
           -bulk_density_sw_swc_otheryear,
           -bulk_density_so_pfh_otheryear)

  summary(as.factor(so_som$bulk_density_source))
  
  
  
  # Coarse fragments: combine columns
  
  if (!"coarse_fragment_vol_orig" %in% names(so_som)) {
    so_som$coarse_fragment_vol_orig <- so_som$coarse_fragment_vol
  }
  
  so_som <- so_som %>%
    mutate(coarse_fragment_source =
             # Priority 1: so_som from same year
             ifelse(!is.na(.data$coarse_fragment_vol),
                    "so_som (same year)",
              # Priority 2: so_pfh from same year
              ifelse(!is.na(.data$coarse_fragment_vol_so_pfh_sameyear),
                     "so_pfh (same year)",
               # Priority 3: so_som from other year
               ifelse(!is.na(.data$coarse_fragment_vol_so_som_otheryear),
                      "so_som (other year)",
                # Priority 4: so_pfh from other year
                ifelse(!is.na(.data$coarse_fragment_vol_so_pfh_otheryear),
                       "so_pfh (other year)",
                       NA)))),
           coarse_fragment_vol =
             # Priority 1: so_som from same year
             ifelse(!is.na(.data$coarse_fragment_vol),
                    .data$coarse_fragment_vol,
              # Priority 2: so_pfh from same year
              ifelse(!is.na(.data$coarse_fragment_vol_so_pfh_sameyear),
                     .data$coarse_fragment_vol_so_pfh_sameyear,
               # Priority 3: so_som from other year
               ifelse(!is.na(.data$coarse_fragment_vol_so_som_otheryear),
                      .data$coarse_fragment_vol_so_som_otheryear,
                # Priority 4: so_pfh from other year
                .data$coarse_fragment_vol_so_pfh_otheryear)))) %>%
    select(-coarse_fragment_vol_so_pfh_sameyear,
           -coarse_fragment_vol_so_som_otheryear,
           -coarse_fragment_vol_so_pfh_otheryear)

  summary(as.factor(so_som$coarse_fragment_source))
  
  
  # Organic layer weight: combine columns
  # (Note: no other data sources for organic_layer_weight)
  
  
  # Total organic carbon: combine columns
  
  if (!"organic_carbon_total_orig" %in% names(so_som)) {
    so_som$organic_carbon_total_orig <- so_som$organic_carbon_total
  }
  
  so_som <- so_som %>%
    mutate(organic_carbon_total_source =
             # Priority 1: so_som from same year
             ifelse(!is.na(.data$organic_carbon_total),
                    "so_som (same year)",
                    # Priority 2: so_pfh from same year
                    ifelse(!is.na(.data$organic_carbon_total_so_pfh_sameyear),
                           "so_pfh (same year)",
                           NA)),
           organic_carbon_total =
             # Priority 1: so_som from same year
             ifelse(!is.na(.data$organic_carbon_total),
                    .data$organic_carbon_total,
                    # Priority 2: so_pfh from same year
                    .data$organic_carbon_total_so_pfh_sameyear)) %>%
    select(-organic_carbon_total_so_pfh_sameyear)
  
  summary(as.factor(so_som$organic_carbon_total_source))
  
  
  # Clay: combine columns
  
  if (!"part_size_clay_orig" %in% names(so_som)) {
    so_som$part_size_clay_orig <- so_som$part_size_clay
  }
  
  so_som <- so_som %>%
    mutate( part_size_clay_source =
              # Priority 1: so_som from same year
              ifelse(!is.na(.data$part_size_clay),
                     "so_som (same year)",
               # Priority 2: so_pfh from same year
               ifelse(!is.na(.data$part_size_clay_so_pfh_sameyear),
                      "so_pfh (same year)",
                # Priority 3: so_som from other year
                ifelse(!is.na(.data$part_size_clay_so_som_otheryear),
                       "so_som (other year)",
                 # Priority 4: so_pfh from other year
                 ifelse(!is.na(.data$part_size_clay_so_pfh_otheryear),
                        "so_pfh (other year)",
                        NA)))),
            part_size_clay =
             # Priority 1: so_som from same year
             ifelse(!is.na(.data$part_size_clay),
                    .data$part_size_clay,
              # Priority 2: so_pfh from same year
              ifelse(!is.na(.data$part_size_clay_so_pfh_sameyear),
                     .data$part_size_clay_so_pfh_sameyear,
               # Priority 3: so_som from other year
               ifelse(!is.na(.data$part_size_clay_so_som_otheryear),
                      .data$part_size_clay_so_som_otheryear,
                # Priority 4: so_pfh from other year
                .data$part_size_clay_so_pfh_otheryear)))) %>%
    select(-part_size_clay_so_pfh_sameyear,
           -part_size_clay_so_som_otheryear,
           -part_size_clay_so_pfh_otheryear)
    
  
  summary(as.factor(so_som$part_size_clay_source))
  
  
  
  
  
  
  

# 7. Export the processed survey forms ----

## 7.1. Save processed survey forms to Google Drive (layer 1) ----

source("./src/functions/save_to_google_drive.R")
save_to_google_drive(path_name = "layer1_data")
save_to_google_drive(objects_to_save = c("si", "so"),
                     path_name = "layer1_data")


## 7.2. Sync local data with Google Drive ----

source("./src/functions/sync_local_data.R")
sync_local_data(list_subfolders_data = "layer1_data",
                list_subfolders_output = FALSE)

## 7.3. For further processing: import the processed survey forms ----

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)
read_processed(survey_forms = c("si", "so"),
               save_to_env = TRUE)



