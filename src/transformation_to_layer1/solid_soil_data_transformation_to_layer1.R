
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

level <- "LII"


# Define surveys and survey forms within level

list_surveys <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                     s1 = c("som", "prf", "pls", "pfh", "lqa"),
                     si = c("eve", "plt", "sta", "tco"),
                     y1 = c("pl1", "st1", "ev1"),
                     sw = c("swa", "swc"))

if (level == "LI") {

  level_surveys <-
    list_surveys[names(list_surveys) %in% c("y1", "s1")]

}

if (level == "LII") {

  level_surveys <-
    list_surveys[names(list_surveys) %in% c("si", "so", "sw")]

}


list_survey_forms <- unlist(
  lapply(names(level_surveys), function(name) {
    paste0(name, "_", level_surveys[[name]])
  })
)



# 3. Import data ----

## 3.1. Use read_raw function ----
# Firstly read "y1" and "si", as their coordinates are used to list coordinates
# of "s1" and "so"

cat("Import raw data\n")

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


if (level == "LI") {

  cat("Solve issues with duplicate records\n")

  source("./src/functions/solve_record_inconsistencies.R")
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

cat("Merge duplicate records 'so_som'\n")

source("./src/functions/merge_duplicate_records.R")
so_som <- merge_duplicate_records(survey_form = "so_som",
                                  data_frame = so_som,
                                  merge = TRUE,
                                  save_to_env = FALSE)

cat("Solve other issues with duplicate records\n")

source("./src/functions/solve_record_inconsistencies.R")
so_som <- solve_record_inconsistencies(survey_form = "so_som",
                                     data_frame = so_som,
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

### 4.2.1. Gap-filling existing records ----

if (level == "LII") {

cat("Gap-fill from old database sources (AFSCDB.LII)\n")

# Import afscdb so_som data

file_path <- paste0("./data/additional_data/afscdb_LII_2_2/plot-aggregated/",
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

}

### 4.2.2. Adding missing records ----

if (level == "LII") {

# Check whether there are any "unique_survey" data in so_som_afscdb
# that are absent in so_som

# Make a vector with the unique surveys in so_som, and also the three years
# before and after, for in case only the survey year was corrected in so_som
# versus so_som_afscdb

unique_surveys_so_som <- unique(so_som$unique_survey)

source("./src/functions/expand_unique_survey_vec_adjacent_years.R")

unique_surveys <-
  expand_unique_survey_vec_adjacent_years(unique_survey_vec =
                                            unique_surveys_so_som,
                                          number_of_years = 3)



# Identify which unique surveys are missing in so_som

unique_surveys_missing <- so_som_afscdb %>%
  # Manually verified: no need to add Swiss data anymore
  filter(code_country != 50) %>%
  filter(!(unique_survey %in% unique_surveys)) %>%
  pull(unique_survey)

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
    line_nr, qif_key, code_plot_orig, download_date, layer_type,
    plot_id, unique_survey, unique_survey_repetition,
    unique_survey_layer, unique_layer_repetition, unique_layer,
    origin_merged, origin_merge_info, bulk_density_orig,
    organic_carbon_total_orig, organic_layer_weight_orig,
    coarse_fragment_vol_orig, part_size_clay_orig)

assertthat::assert_that(all(names(so_som) == names(so_som_afscdb_to_add)))

so_som <- rbind(so_som,
                so_som_afscdb_to_add)

} # End of adding afscdb records
}



## 4.3. Gap-fill "so_prf" using manually harmonised profile data Nathalie ----
# To do: assess whether machine learning predictions of a harmonised
# WRB 2014/2015 are an option.


if (level == "LII") {

cat("Gap-fill 'so_prf' using manually harmonised profile data\n")

assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                           "SO_PRF_ADDS.xlsx")),
                        msg = paste0("'./data/additional_data/",
                                     "SO_PRF_ADDS.xlsx' ",
                                     "does not exist."))

# This file was created by Nathalie on 17 Oct 2023

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
                           EFTC, "_",
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
                    "forest_type",
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




## 5.2. Inconsistencies in coordinates ----

# Surveys with coordinates:
# "y1_pl1" "s1_pls" "s1_prf" "si_plt" "so_pls" "so_prf"


# Get inconsistencies

# source("./src/functions/get_coordinate_inconsistencies.R")

# get_coordinate_inconsistencies(boundary_buffer_meter = 3000,
#                                save_to_env = TRUE)




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
s1_pfh2 <- s1_pfh
s1_som2 <- s1_som
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
so_pfh2 <- so_pfh
so_som2 <- so_som
}


# source("./src/functions/bind_objects_starting_with.R")
# bind_objects_starting_with("list_layer_inconsistencies", save_to_env = TRUE)




# At this stage, link forest floor layers in "som" with those of "pfh"
# in the same survey.
# This can be used to gap-fill forest floor layer limits as well as bulk
# densities





## 5.4. Inconsistencies in range/presence of data ----
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
s1_pfh3 <- s1_pfh
s1_som3 <- s1_som
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
so_pfh3 <- so_pfh
so_som3 <- so_som
}

# source("./src/functions/bind_objects_starting_with.R")
# bind_objects_starting_with("list_range_inconsistencies", save_to_env = TRUE)







## 5.5. Harmonise data below LOQ ----

source("./src/functions/harmonise_below_loqs.R")

if (level == "LI") {

  s1_som <- harmonise_below_loqs(survey_form = "s1_som",
                                 data_frame = s1_som)

  s1_pfh <- harmonise_below_loqs(survey_form = "s1_pfh",
                                 data_frame = s1_pfh,
                                 parameters = c("horizon_clay",
                                                "horizon_silt",
                                                "horizon_sand",
                                                "horizon_c_organic_total",
                                                "horizon_n_total"))
}

if (level == "LII") {

so_som <- harmonise_below_loqs(survey_form = "so_som",
                               data_frame = so_som)

so_pfh <- harmonise_below_loqs(survey_form = "so_pfh",
                               data_frame = so_pfh,
                               parameters = c("horizon_clay",
                                              "horizon_silt",
                                              "horizon_sand",
                                              "horizon_c_organic_total",
                                              "horizon_n_total"))
}





## 5.6. Inconsistencies in derived variables ----
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

if (level == "LII") {

cat("Apply additional manual corrections\n")

# Romania: organic_carbon_total of forest floors seem to be reported in %,
# while those of the mineral layers are fine.
# This is in line with the data in AFSCDB_LII.

# Forest floor layers

unique_layers_to_convert <- so_som %>%
  filter(code_country == 52) %>%
  filter(layer_type == "forest_floor") %>%
  mutate(unit_issue_toc =
           # Upper limit of organic_carbon_total plausible range in %
           ifelse(.data$organic_carbon_total < 59,
                  TRUE,
                  FALSE))

if (length(which(unique_layers_to_convert$unit_issue_toc == TRUE)) >=
    0.9 * nrow(unique_layers_to_convert) &&
    nrow(unique_layers_to_convert) >= 2) {

  unique_layers_to_convert <-
    unique(unique_layers_to_convert$unique_layer_repetition)

  so_som <- so_som %>%
    mutate(organic_carbon_total =
             ifelse(.data$unique_layer_repetition %in%
                      unique_layers_to_convert,
                    10 * .data$organic_carbon_total,
                    .data$organic_carbon_total))
}

# Plot 52_10
# n_total of mineral layers should be divided by 10
# (in comparison with AFSCDB_LII)

unique_layers_to_convert <- so_som %>%
  filter(plot_id == "52_10") %>%
  filter(layer_type != "forest_floor") %>%
  mutate(unit_issue_tn =
           # Upper limit of n_total plausible range
           ifelse(.data$n_total > 10,
                  TRUE,
                  FALSE))

if (length(which(unique_layers_to_convert$unit_issue_tn == TRUE)) >=
    0.7 * nrow(unique_layers_to_convert) &&
    nrow(unique_layers_to_convert) >= 2) {

  unique_layers_to_convert <-
    unique(unique_layers_to_convert$unique_layer_repetition)

  so_som <- so_som %>%
    mutate(n_total =
             ifelse(.data$unique_layer_repetition %in%
                      unique_layers_to_convert,
                    0.1 * .data$n_total,
                    .data$n_total))
}

# Plot 52_12
# organic_carbon_total of mineral layers should be a factor 10 higher
# (in comparison with AFSCDB_LII)

unique_layers_to_convert <- so_som %>%
  filter(plot_id == "52_12") %>%
  filter(layer_type != "forest_floor") %>%
  mutate(unit_issue_toc =
           # Upper limit of organic_carbon_total plausible range in %
           ifelse(.data$organic_carbon_total < 15,
                  TRUE,
                  FALSE))

if (length(which(unique_layers_to_convert$unit_issue_toc == TRUE)) >=
    0.9 * nrow(unique_layers_to_convert) &&
    nrow(unique_layers_to_convert) >= 2) {

  unique_layers_to_convert <-
    unique(unique_layers_to_convert$unique_layer_repetition)

  so_som <- so_som %>%
    mutate(organic_carbon_total =
             ifelse(.data$unique_layer_repetition %in%
                      unique_layers_to_convert,
                    10 * .data$organic_carbon_total,
                    .data$organic_carbon_total))
}

}

if (level == "LI") {

  s1_som <- s1_som %>%
    mutate(organic_layer_weight =
             ifelse(layer_type == "mineral",
                    NA,
                    .data$organic_layer_weight))

}

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

cat("Gap-fill internally\n")

source("./src/functions/gapfill_internally.R")

if (level == "LI") {

s1_som <- gapfill_internally(survey_form = "s1_som",
                           data_frame = s1_som,
                           save_to_env = FALSE)

write.csv2(s1_pfh_fixed,
           paste0("./output/gap_filling_details/",
                  "s1_pfh_fixed_depths.csv"),
           row.names = FALSE,
           na = "")

s1_pfh <- gapfill_internally(survey_form = "s1_pfh",
                             data_frame = s1_pfh,
                             save_to_env = FALSE)

write.csv2(s1_som_pedogenic,
           paste0("./output/gap_filling_details/",
                  "s1_som_pedogenic.csv"),
           row.names = FALSE,
           na = "")

}



if (level == "LII") {

so_som <- gapfill_internally(survey_form = "so_som",
                             data_frame = so_som,
                             save_to_env = FALSE)

write.csv2(so_pfh_fixed,
           paste0("./output/gap_filling_details/",
                  "so_pfh_fixed_depths.csv"),
           row.names = FALSE,
           na = "")

so_pfh <- gapfill_internally(survey_form = "so_pfh",
                             data_frame = so_pfh,
                             save_to_env = FALSE)

write.csv2(so_som_pedogenic,
           paste0("./output/gap_filling_details/",
                  "so_som_pedogenic.csv"),
           row.names = FALSE,
           na = "")
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

