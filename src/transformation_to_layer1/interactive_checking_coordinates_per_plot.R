
# Interactive checking of coordinate inconsistencies per plot

# Script initiation date: 20240109

# Problem:
# Because of several issues in the coordinates or plot codes, reported plot
# codes in "layer 0" may not always be unambiguously linked to a single
# coordinate pair in both Level I and Level II.
# Inconsistencies may be due to:
# - mistakes/typos in reported coordinate, e.g. a "minus" sign that is
#   omitted...
# - updates of reported coordinates due to improved GPS accuracy are not
#   applied to each of the reported coordinates across survey years and
#   survey forms
# - moving a plot to a different location without updating the reported plot
#   code

# Purpose of this script:
# to come up with one assumedly correct coordinate pair and one code_plot
# per surveyed location.

# This script evaluates all of the reported coordinate pairs for a plot
# If

# Apply the script after PIR gap-filling!

# in a loop, show:
# 1. columns (arrange by change_date):
# - survey_form/data source
# - survey_year
# - coordinates DDMMSS
# - change_date
# - inconsistencies related to coordinate pairs
# - remark Bruno
#
# 2. map

# Data sources LI
# - y1
# - s1
# - PIR
# - bd
# - Heavy metal study 2020
# - FSCDB

# Data sources LII
# - si
# - so
# - PIR
# - ICP Forests meteo database

# Create a table with columns:
# - code_country
# - code_plot
# - plot_id
# - coordinates DDMMSS
# - observation_date



source("./src/functions/dec_coordinate.R")
source("./src/functions/add_dec_coord_columns.R")
source("./src/functions/gives_warning.R")
source("./src/functions/as_sf.R")


# Define required packages
stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"),
          require("aqp"),
          require("ggmap"),
          require("leaflet"))

compare_ranks <- function(df, additional_info) {

  # Extract the row with the specified rank
  focus_row <- df[df$rank == additional_info, ]

  # Select the relevant columns for comparison
  comparison_cols <- c("rank_change_date_recent",
                       "rank_heavy_metals",
                       "rank_survey_recent",
                       "rank_has_fscdb",
                       "rank_has_bd",
                       "rank_has_sx",
                       "rank_minus",
                       "rank_error")

  # Mapping of column names to descriptions
  col_descriptions <- c(
    "rank_change_date_recent" =
      "More recent change_date in solid soil surveys",
    "rank_heavy_metals" =
      "Manually corrected in heavy metal study",
    "rank_survey_recent" =
      "Included in surveys after the year 2000",
    "rank_has_fscdb" =
      "Included in harmonised fscdb",
    "rank_has_bd" =
      "Remeasured during biodiversity survey",
    "rank_has_sx" =
      "Better represented in s1 or so",
    "rank_minus" =
      "Minus not forgotten",
    "rank_error" =
      paste0("No mistakes with 0-60 range coordinates and/or ",
              "within the reported country boundaries"))

  # Convert selected columns to numeric
  df[comparison_cols] <- lapply(df[comparison_cols], as.numeric)

  # Calculate maximum values for each comparison column
  max_values <- reframe(df, across(comparison_cols, max))
  focus_row_values <- select(focus_row, comparison_cols)

  selected_coord_advantages <- names(max_values)[max_values > focus_row_values]


  # Get the corresponding descriptions
  result_description <-
    paste(col_descriptions[selected_coord_advantages], collapse = "; ")

  return(result_description)
}



# 1. Input level ----

level <- "LI"



# 2. Assemble the data ----

## 2.1. Level I ----

if (level == "LI") {

  # s1
  source("./src/functions/read_raw.R")
  read_raw("s1", save_to_env = TRUE)


  # y1
  source("./src/functions/read_raw.R")
  read_raw("y1", save_to_env = TRUE)


  # LI plots to ignore in external sources
  # because we are sure that the coordinates in s1 are correct

  plots_to_ignore_li <- unique(c(
    # UK LI
    read.csv(paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                    "LI_6_plot_coord_harmonisation_key_UK.csv"),
             sep = ";") %>%
      pull(plot_id_orig),
    read.csv(paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                    "LI_6_plot_coord_harmonisation_key_UK.csv"),
             sep = ";") %>%
      pull(plot_id),
    # Poland LI
    read.csv(paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                    "LI_53_plot_coord_harmonisation_key_Poland.csv"),
             sep = ";") %>%
      pull(plot_id),
    # German LI
    read.csv(paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                    "LI_4_plot_coord_harmonisation_key_Germany.csv"),
             sep = ";") %>%
      pull(plot_id)))


  # PIR
  dir <- "./data/additional_data/20230302_checked_pirs.xlsx"

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir,
                                       "' does not exist."))

  pir_checked <-
    openxlsx::read.xlsx(dir,
                        sheet = 1) %>%
    mutate(plot_id = paste0(code_country, "_",
                            code_plot)) %>%
    filter(grepl("^s1|^y1", survey_form)) %>%
    filter(parameter %in% c("latitude", "longitude")) %>%
    filter(!is.na(updated_value))

  assertthat::assert_that(nrow(pir_checked) == 0)


  # bd
  dir <- paste0("./data/additional_data/",
                "126_bd_20230405143047/bd_gpl.csv")

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir,
                                       "' does not exist."))

  coord_bd <-
    read.csv(dir,
             sep = ";") %>%
    mutate(plot_id = paste0(code_country, "_", code_plot),
           change_date = as.character(as.Date(change_date))) %>%
    rename(longitude = longitude_plot,
           latitude = latitude_plot) %>%
    mutate(remark = NA) %>%
    mutate(comb = paste0(plot_id, "_", survey_year, "_",
                         longitude, "_", latitude)) %>%
    filter(!is.na(longitude) &
             !is.na(latitude)) %>%
    distinct(comb, .keep_all = TRUE) %>%
    select(code_country, plot_id, survey_year, longitude, latitude,
           remark,
           change_date)


  # FSCDB
  dir <- paste0("./data/additional_data/",
                "coordinates_plots/s1/LI_full_plot_coordinate_list.csv")

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir,
                                       "' does not exist."))

  coord_fscdb <-
    read.csv(dir,
             sep = ";") %>%
    mutate(code_country = as.numeric(str_extract(plot_id, "\\d+"))) %>%
    select(code_country, plot_id, survey_year, longitude, latitude) %>%
    filter(!is.na(longitude) &
             !is.na(latitude)) %>%
    mutate(comb = paste0(plot_id, "_", longitude, "_", latitude)) %>%
    distinct(comb, .keep_all = TRUE) %>%
    select(-comb) %>%
    mutate(remark = NA) %>%
    mutate(change_date = NA) %>%
    as_tibble


  # Heavy metal study 2020
  dir <- paste0("./data/additional_data/coordinates_plots/s1/",
                "s1_pls_corrected_coords_Heavy_Metal_study.xlsx")

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir,
                                       "' does not exist."))

  coord_hm <-
    openxlsx::read.xlsx(dir,
                        sheet = 2) %>%
    dplyr::slice(-1) %>%
    rename(plot_id = "BPLOTID") %>%
    rename(survey_year = "Survey") %>%
    rename(latitude = "Corrected") %>%
    rename(longitude = "X6") %>%
    rename(source = "Reference") %>%
    rename(remark = "X8") %>%
    mutate(remark = ifelse(!is.na(remark),
                           paste0("Source: ", source,
                                  "_Remark: ", remark),
                           paste0("Source: ", source))) %>%
    mutate(survey_year = as.numeric(survey_year),
           longitude = as.numeric(longitude),
           latitude = as.numeric(latitude)) %>%
    mutate(code_country = as.numeric(str_extract(plot_id, "\\d+"))) %>%
    select(code_country, plot_id, survey_year, longitude, latitude, remark) %>%
    mutate(change_date = NA) %>%
    as_tibble



 # Compile

  coord_sources <-
    bind_rows(
      # s1_pls
      s1_pls %>%
        mutate(source = "s1_pls",
               remark = NA) %>%
        mutate(change_date = ifelse(is.na(change_date),
                                    as.character(as.Date("2008-05-15")),
                                    change_date)) %>%
        mutate(comb = paste0(plot_id, "_", survey_year, "_",
                             longitude, "_", latitude)) %>%
        distinct(comb, .keep_all = TRUE) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # y1_pl1
      y1_pl1 %>%
        mutate(source = "y1_pl1",
               remark = NA) %>%
        filter(!plot_id %in% plots_to_ignore_li) %>%
        mutate(survey_year = NA) %>%
        mutate(comb = paste0(plot_id, "_", survey_year, "_",
                             longitude, "_", latitude)) %>%
        distinct(comb, .keep_all = TRUE) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # s1_prf
      s1_prf %>%
        mutate(source = "s1_prf",
               remark = NA) %>%
        mutate(change_date = ifelse(is.na(change_date),
                                    as.character(as.Date("2008-05-15")),
                                    change_date)) %>%
        filter(!is.na(longitude) &
                 !is.na(latitude)) %>%
        mutate(comb = paste0(plot_id, "_", survey_year, "_",
                             longitude, "_", latitude)) %>%
        distinct(comb, .keep_all = TRUE) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # coord_bd
      coord_bd %>%
        mutate(source = "bd") %>%
        filter(!plot_id %in% plots_to_ignore_li) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # coord_fscdb
      coord_fscdb %>%
        mutate(source = "fscdb") %>%
        filter(!plot_id %in% plots_to_ignore_li) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # coord_hm
      coord_hm %>%
        mutate(source = "heavy_metals") %>%
        filter(!plot_id %in% plots_to_ignore_li) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date)) %>%
    as_tibble %>%
    add_dec_coord_columns %>%
    mutate(longitude_dec = as.numeric(longitude_dec),
           latitude_dec = as.numeric(latitude_dec))

}




## 2.2. Level II ----

if (level == "LII") {

  # so
  source("./src/functions/read_raw.R")
  read_raw("so", save_to_env = TRUE)


  # si
  source("./src/functions/read_raw.R")
  read_raw("si", save_to_env = TRUE)

  # LII plots to ignore in external sources
  # because we are sure that the coordinates in s1 are correct

  plots_to_ignore_lii <- unique(c(
    # Poland LII
    read.csv(paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                    "LII_53_plot_coord_harmonisation_key_Poland.csv"),
             sep = ";") %>%
      pull(plot_id)))


  # PIR
  dir <- "./data/additional_data/20230302_checked_pirs.xlsx"

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir,
                                       "' does not exist."))

  pir_checked <-
    openxlsx::read.xlsx(dir,
                        sheet = 1)

  coord_pir <- pir_checked %>%
    mutate(plot_id = paste0(code_country, "_",
                            code_plot)) %>%
    filter(grepl("^so|^si", survey_form)) %>%
    filter(parameter %in% c("latitude", "longitude")) %>%
    filter(!is.na(updated_value)) %>%
    select(survey_form, partner, partner_code, country,
           code_country, survey_year, code_plot, plot_id, parameter,
           updated_value) %>%
    pivot_wider(
      names_from = parameter,
      values_from = updated_value)

  coord_pir_add <-
    bind_rows(
      so_pls %>%
        filter(plot_id %in% unique(coord_pir$plot_id)) %>%
        select(plot_id, longitude, latitude, change_date),
      so_prf %>%
        filter(plot_id %in% unique(coord_pir$plot_id)) %>%
        select(plot_id, longitude, latitude, change_date),
      si_plt %>%
        filter(plot_id %in% unique(coord_pir$plot_id)) %>%
        select(plot_id, longitude, latitude, change_date)) %>%
    mutate(change_date = as.Date(change_date, format = "%Y-%m-%d")) %>%
    arrange(plot_id) %>%
    group_by(plot_id) %>%
    arrange(desc(change_date)) %>%
    slice_head() %>%
    ungroup %>%
    select(-change_date)

  coord_pir <- coord_pir %>%
    left_join(coord_pir_add %>%
                rename(longitude_pcc = longitude,
                       latitude_pcc = latitude),
              by = "plot_id") %>%
    mutate(longitude = ifelse(is.na(longitude),
                              longitude_pcc,
                              longitude),
           latitude = ifelse(is.na(latitude),
                             latitude_pcc,
                             latitude)) %>%
    mutate(longitude = as.numeric(longitude),
           latitude = as.numeric(latitude)) %>%
    mutate(change_date = as.character(as.Date("2023-03-02")),
           remark = NA) %>%
    select(code_country, plot_id, survey_year, longitude, latitude, remark,
           change_date)


  # AFSCDB_LII_2_2

  dir <- paste0("./data/additional_data/afscdb_LII_2_2/plot-aggregated/",
                "AFSCDB_LII_2_2_080515_som.csv")

  plot_ids_afscdb <- read.csv(dir,
                              sep = ";", na.strings = "") %>%
    mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
    distinct(plot_id) %>%
    pull(plot_id)

  dir <- paste0("./data/additional_data/afscdb_LII_2_2/plot-aggregated/",
                      "AFSCDB_LII_2_2_080515_pls.csv")

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir, "' ",
                                       "does not exist."))

  coord_afscdb <- read.csv(dir,
                           sep = ";", na.strings = "") %>%
    mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
    filter(plot_id %in% plot_ids_afscdb) %>%
    mutate(survey_year = NA_integer_,
           remark = NA,
           change_date = NA_character_) %>%
    filter(!is.na(longitude) &
             !is.na(latitude)) %>%
    select(code_country, plot_id, survey_year, longitude, latitude, remark,
           change_date)



  # ICP Forests meteo database
  dir <- paste0("./data/additional_data/",
                "coordinates_plots/so/LII_all_plot_coordinates_meteo.csv")

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir,
                                       "' does not exist."))

  coord_meteo <-
    read.csv(dir,
             sep = ";") %>%
    as_tibble %>%
    filter(!is.na(longitude) &
             !is.na(latitude)) %>%
    mutate(latitude = str_replace_all(latitude, ",", "."),
           longitude = str_replace_all(longitude, ",", ".")) %>%
    mutate(longitude_dec = as.numeric(longitude),
           latitude_dec = as.numeric(latitude)) %>%
    # longitude in ddmmss
    mutate(
      sign = ifelse(longitude_dec < 0, "-", ""),
      longitude_abs = abs(longitude_dec),
      longitude = str_c(
        sign,
        floor(longitude_abs),
        str_pad(floor((longitude_abs - floor(longitude_abs)) * 60), 2,
                pad = "0"),
        str_pad(round((longitude_abs * 3600) %% 60), 2,
                pad = "0"),
        sep = "")) %>%
    # latitude in ddmmss
    mutate(
      sign = ifelse(latitude_dec < 0, "-", ""),
      latitude_abs = abs(latitude_dec),
      latitude = str_c(
        sign,
        floor(latitude_abs),
        str_pad(floor((latitude_abs - floor(latitude_abs)) * 60), 2,
                pad = "0"),
        str_pad(round((latitude_abs * 3600) %% 60), 2,
                pad = "0"),
        sep = "")) %>%
    select(-sign, -latitude_abs, -longitude_abs,
           latitude_dec, longitude_dec) %>%
    mutate(longitude = as.numeric(longitude),
           latitude = as.numeric(latitude)) %>%
    left_join(d_partner %>%
                rename(partner_code = code) %>%
                rename(code_country = country_code) %>%
                select(partner_code, code_country),
              by = "partner_code") %>%
    mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
    mutate(comb = paste0(plot_id, "_", longitude, "_", latitude)) %>%
    distinct(comb, .keep_all = TRUE) %>%
    select(-comb) %>%
    mutate(remark = NA,
           survey_year = NA) %>%
    mutate(change_date = NA) %>%
    select(code_country, plot_id, survey_year, longitude, latitude, remark,
           change_date)





  # Compile

  coord_sources <-
    bind_rows(
      # so_pls
      so_pls %>%
        mutate(source = "so_pls",
               remark = NA) %>%
        mutate(change_date = ifelse(is.na(change_date),
                                    as.character(as.Date("2008-05-15")),
                                    change_date)) %>%
        mutate(comb = paste0(plot_id, "_", survey_year, "_",
                             longitude, "_", latitude)) %>%
        distinct(comb, .keep_all = TRUE) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # si_plt
      si_plt %>%
        mutate(source = "si_plt",
               remark = NA) %>%
        filter(!plot_id %in% plots_to_ignore_lii) %>%
        mutate(survey_year = NA) %>%
        mutate(comb = paste0(plot_id, "_", survey_year, "_",
                             longitude, "_", latitude)) %>%
        distinct(comb, .keep_all = TRUE) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # so_prf
      so_prf %>%
        mutate(source = "so_prf",
               remark = NA) %>%
        mutate(change_date = ifelse(is.na(change_date),
                                    as.character(as.Date("2008-05-15")),
                                    change_date)) %>%
        filter(!is.na(longitude) &
                 !is.na(latitude)) %>%
        mutate(comb = paste0(plot_id, "_", survey_year, "_",
                             longitude, "_", latitude)) %>%
        distinct(comb, .keep_all = TRUE) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # coord_pir
      coord_pir %>%
        mutate(source = "pir") %>%
        filter(!plot_id %in% plots_to_ignore_lii) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # coord_afscdb
      coord_afscdb %>%
        mutate(source = "afscdb") %>%
        mutate(code_country = as.integer(code_country)) %>%
        mutate(change_date = ifelse(is.na(change_date),
                                    as.character(as.Date("2008-05-15")),
                                    change_date)) %>%
        filter(!plot_id %in% plots_to_ignore_lii) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date),
      # coord_meteo
      coord_meteo %>%
        mutate(source = "meteo") %>%
        filter(!plot_id %in% plots_to_ignore_lii) %>%
        select(source,
               code_country, plot_id, survey_year, longitude, latitude,
               remark,
               change_date)) %>%
    as_tibble %>%
    add_dec_coord_columns %>%
    mutate(longitude_dec = as.numeric(longitude_dec),
           latitude_dec = as.numeric(latitude_dec))


}





# 3. Add countries and partners ----

world_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "world_spat") %>%
  rename(country = "NAME_0") %>%
  select(country, geom) %>%
  mutate(country = case_when(
    country == "Åland" ~ "Finland",
    country == "Slovakia" ~ "Slovak Republic",
    TRUE ~ country
  ))


german_partners_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "german_partners_spat") %>%
  rename(partner = StateName1) %>%
  filter(partner %in% c("Schleswig-Holstein",
                        "Sachsen",
                        "Thüringen",
                        "Mecklenburg-Vorpommern",
                        "Nordrhein-Westfalen",
                        "Rheinland-Pfalz",
                        "Saarland",
                        "Hamburg",
                        "Baden-Württemberg",
                        "Bayern")) %>%
  select(partner, geom)

eberswalde_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "eberswalde_spat") %>%
  mutate(partner = "Eberswalde") %>%
  relocate(partner, .before = geom)

flanders_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "flanders_spat") %>%
  mutate(partner = "Flanders") %>%
  relocate(partner, .before = geom)

wallonia_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "wallonia_spat") %>%
  mutate(partner = "Wallonia") %>%
  relocate(partner, .before = geom)

canary_islands_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "canary_islands_spat") %>%
  mutate(partner = "Canary Islands") %>%
  select(partner, geom)

azores_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "azores_spat") %>%
  mutate(partner = "Azores") %>%
  select(partner, geom)

combined_spat <- rbind(german_partners_spat, eberswalde_spat, flanders_spat,
                       wallonia_spat, canary_islands_spat, azores_spat)



if (level == "LI") {
  data_availability <- data_availability_s1
} else if (level == "LII") {
  data_availability <- data_availability_so
}

records_to_exclude <-
  c(data_availability %>%
      group_by(plot_id) %>%
      summarise(count = n(),
                contains_98 = any(partner_code == 98)) %>%
      filter(count == 2 &
               contains_98 == TRUE) %>%
      pull(plot_id),
    data_availability %>%
      group_by(plot_id) %>%
      summarise(count = n(),
                contains_11_and_95 = (any(partner_code == 11) &
                                        any(partner_code == 95))) %>%
      filter(count == 2 &
               contains_11_and_95 == TRUE) %>%
      pull(plot_id))


data_availability <- data_availability %>%
  filter(!plot_id %in% records_to_exclude |
           (plot_id %in% records_to_exclude &
              !partner_code %in% c(98, 11)))


length(unique(data_availability$plot_id))


coord_sources <- coord_sources %>%
  as_sf %>%
  st_join(combined_spat) %>%
  st_join(world_spat) %>%
  st_drop_geometry %>%
  rename(country_location = country) %>%
  rename(partner_location = partner) %>%
  mutate(partner_location = ifelse(is.na(partner_location),
                                   country_location,
                                   partner_location)) %>%
  mutate(country_location = ifelse(partner_location == "Azores" &
                                     country_location == "Portugal",
                                   "Azores (Portugal)",
                                   country_location),
         country_location = ifelse(partner_location == "Canary Islands" &
                                     country_location == "Spain",
                                   "Canaries (Spain)",
                                   country_location)) %>%
  left_join(data_availability %>%
              select(plot_id, partner_code,
                     country, partner_short, partner),
            by = "plot_id") %>%
  mutate(country_error = ifelse(country == country_location,
                                NA,
                                "Error: not same country")) %>%
  relocate(country, .before = "plot_id") %>%
  relocate(code_country, .after = partner) %>%
  relocate(partner_code, .after = code_country) %>%
  relocate(country_error, .after = ddmmss_error) %>%
  relocate(remark, .before = ddmmss_error) %>%
  relocate(longitude_dec, .after = last_col()) %>%
  relocate(latitude_dec, .after = last_col())







# 4. Evaluate per plot_id ----


plot_ids <- coord_sources %>%
  filter(grepl("^s1|^so|afscdb", source)) %>%
  mutate(code_plot = as.numeric(str_extract(plot_id, "(?<=_)\\d+$"))) %>%
  arrange(code_country, code_plot) %>%
  distinct(plot_id) %>%
  pull(plot_id)


coord_harmonised <- NULL

for (i in seq_along(plot_ids)) {

  coord_sources_i <- coord_sources %>%
    filter(plot_id == plot_ids[i]) %>%
    arrange(desc(change_date))


  # Manual corrections

  if (plot_ids[i] == "53_816") {

    coord_sources_i <- coord_sources_i %>%
      # These coordinates are wrong
      filter(survey_year != 2017)
  }



  # Diagonal distance of bounding box

  diagonal_dist_m <-
    st_distance(
      # Bounding box: corner left below
      as_sf(data.frame(longitude_dec = min(coord_sources_i$longitude_dec),
                       latitude_dec = min(coord_sources_i$latitude_dec))),
      # Bounding box: corner right up
      as_sf(data.frame(longitude_dec = max(coord_sources_i$longitude_dec),
                       latitude_dec = max(coord_sources_i$latitude_dec)))) %>%
    as.numeric

  ## 4.1. Consistent coordinates and no errors ----

  if (length(unique(coord_sources_i$longitude)) == 1 &&
      length(unique(coord_sources_i$latitude)) == 1 &&
      all(is.na(coord_sources_i$ddmmss_error)) &&
      all(is.na(coord_sources_i$country_error))) {

    coord_harmonised <-
      rbind(coord_harmonised,
            data.frame(
              country = unique(coord_sources_i$country),
              plot_id = plot_ids[i],
              longitude = unique(coord_sources_i$longitude),
              latitude = unique(coord_sources_i$latitude),
              eval = "Consistent coordinates and no issues",
              sources = paste(unique(sort(coord_sources_i$source)),
                              collapse = "-"),
              sources_harm = paste(unique(sort(coord_sources_i$source)),
                                   collapse = "-"),
              observation_date =
                as.character(format(Sys.Date(), "%Y-%m-%d")),
              dist_diagonal_m = round(diagonal_dist_m)))
  } else {


    # Create auxiliary objects

    # Range of longitude and latitude coordinates

    range_difference_lon <-
      max(coord_sources_i$longitude_dec) - min(coord_sources_i$longitude_dec)

    range_difference_lat <-
      max(coord_sources_i$latitude_dec) - min(coord_sources_i$latitude_dec)

    # Subset of dataframe at most recent change_date

    change_date_recent_sx <- coord_sources_i %>%
      filter(grepl("^s1|^so|afscdb", source)) %>%
      slice_max(order_by =
                  as.Date(change_date, format = "%Y-%m-%d")) %>%
      distinct(change_date) %>%
      pull(change_date)

    coord_recent_i <- coord_sources_i %>%
      filter(change_date == change_date_recent_sx)



    # 4.2. Consistent coordinates at most recent soil data update ----
    ##      and within 2000 meters distance previously and no errors

    if (diagonal_dist_m < 2000 &&
        # any(startsWith(coord_recent_i$source, c("s1", "so"))) &&
        length(unique(coord_recent_i$longitude)) == 1 &&
        length(unique(coord_recent_i$latitude)) == 1 &&
        all(is.na(coord_sources_i$ddmmss_error)) &&
        all(is.na(coord_sources_i$country_error))) {

      sources_harm_i <- coord_sources_i %>%
        filter(longitude == unique(coord_recent_i$longitude) &
                 latitude == unique(coord_recent_i$latitude)) %>%
        pull(source)

      coord_harmonised <-
        rbind(coord_harmonised,
              data.frame(
                country = unique(coord_sources_i$country),
                plot_id = plot_ids[i],
                longitude = unique(coord_recent_i$longitude),
                latitude = unique(coord_recent_i$latitude),
                eval = paste0("Consistent coordinates at recent soil update, ",
                              "and less accurate (< 2000 meter) before, ",
                              "and no issues"),
                sources = paste(unique(sort(coord_sources_i$source)),
                                collapse = "-"),
                sources_harm = paste(unique(sort(sources_harm_i)),
                                     collapse = "-"),
                observation_date =
                  as.character(format(Sys.Date(), "%Y-%m-%d")),
                dist_diagonal_m = round(diagonal_dist_m)))

    } else {

      # 4.3 Else, rank the different coordinate pairs based on criteria ----

      # Summary across unique locations

      coord_summ_i <- coord_sources_i %>%
        mutate(change_date_sx = ifelse(grepl("s1|so", source),
                                       change_date,
                                       NA)) %>%
        group_by(longitude, latitude) %>%
        reframe(sources = paste(source, collapse = "-"),
                survey_years = paste(sort(unique(survey_year)),
                                     collapse = "_"),
                longitude_dec = first(longitude_dec),
                latitude_dec = first(latitude_dec),
                change_date_recent =
                  ifelse(all(is.na(change_date_sx)),
                         NA,
                         change_date_sx[which.max(
                           as.numeric(as.Date(change_date_sx)))]),
                any_error = (any(!is.na(ddmmss_error)) |
                               any(!is.na(country_error))),
                any_remark = paste(na.omit(remark), collapse = "-")) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(
          # If the coordinate pair has been reported in survey years
          # after 2000: better
          survey_recent =
            any(as.numeric(unlist(strsplit(survey_years, "_"))) > 2000)
        ) %>%
        ungroup() %>%
        mutate(
          rank_survey_recent = rank(desc(survey_recent),
                                    ties.method = "min"),

          # The more recent the change_date of s1 or so, the better
          rank_change_date_recent = rank(desc(as.Date(change_date_recent)),
                                         ties.method = "min"),

          # If "heavy_metals" appears in sources (i.e. it was corrected for
          # the heavy metal study): better
          has_heavy_metals = grepl("heavy_metals", sources),
          rank_heavy_metals = rank(desc(has_heavy_metals), ties.method = "min"),

          # The closer to the average of the coordinates, the better
          # distance_to_avg =
          #   sqrt((longitude_dec - mean(longitude_dec))^2 +
          #          (latitude_dec - mean(latitude_dec))^2),
          # rank_dist = rank(round(distance_to_avg, 5), ties.method = "min"),

          # If negative (and the alternatives not), better (since the minus
          # may be forgotten in the alternatives)
          long_neg = ifelse(longitude < 0, -1, 1),
          lat_neg = ifelse(latitude < 0, -1, 1),
          rank_minus = rank(0.5 * rank(long_neg, ties.method = "min") +
                              0.5 * rank(lat_neg, ties.method = "min"),
                            ties.method = "min"),

          # The more abundant (i.e. the longer the number of sources),
          # the better
          # num_sources = str_count(sources, "-") + 1,
          # rank_sources = rank(desc(num_sources), ties.method = "min"),

          # If "fscdb" appears in sources: better
          has_fscdb = grepl("fscdb", sources),
          rank_has_fscdb = rank(desc(has_fscdb), ties.method = "min"),

          # If "bd" appears in sources: better
          has_bd = grepl("bd", sources),
          rank_has_bd = rank(desc(has_bd), ties.method = "min"),

          # If "sx_pls" appears in sources: better
          has_sx_pls = grepl("s1_pls|so_pls", sources),
          has_sx_prf = grepl("s1_prf|so_prf", sources),
          has_sx = case_when(
            has_sx_pls & has_sx_prf ~ 1,
            has_sx_pls ~ 2,
            has_sx_prf ~ 3,
            TRUE ~ 4),
          rank_has_sx = rank(has_sx, ties.method = "min"),

          # If any_error is false: better
          rank_error = rank(any_error, ties.method = "min"),

          # Create the composite index (using weights)
          # The lower, the better
          composite_index =
            0.2 * rank_change_date_recent +
            0.2 * rank_survey_recent +
            0.15 * rank_has_fscdb +
            0.15 * rank_minus +
            0.1 * rank_heavy_metals +
            0.1 * rank_has_bd +
            0.05 * rank_has_sx +
            0.05 * rank_error) %>%
        select(-long_neg, -lat_neg) %>%
        arrange(composite_index) %>%
        mutate(rank = row_number(),
               label = paste0(rank, " · ",
                              round(composite_index, 1), " · ",
                              sources)) %>%
        mutate(colour =
                 ifelse(!is.na(change_date_recent) &
                          change_date_recent ==
                          unique(coord_recent_i$change_date),
                        # contains coordinates of most recent change_date
                        "red",
                        # older coordinates
                        "blue"))



      # Manually give priority to certain coordinate pairs of plots with
      # otherwise the same coordinates like other plots

      if (unlist(strsplit(plot_ids[i], "_"))[1] == 58 &
          any(grepl("s1", coord_summ_i$sources))) {

        # From direct communication with Czech partner:
        # y1_pl1 is the most correct source

        row_ind <- which(grepl("y1_pl1", coord_summ_i$sources))[1]
        coord_summ_i$composite_index[row_ind] <- 0.99
        coord_summ_i <- arrange(coord_summ_i, composite_index)

      }

      if (plot_ids[i] %in% c("59_167", "59_68") &
          any(grepl("s1", coord_summ_i$sources))) {

        coord_summ_i <- coord_summ_i %>%
          mutate(composite_index =
                   ifelse(grepl("fscdb", sources),
                          0.99,
                          composite_index)) %>%
          arrange(composite_index)
      }

      if (plot_ids[i] %in% c("66_101", "66_102", "66_103", "66_104") &
          any(grepl("so", coord_summ_i$sources))) {

        coord_summ_i <- coord_summ_i %>%
          mutate(composite_index =
                   ifelse(grepl("si_plt", sources),
                          0.99,
                          composite_index)) %>%
          arrange(composite_index)
      }

      if (plot_ids[i] == "7_16" &
          any(grepl("so", coord_summ_i$sources))) {

        coord_summ_i <- coord_summ_i %>%
          mutate(composite_index =
                   ifelse(grepl("pir", sources),
                          0.99,
                          composite_index)) %>%
          arrange(composite_index)
      }



      # Check if there is a clear "winner" with the lowest composite_index
      # ranking number

      if (nrow(coord_summ_i) == 1 ||
          sum(coord_summ_i$composite_index ==
              min(coord_summ_i$composite_index)) == 1) {

        reasons_better_eval <- compare_ranks(coord_summ_i, 1)

        row_selected <- coord_summ_i[coord_summ_i$rank == 1, ]

        sources_harm_i <- coord_sources_i %>%
          filter(longitude == unique(row_selected$longitude) &
                   latitude == unique(row_selected$latitude)) %>%
          pull(source)

        coord_harmonised <-
          rbind(coord_harmonised,
                data.frame(
                  country = unique(coord_sources_i$country),
                  plot_id = plot_ids[i],
                  longitude = row_selected$longitude,
                  latitude = row_selected$latitude,
                  eval = reasons_better_eval,
                  sources = paste(unique(sort(coord_sources_i$source)),
                                  collapse = "-"),
                  sources_harm = paste(unique(sort(sources_harm_i)),
                                       collapse = "-"),
                  observation_date =
                    as.character(format(Sys.Date(), "%Y-%m-%d")),
                  dist_diagonal_m = round(diagonal_dist_m)))

      } else {

        # 4.4 Else, check visually ----

        # Show dataframe

        print(coord_sources_i %>%
                select(source, survey_year, longitude, latitude, change_date,
                       remark))

        cat(paste0(" \nCountry: ", unique(coord_sources_i$country), "\n"))
        cat(paste0("Plot: ", plot_ids[i], "\n"))
        cat(paste0(" \nDiagonal distance: ", round(diagonal_dist_m),
                   " meter\n \n"))

        print(coord_summ_i %>%
                select(-longitude_dec, -latitude_dec,
                       -label, -colour, -rank) %>%
                select(-rank_change_date_recent,
                       -survey_recent, -rank_survey_recent,
                       -rank_minus,
                       -has_fscdb, -rank_has_fscdb,
                       -has_bd, -rank_has_bd,
                       -has_sx_pls, -has_sx_prf, - has_sx, -rank_has_sx,
                       -has_heavy_metals, -rank_heavy_metals,
                       -rank_error))

        # Show the plots on a map

        map <- leaflet() %>%
          addTiles() %>%  # OSM basemap
          addCircleMarkers(lng = coord_summ_i$longitude_dec,
                           lat = coord_summ_i$latitude_dec,
                           label = coord_summ_i$label,
                           color = coord_summ_i$colour,
                           labelOptions = labelOptions(noHide = TRUE))

        print(map)

        # Pause to observe the changes (optional)
        Sys.sleep(2)

        extra_reason_first <- NULL


        additional_info <-
          readline(prompt = paste0("Enter the row rank corresponding with the ",
                                   "most likely coordinate pair after visual ",
                                   "assessment:  "))



        if (gives_warning(as.numeric(additional_info))) {

          additional_info_num <-
            as.numeric(unlist(strsplit(additional_info, "-"))[1])

          extra_reason_first <- unlist(strsplit(additional_info, "-"))[2]

          if (extra_reason_first %in% c("forest", " forest", "for", " for")) {
            extra_reason_first <-
              "Located in forest whereas alternatives are not (OpenStreetMap)"
          }

        } else {
          # additional_info_num <- as.numeric(additional_info)
          additional_info_num <- ifelse(additional_info == "",
                                        1,
                                        as.numeric(additional_info))
        }




        reasons_better_eval <- compare_ranks(coord_summ_i, additional_info_num)

        if (!is.null(extra_reason_first)) {
          reasons_better_eval <- paste0(extra_reason_first, "; ",
                                        reasons_better_eval)
        }

        row_selected <- coord_summ_i[coord_summ_i$rank == additional_info_num, ]

        sources_harm_i <- coord_sources_i %>%
          filter(longitude == unique(row_selected$longitude) &
                   latitude == unique(row_selected$latitude)) %>%
          pull(source)


        coord_harmonised <-
          rbind(coord_harmonised,
                data.frame(
                  country = unique(coord_sources_i$country),
                  plot_id = plot_ids[i],
                  longitude = row_selected$longitude,
                  latitude = row_selected$latitude,
                  eval = reasons_better_eval,
                  sources = paste(unique(sort(coord_sources_i$source)),
                                  collapse = "-"),
                  sources_harm = paste(unique(sort(sources_harm_i)),
                                       collapse = "-"),
                  observation_date =
                    as.character(format(Sys.Date(), "%Y-%m-%d")),
                  dist_diagonal_m = round(diagonal_dist_m)))




      }


    }

  }
}








# 5. Save the data ----


if (level == "LI") {

write.table(coord_harmonised,
            file = paste0("./data/additional_data/coordinates_plots/",
                          "coord_harmonised_s1.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

}



if (level == "LII") {

  write.table(coord_harmonised,
              file = paste0("./data/additional_data/coordinates_plots/",
                            "coord_harmonised_so.csv"),
              row.names = FALSE,
              na = "",
              sep = ";",
              dec = ".")

}




























