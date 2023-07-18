
#' List inconsistencies in coordinates
#'
#' This function evaluates the format and location of reported coordinates.
#'
#' @param surveys_with_coordinates Character vector - Vector with names of
#' the survey forms which contain coordinates. For example:
#' c("y1_pl1", "s1_pls", "s1_prf", "si_plt", "so_pls", "so_prf")
#' These data forms must contain the columns "latitude_error" and
#' "longitude_error", which means that they need to be previously processed
#' with the function "dec_coordinates()".
#' @param boundary_buffer_meter Numeric - Buffer distance in meters to be
#' added outside the reported partner boundaries as a margin of error for
#' the check if coordinates are within the partner boundaries. Default is
#' 2000 meter.
#' @param save_to_env Logical which indicates whether the output dataframes
#' can be saved to the environment and override any existing objects
#' with the same name. Default is FALSE.
#'
#' @details
#' Which kind of inconsistencies are identified in this function?
#'
#' - FSCC_4: Are the MM and SS of the reported coordinate within their possible
#'   range (0-59)?
#'
#'   This is needs to be checked beforehand and indicated using the columns
#'   "latitude_error" and "longitude_error". The function "dec_coordinate()"
#'   adds these columns when converting the coordinates in +/-DDMMSS to
#'   decimal degrees, which is done in the function "read_icpforests_csv()".
#'
#' - FSCC_13: Are the reported coordinates located within their respective
#'   partner boundaries?
#'
#'   This depends on a custom-made geopackage for the partner boundaries.
#'   Sources:
#'   * Countries via R function: geodata::world(resolution = 1, level = 0)
#'   * German Länder:
#'     <https://maps.princeton.edu/catalog/tufts-germany-states-15>
#'   * Flanders and Wallonia:
#'     <https://hub.arcgis.com/datasets/esribeluxdata::belgium-provinces-1/
#'     explore>
#'   * Canary islands:
#'     <https://data.metabolismofcities.org/dashboards/madrid/maps/35475/view/>
#'   * Azores:
#'     <https://data.metabolismofcities.org/dashboards/lisbon/maps/35879/>
#'
#'   Note: Cross-checking border-region coordinates with Google maps reveals
#'   that the borders in the geopackage are not optimally accurate.
#'   Therefore, it is recommended to specify a buffer zone of 2000 meters
#'   around the partner boundaries as an error margin in the
#'   "boundary_buffer_meter" input argument.
#'
#' Outputs - This function generates and returns:
#' - an inconsistency report ("list_coordinate_inconsistencies")
#' - the original data forms, but without the columns "longitude_error" and
#'   "latitude_error"
#'
#' WARNING - This function may not be optimally efficient and may ideally
#' require refactoring for better performance.
#'
#' @examples
#' surveys_with_coordinates <-
#'   c("y1_pl1", "s1_pls", "s1_prf", "si_plt", "so_pls", "so_prf")
#' get_coordinate_inconsistencies(surveys_with_coordinates, 3000)

get_coordinate_inconsistencies <- function(surveys_with_coordinates = NULL,
                                           boundary_buffer_meter = 2000,
                                           save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  if (isTRUE(getOption("knitr.in.progress"))) {
    envir <- knitr::knit_global()
  } else {
    envir <- globalenv()
  }

if (is.null(surveys_with_coordinates)) {

  surveys <- c("y1_pl1", "y1_st1", "s1_lqa", "s1_pfh", "s1_pls", "s1_prf",
               "s1_som", "si_eve", "si_plt", "si_sta", "si_tco", "so_lqa",
               "so_pfh", "so_pls", "so_prf", "so_som", "sw_swa", "sw_swc")
  
  surveys_with_coordinates <- NULL
  
  for (i in seq_along(surveys)) {
    if (surveys[i] %in% ls(envir = envir)) {
      if (!identical(get_env(surveys[i])$latitude_dec, NULL) &&
          !identical(get_env(surveys[i])$longitude_dec, NULL)) {

        surveys_with_coordinates <- c(surveys_with_coordinates,
                                      surveys[i])
      }
    }
  }
}

  # Import the inconsistency catalogue ----

  assertthat::assert_that(
    file.exists("./data/additional_data/inconsistency_catalogue.csv"),
    msg = paste0("There is no 'inconsistency catalogue' in the",
                 " '.data/additional_data/' folder."))

  inconsistency_catalogue <-
    read.csv("./data/additional_data/inconsistency_catalogue.csv", sep = ";")

  # Assert that all surveys contain columns with coordinates and
  # coordinate errors

  coords <- vector(length = length(surveys_with_coordinates), mode = "logical")

  for (i in seq_along(surveys_with_coordinates)) {

    df_name <- surveys_with_coordinates[i]
    df <- get_env(df_name)

    # Check if both "latitude_dec" and "longitude_dec" are present in the
    # column names

    coords[i] <- "latitude_dec" %in% names(df) &&
      "longitude_dec" %in% names(df)
    }

  assertthat::assert_that(all(coords == TRUE),
                          msg = paste0("One or multiple survey forms given",
                                       " as input do(es) not contain columns",
                                       " 'latitude_dec' and 'longitude_dec'."))

  coord_errors <-
    vector(length = length(surveys_with_coordinates), mode = "logical")

  for (i in seq_along(surveys_with_coordinates)) {

    df_name <- surveys_with_coordinates[i]
    df <- get_env(df_name)
    coord_errors[i] <- "latitude_error" %in% names(df) &&
      "longitude_error" %in% names(df)
    }

  assertthat::assert_that(all(coord_errors == TRUE),
                          msg = paste0("One or multiple survey forms given",
                                       " as input do(es) not contain columns",
                                       " 'latitude_error' and",
                                       " 'longitude_error' with",
                                       " inconsistencies in +/-DDMMSS format."))


  # Set up a progress bar to track processing

  if (!isTRUE(getOption("knitr.in.progress"))) {
    progress_bar <- txtProgressBar(min = 0,
                                   max = length(surveys_with_coordinates),
                                   style = 3)
    }

### Get partner boundaries sf dataframes (partner_boundaries.gpkg) ----

world_spat <- 
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "world_spat")
german_partners_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "german_partners_spat")
eberswalde_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "eberswalde_spat")
flanders_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "flanders_spat")
wallonia_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "wallonia_spat")
canary_islands_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "canary_islands_spat")
azores_spat <-
  read_sf("./data/additional_data/partner_boundaries.gpkg",
          layer = "azores_spat")

### Match ICP Forests' partners with respective spatial polygons ----

d_partner_spat <- get_env("d_partner")
d_partner_spat$spatial_feature_source <- NA
d_partner_spat$spatial_feature_name <- NA

for (j in seq_len(nrow(d_partner_spat))) {

  if (!identical(which(d_partner_spat$desc_short[j] == world_spat$NAME_0),
                integer(0))) {
  ind <- which(d_partner_spat$desc_short[j] == world_spat$NAME_0)
  d_partner_spat$spatial_feature_source[j] <- "world_spat$NAME_0"
  d_partner_spat$spatial_feature_name[j] <- world_spat$NAME_0[ind]
  } else

if (d_partner_spat$code[j] == 1301) {
  d_partner_spat$spatial_feature_source[j] <- "world_spat$NAME_0"
  d_partner_spat$spatial_feature_name[j] <- "Sweden"
  } else

if (d_partner_spat$code[j] == 3404) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Schleswig-Holstein"
  } else

if (d_partner_spat$code[j] == 3604) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Sachsen"
  } else

if (d_partner_spat$code[j] == 3704) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Thüringen"
  } else

if (d_partner_spat$code[j] == 2704) {
  d_partner_spat$spatial_feature_source[j] <- "eberswalde_spat"
  } else

if (d_partner_spat$code[j] == 3004) {
  # There are several states, so we can consider the boundaries of Germany?
  d_partner_spat$spatial_feature_source[j] <- "world_spat$NAME_0"
  d_partner_spat$spatial_feature_name[j] <- "Germany"}
  else

if (d_partner_spat$code[j] == 3104) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Mecklenburg-Vorpommern"
  } else

if (d_partner_spat$code[j] == 3204) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Nordrhein-Westfalen"
  } else

if (d_partner_spat$code[j] == 3304) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Rheinland-Pfalz"
  } else

if (d_partner_spat$code[j] == 3504) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Saarland"
  } else

if (d_partner_spat$code[j] == 9804) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Hamburg"
  } else

if (d_partner_spat$code[j] == 2804) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Baden-Württemberg"
  } else

if (d_partner_spat$code[j] == 2904) {
  d_partner_spat$spatial_feature_source[j] <- "german_partners_spat$StateName1"
  d_partner_spat$spatial_feature_name[j] <- "Bayern"
  } else

if (d_partner_spat$code[j] == 98) {
  d_partner_spat$spatial_feature_source[j] <- "world_spat$NAME_0"
  d_partner_spat$spatial_feature_name[j] <- "Germany"
  } else

if (d_partner_spat$code[j] == 102) {
  d_partner_spat$spatial_feature_source[j] <- "flanders_spat"
  } else

if (d_partner_spat$code[j] == 202) {
  d_partner_spat$spatial_feature_source[j] <- "wallonia_spat"
  } else

if (d_partner_spat$code[j] == 6) {
  d_partner_spat$spatial_feature_source[j] <- "world_spat$NAME_0"
  d_partner_spat$spatial_feature_name[j] <- "United Kingdom"
  } else

if (d_partner_spat$code[j] == 54) {
  d_partner_spat$spatial_feature_source[j] <- "world_spat$NAME_0"
  d_partner_spat$spatial_feature_name[j] <- "Slovakia"
  } else

if (d_partner_spat$code[j] == 58) {
  d_partner_spat$spatial_feature_source[j] <- "world_spat$NAME_0"
  d_partner_spat$spatial_feature_name[j] <- "Czech Republic"
  } else

if (d_partner_spat$code[j] == 95) {
  d_partner_spat$spatial_feature_source[j] <- "canary_islands_spat"
  } else

if (d_partner_spat$code[j] == 96) {
  d_partner_spat$spatial_feature_source[j] <- "azores_spat"
  } else

if (d_partner_spat$code[j] == 72) {
  d_partner_spat$spatial_feature_source[j] <- "world_spat$NAME_0"
  d_partner_spat$spatial_feature_name[j] <- "Turkey"
}
}


# get coordinate inconsistencies for each survey
# The intention is to create a list_layer_inconsistencies of this format:

list_coordinate_inconsistencies <-
  data.frame(survey_form = NULL,
             partner = NULL,
             partner_code = NULL,
             country = NULL,
             code_country = NULL,
             survey_year = NULL,
             code_plot = NULL,
             plot_id = NULL,
             code_layer_horizon_master = NULL,
             repetition_profile_pit_id = NULL,
             code_line = NULL,
             parameter = NULL,
             parameter_unit = NULL,
             parameter_value = NULL,
             inconsistency_reason = NULL,
             inconsistency_type = NULL,
             rule_id = NULL,
             non_duplicated_error_type_per_record = NULL,
             change_date = NULL,
             download_date = NULL)

# Evaluate for each of the survey forms

for (i in seq_along(surveys_with_coordinates)) {

  if (surveys_with_coordinates[i] %in% ls(envir = envir)) {

  df <- get_env(surveys_with_coordinates[i])

# FSCC_4: Are minutes and seconds in 0-59 range? "Error: out of range (0 - 59)"

  # Latitude

  if (!identical(df$latitude_error, NULL)) {

    vec <- which(!is.na(df$latitude_error))

  if (!identical(vec, integer(0))) {

  if (identical(df$survey_year,NULL)) {
      survey_year_vec <- rep(NA, length(vec))
  } else {
      survey_year_vec <- df$survey_year[vec]
  }

  if ("profile_pit_id" %in% names(df)) {
      repetition_profile_pit_id_vec <- df$profile_pit_id[vec]
    } else {
      repetition_profile_pit_id_vec <- rep(NA, length(vec))
    }

  rule_id <- "FSCC_4"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

    list_coordinate_inconsistencies <- rbind(
       list_coordinate_inconsistencies,
       data.frame(survey_form = rep(as.factor(surveys_with_coordinates[i]),
                                    length(vec)),
                  partner = df$partner[vec],
                  partner_code = df$partner_code[vec],
                  country = df$country[vec],
                  code_country = df$code_country[vec],
                  survey_year = survey_year_vec,
                  code_plot = df$code_plot[vec],
                  plot_id = df$plot_id[vec],
                  code_layer_horizon_master = rep(NA, length(vec)),
                  repetition_profile_pit_id = repetition_profile_pit_id_vec,
                  code_line = df$code_line[vec],
                  parameter = as.factor(rep("latitude", length(vec))),
                  parameter_unit = as.factor(rep("+/-DDMMSS", length(vec))),
                  parameter_value = df$latitude[vec],
                  inconsistency_reason = inconsistency_reason,
                  inconsistency_type = inconsistency_type,
                  rule_id = rule_id,
                  non_duplicated_error_type_per_record = rep(TRUE, length(vec)),
                  change_date = df$change_date[vec],
                  download_date = rep(download_date_pir, length(vec))))
  }
  }

  # Longitude

  if (!identical(df$longitude_error, NULL)) {

    vec <- which(!is.na(df$longitude_error))

  if (!identical(vec, integer(0))) {

  if (identical(df$survey_year,NULL)) {
      survey_year_vec <- rep(NA,length(vec))
  } else {
      survey_year_vec <- df$survey_year[vec]
  }

  if ("profile_pit_id" %in% names(df)) {
      repetition_profile_pit_id_vec <- df$profile_pit_id[vec]
    } else {
      repetition_profile_pit_id_vec <- rep(NA, length(vec))
    }

  rule_id <- "FSCC_4"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  list_coordinate_inconsistencies <- rbind(
    list_coordinate_inconsistencies,
    data.frame(survey_form = rep(as.factor(surveys_with_coordinates[i]),
                                 length(vec)),
               partner = df$partner[vec],
               partner_code = df$partner_code[vec],
               country = df$country[vec],
               code_country = df$code_country[vec],
               survey_year = survey_year_vec,
               code_plot = df$code_plot[vec],
               plot_id = df$plot_id[vec],
               code_layer_horizon_master = rep(NA, length(vec)),
               repetition_profile_pit_id = repetition_profile_pit_id_vec,
               code_line = df$code_line[vec],
               parameter = as.factor(rep("longitude", length(vec))),
               parameter_unit = as.factor(rep("+/-DDMMSS", length(vec))),
               parameter_value = df$longitude[vec],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record = rep(TRUE, length(vec)),
               change_date = df$change_date[vec],
               download_date = rep(download_date_pir, length(vec))))
  }
  }


# FSCC_13: Are plot coordinates within the respective partner boundaries?

  # Per partner

  for (j in seq_along(unique(df$partner_code))) {

  # Create a spatial dataframe with plot locations of the given partner

  df_indiv_partner_spat <- df %>%
    filter(partner_code == unique(df$partner_code)[j]) %>%
    filter(!is.na(latitude_dec) & !is.na(longitude_dec))
  
  if (nrow(df_indiv_partner_spat) > 0) {
  
  df_indiv_partner_spat <- df_indiv_partner_spat %>%
    st_as_sf(coords = c("longitude_dec", "latitude_dec"), crs = 4326) %>%
    st_transform(crs = 3035)


  # Create a dataframe with coordinates of the given partner (without geometry)

  df_indiv_partner <- st_drop_geometry(df_indiv_partner_spat)

  # Retrieve the boundaries of the given partner

  spatial_feature_source <- d_partner_spat$spatial_feature_source[
          which(d_partner_spat$code == unique(df$partner_code)[j])]

  spatial_feature_source <- unlist(strsplit(spatial_feature_source, "[$]"))

  spatial_feature_name <- d_partner_spat$spatial_feature_name[
          which(d_partner_spat$code == unique(df$partner_code)[j])]

  indiv_partner_spat <- get(spatial_feature_source[1])

  if (!is.na(spatial_feature_name)) {

    names(indiv_partner_spat)[
      which(names(indiv_partner_spat) == spatial_feature_source[2])] <-
      "partner_name"

  suppressWarnings(indiv_partner_spat <- indiv_partner_spat %>%
                     filter(partner_name == spatial_feature_name) %>%
                     # Exclude some oversea areas (e.g. for France)
                     st_crop(xmin = 1110000, xmax = 7290000,
                             ymin = 980000, ymax = 5360000))
  }

  # Identify records outside each partner polygon (with buffer)

  records_outside_partner_boundaries <-
    unlist(st_disjoint(st_buffer(indiv_partner_spat,
                                 dist = boundary_buffer_meter),
                       df_indiv_partner_spat, sparse = TRUE))

  # Store inconsistencies in case any coordinates outside the partner
  # boundaries were identified

  if (!identical(records_outside_partner_boundaries, integer(0))) {

    vec <- records_outside_partner_boundaries

  for (k in vec) {

  if (identical(df_indiv_partner$survey_year, NULL)) {
      survey_year_k <- rep(NA, length(k))
    } else {
      survey_year_k <- df_indiv_partner$survey_year[k]
    }

  if ("profile_pit_id" %in% names(df)) {
      repetition_profile_pit_id_k <- df$profile_pit_id[k]
    } else {
      repetition_profile_pit_id_k <- rep(NA, length(k))
    }

  rule_id <- "FSCC_13"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  list_coordinate_inconsistencies <- rbind(
    list_coordinate_inconsistencies,
    data.frame(survey_form = rep(as.factor(surveys_with_coordinates[i]),
                                 length(k)),
               partner = df_indiv_partner$partner[k],
               partner_code = df_indiv_partner$partner_code[k],
               country = df_indiv_partner$country[k],
               code_country = df_indiv_partner$code_country[k],
               survey_year = survey_year_k,
               code_plot = df_indiv_partner$code_plot[k],
               plot_id = df_indiv_partner$plot_id[k],
               code_layer_horizon_master = rep(NA, length(k)),
               repetition_profile_pit_id = repetition_profile_pit_id_k,
               code_line = df_indiv_partner$code_line[k],
               parameter = as.factor(rep("latitude", length(k))),
               parameter_unit = as.factor(rep("+/-DDMMSS", length(k))),
               parameter_value = df_indiv_partner$latitude[k],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record = rep(TRUE, length(k)),
               change_date = df_indiv_partner$change_date[k],
               download_date = rep(download_date_pir, length(k))))

  rule_id <- "FSCC_13"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  list_coordinate_inconsistencies <- rbind(
    list_coordinate_inconsistencies,
    data.frame(survey_form = rep(as.factor(surveys_with_coordinates[i]),
                                 length(k)),
               partner = df_indiv_partner$partner[k],
               partner_code = df_indiv_partner$partner_code[k],
               country = df_indiv_partner$country[k],
               code_country = df_indiv_partner$code_country[k],
               survey_year = survey_year_k,
               code_plot = df_indiv_partner$code_plot[k],
               plot_id = df_indiv_partner$plot_id[k],
               code_layer_horizon_master = rep(NA, length(k)),
               repetition_profile_pit_id = repetition_profile_pit_id_k,
               code_line = df_indiv_partner$code_line[k],
               parameter = as.factor(rep("longitude", length(k))),
               parameter_unit = as.factor(rep("+/-DDMMSS", length(k))),
               parameter_value = df_indiv_partner$longitude[k],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record = rep(FALSE, length(k)),
               change_date = df_indiv_partner$change_date[k],
               download_date = rep(download_date_pir, length(k))))
  }
  }
  }
  }

  if (!isTRUE(getOption("knitr.in.progress"))) {
    close(progress_bar)
  }

  # Remove columns "latitude_error" and "longitude_error"

  df <- df[, -which(names(df) %in% c("latitude_error", "longitude_error"))]

  # Save dataframe to the environment

  if (save_to_env == TRUE) {
  assign_env(surveys_with_coordinates[i], df)
  }
  }

  if (!isTRUE(getOption("knitr.in.progress"))) {
  setTxtProgressBar(progress_bar, i)
  }
  }

# Save "list_coordinate_inconsistencies" in environment

if (save_to_env == TRUE) {

  assign_env("list_coordinate_inconsistencies",
             list_coordinate_inconsistencies)

  cat(paste0("'", "list_coordinate_inconsistencies", "' contains ",
             length(which(
       list_coordinate_inconsistencies$non_duplicated_error_type_per_record ==
         TRUE)),
             " unique inconsistencies.\n"))
}

}
