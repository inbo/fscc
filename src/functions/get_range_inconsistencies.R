

#' List inconsistencies in (ranges of) parameter values
#'
#' This function evaluates the presence/plausibility of the data.
#'
#' @param survey_form Character string - Name of the survey form (lower case and
#' separated by '_') to be evaluated
#' @param solve Logical - Indicates whether obvious mistakes can be solved in
#' this function (if TRUE)
#' @details
#' For survey forms: "s1_som", "s1_pfh", "s1_prf", "s1_pls", "y1_st1",
#' "so_som", "so_pfh", "so_prf", "so_pls", "si_sta", "sw_swc"
#'
#' This function uses:
#' - the function "as_character_summary" which creates a character summary of
#' the elements in a vector. For example, it turns a vector c(1, 2, 3, 1000)
#' into "1, 2, 3 and 1000".
#'
#' Outputs - This function generates and returns:
#' - an inconsistency report ("list_range_inconsistencies")
#' - the original data form, but with correct parameter units, provided that
#' the original data were in the wrong parameter units (if solve == TRUE)
#'
#' Which kind of inconsistencies are identified in this function?
#' FSCC_15: Are all mandatory parameters reported?
#' FSCC_42: Are some parameters in "prf", which are highly informative
#'          (even though they have not always been mandatory to report)
#'          reported?
#' FSCC_50: Are soil classifications not reported according to FAO88?
#' FSCC 49: Are the European forest types reported? (system installment forms)
#' FSCC_22: Are the data reported in the correct units?
#' FSCC 38: Are the reported data in a possible range?
#' FSCC 14: Are the reported data in a plausible range?
#' FSCC_12: For the parameter "horizon_ph" in "pfh" survey forms:
#'          Has the pH extraction method been specified under "other_obs"?
#' FSCC_41: Are the reported codes in the range of possible codes
#'          (as defined in manual)?
#' FSCC_46: Is one of the three texture classes (clay/silt/sand) missing
#'          while the others have been reported?
#' FSCC_43: Is the parameter "horizon_coarse_weight" a weight percentage?
#'          (During the BioSoil project, this parameter had to be reported
#'          as a volumetric percentage. However, ICP Forests requires this
#'          to be reported as a weight percentage.)
#'
#'  How are the survey forms exactly updated in this function
#'  (if "solve" == TRUE)?
#'  - Convert data that are reported in the wrong parameter units to the right
#'    parameter units (see PIR for rule_id = "FSCC_22") by multiplying with a
#'    factor 10 (% to g kg-1 for "organic_carbon_total", "n_total",
#'    "horizon_c_organic_total", "horizon_n_total", "horizon_caco3_total",
#'    "horizon_gypsum") or with a factor *1000 (g cm-3 to kg m-3 for
#'    "bulk_density", "horizon_bulk_dens_measure", "horizon_bulk_dens_est")
#'
#'  WARNING - This function may not be optimally efficient and may ideally
#'  require refactoring for better performance.
#'
#' @examples
#' get_range_inconsistencies("so_som", solve = TRUE)

get_range_inconsistencies <- function(survey_form, solve = NULL) {


  # Monitor how long it takes to run this function

  start_time_r <- Sys.time()

  # If no "solve" argument is provided, set it to "FALSE" (by default)

  if (is.null(solve)) {
    solve <- FALSE
    }

  # Retrieve the survey_form data

  df <- get(survey_form, envir = .GlobalEnv)

  # Import the inconsistency catalogue ----

  assertthat::assert_that(
    file.exists("./data/additional_data/inconsistency_catalogue.csv"),
    msg = paste0("There is no 'inconsistency catalogue' in the",
                 " '.data/additional_data/' folder."))

  inconsistency_catalogue <-
    read.csv("./data/additional_data/inconsistency_catalogue.csv", sep = ";")


  # Retrieve a dataframe which contains ranges that are plausible or possible
  # for different analytical parameters.
  # The plausible ranges in this dataframe are derived from:
  # Table 6 of the ICP Forests manual (Part XVI Quality Assurance and Control
  # in Laboratories, Version 2020-1) [based on the BioSoil survey dataset]
  # Note that these ranges are relatively narrow and require an update
  # once the ICP Forests dataset is validated.
  # The possible ranges are based on "common knowledge" (if possible),
  # e.g. many analytical values can't be negative; fractions/percentages can
  # often not be higher than 100 %; bulk densities can't be higher than
  # 2650 kg m-3 (mineral density of quartz)...

  ranges_qaqc <- read.csv("./data/additional_data/ranges_qaqc.csv",
                          sep = ";", dec = ",",
                          colClasses = c(NA, NA, NA, rep("numeric", 6)))

  # 1. Identify the parameters which will be evaluated in this function
  #    (depending on the survey form)

  # 1.1 Mandatory parameters

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "prf", "pfh")) {

    # Retrieve a dataframe with information about which soil parameters have been
    # mandatory in ICP Forests + since when + for which layers

    parameters_mandatory <-
      read.csv("./data/additional_data/parameters_mandatory.csv",
               sep = ";", dec = ",")

    parameters_mandatory_df <- parameters_mandatory[
      which((parameters_mandatory$survey_form ==
               unlist(strsplit(survey_form, "_"))[2]) &
              (rowSums(!is.na(parameters_mandatory[, 3:15])) > 0)), ]

    parameters_mandatory_names <- parameters_mandatory_df$parameter
    length_parameters_mandatory <- length(parameters_mandatory_names)

  } else {
    length_parameters_mandatory <- 0
    }


  # 1.2 Wrong units

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "pfh", "swc")) {

  parameters_wrong_units <-
    names(df)[which(names(df) %in% c("bulk_density",
                                     "organic_carbon_total",
                                     "n_total",
                                     "horizon_bulk_dens_measure",
                                     "horizon_bulk_dens_est",
                                     "horizon_c_organic_total",
                                     "horizon_n_total",
                                     "horizon_gypsum",
                                     "horizon_caco3_total"))]
  length_parameters_wrong_units <- length(parameters_wrong_units)
  } else {
    length_parameters_wrong_units <- 0
  }


  # 1.3 Plausible or possible ranges

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "pfh", "swc")) {

      if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {
        names(ranges_qaqc)[
          which(names(ranges_qaqc) == "parameter_pfh")] <- "parameter"
        } else

      if (unlist(strsplit(survey_form, "_"))[2] == "som") {
        names(ranges_qaqc)[
          which(names(ranges_qaqc) == "parameter_som")] <- "parameter"
      } else

      if (survey_form == "sw_swc") {
        names(ranges_qaqc)[
          which(names(ranges_qaqc) == "parameter_som")] <- "parameter"
        }

  parameters <- names(df)[names(df) %in% ranges_qaqc$parameter]
  length_parameters_range <- length(parameters)
  } else {
    length_parameters_range <- 0
  }


  # 1.4 Possible codes

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "pfh",
                                                   "prf", "pls")) {

    # Create a dataframe "list_codes" which lists the possible codes per
    # coded parameter

  dir <- paste0("./data/Download_", download_date)

  subdir <- list.dirs(path = dir,
                      full.names = TRUE, recursive = TRUE) %>%
    str_subset(., pattern = unlist(str_split(survey_form, "_"))[1]) %>%
    # selects the string that ends in a number (i.e. not the subfolders)
    str_subset(., pattern = "\\d$")

  d_texture_class <-
    read.csv(paste0(subdir, "/adds/dictionaries/d_texture_class.csv"),
             sep = ";")
  d_soil_group <-
    read.csv(paste0(subdir, "/adds/dictionaries/d_soil_group.csv"),
             sep = ";")
  d_soil_adjective <-
    read.csv(paste0(subdir,"/adds/dictionaries/d_soil_adjective.csv"), sep=";")
  d_soil_specifier <-
    read.csv(paste0(subdir, "/adds/dictionaries/d_soil_specifier.csv"),
             sep = ";")
  d_wrb_pub <-
    read.csv(paste0(subdir, "/adds/dictionaries/d_wrb_pub.csv"),
             sep = ";")
  d_parent_material <-
    read.csv(paste0(subdir, "/adds/dictionaries/d_parent_material.csv"),
             sep = ";")
  d_hori_disc <-
    read.csv(paste0(subdir, "/adds/dictionaries/d_hori_disc.csv"),
             sep = ";")

  list_codes <- list("code_horizon_discont" = d_hori_disc$code,
                     "code_horizon_destinct" = (1:6),
                     "code_horizon_topo" = (1:5),
                     "code_soil_structure" = (1:10),
                     "code_horizon_texture_class" = d_texture_class$code,
                     "code_horizon_coarse_vol" = c(1, 2, 3, 4, 5, 9),
                     "code_horizon_porosity" = (1:5),
                     "code_roots_very_fine" = c(1, 2, 3, 4, 9),
                     "code_roots_fine" = c(1, 2, 3, 4, 9),
                     "code_roots_medium" = c(1, 2, 3, 4, 9),
                     "code_roots_coarse" = c(1, 2, 3, 4, 9),
                     "code_altitude" = (1:51),
                     "code_wrb_soil_group" = d_soil_group$code,
                     "code_wrb_qualifier_1" = d_soil_adjective$code,
                     "code_wrb_spezifier_1" = d_soil_specifier$code,
                     "code_wrb_qualifier_2" = d_soil_adjective$code,
                     "code_wrb_spezifier_2" = d_soil_specifier$code,
                     "code_wrb_qualifier_3" = d_soil_adjective$code,
                     "code_wrb_spezifier_3" = d_soil_specifier$code,
                     "code_wrb_qualifier_4" = d_soil_adjective$code,
                     "code_wrb_spezifier_4" = d_soil_specifier$code,
                     "code_wrb_qualifier_5" = d_soil_adjective$code,
                     "code_wrb_spezifier_5" = d_soil_specifier$code,
                     "code_wrb_qualifier_6" = d_soil_adjective$code,
                     "code_wrb_spezifier_6" = d_soil_specifier$code,
                     "code_wrb_publication" = d_wrb_pub$code,
                     "code_parent_material_1" = d_parent_material$code,
                     "code_parent_material_2" = d_parent_material$code,
                     "code_water_level_high" = c(1, 2, 3, 4, 5, 9),
                     "code_water_level_low" = c(1, 2, 3, 4, 5, 9),
                     "code_water_type" = c(1, 2, 9),
                     "code_water" = c(1, 2, 3),
                     "code_humus" = (1:9),
                     "code_texture_class" = d_texture_class$code)

  parameters_code <- names(df)[which(names(df) %in% names(list_codes))]
  length_parameters_code <- length(parameters_code)
  } else {
    length_parameters_code <- 0
  }


  # Set up a progress bar to track processing

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "pfh", "prf",
                                                   "pls", "swc")) {
  progress_bar <- txtProgressBar(min = 0,
                  max = (length_parameters_mandatory +
                           length_parameters_wrong_units +
                           length_parameters_range + length_parameters_code),
                                 style = 3)
  }


  # The intention is to create a list_range_inconsistencies of the following format
  # to store inconsistencies found in the survey_form data:

  list_range_inconsistencies <-
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





  # 2. FSCC_15: Check whether mandatory data are reported

  # Check if this has to be tested for the given survey_form

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "prf", "pfh")) {

  # Prepare the dataframe for the test for "som" survey forms

  if (unlist(strsplit(survey_form, "_"))[2] == "som") {

    # Create a dataframe "parameter_layers" which shows for which layers and depths
    # the indicated layers in "parameter_mandatory_df" are exactly valid

    parameters_layers <- rbind(data.frame(layer_name_table = "LI_OF_OH_H",
                                          code_layer = "OF",
                                          layer_limit_superior = NA,
                                          layer_limit_inferior = NA),
                               c("LI_OF_OH_H", "OH", NA, NA),
                               c("LI_OF_OH_H", "H", NA, NA),
                               c("LI_0_10", "M01", 0, 10),
                               c("LI_10_20", "M12", 10, 20),
                               c("LI_20_40", "M24", 20, 40),
                               c("LI_40_80", "M48", 40, 80),

                               c("LII_OF_OH_H", "OF", NA, NA),
                               c("LII_OF_OH_H", "OH", NA, NA),
                               c("LII_OF_OH_H", "H", NA, NA),
                               c("LII_0_10", "M01", 0, 10),
                               c("LII_10_20", "M12", 10, 20),
                               c("LII_20_40", "M24", 20, 40),
                               c("LII_40_80", "M48", 40, 80))

    # Add columns to "df" ("som" survey forms) which indicate several properties
    # on which the fact whether a parameter is mandatory or not, depends

    df$coarse_fragment_vol_any_survey <- NA
    df$bulk_density_any_survey <- NA
    df$part_size_clay_any_survey <- NA
    df$part_size_silt_any_survey <- NA
    df$part_size_sand_any_survey <- NA
    df$exch_ca_any_survey <- NA
    df$rea_al_any_survey <- NA
    df$rea_fe_any_survey <- NA
    df$non_stony <- NA
    df$calcareous_pH5 <- NA
    df$calcareous_pH6 <- NA

    for (i in seq_len(nrow(df))) {

      # Create a vector with row indices of all the survey profiles of the given
      # layer, in order to find out whether a parameter has ever been reported
      # (since some parameters have to be reported just once)

      vec <- which(df$plot_id == df$plot_id[i] &
                   df$code_layer == df$code_layer[i])

      # Has "coarse_fragment_vol" ever been reported?

      if (any(!is.na(df$coarse_fragment_vol[vec]) &
              (df$coarse_fragment_vol[vec] != ""))) {
        df$coarse_fragment_vol_any_survey[i] <- TRUE
        }

      # Has bulk density ever been reported?

      if (any(!is.na(df$bulk_density[vec]) &
              (df$bulk_density[vec] != ""))) {
        df$bulk_density_any_survey[i] <- TRUE
        }

      # Have the clay, silt and sand contents ever been reported?

      if (any(!is.na(df$part_size_clay[vec]) &
              (df$part_size_clay[vec] != ""))) {
        df$part_size_clay_any_survey[i] <- TRUE
      }

      if (any(!is.na(df$part_size_silt[vec]) &
              (df$part_size_silt[vec] != ""))) {
        df$part_size_silt_any_survey[i] <- TRUE
      }

      if (any(!is.na(df$part_size_sand[vec]) &
              (df$part_size_sand[vec] != ""))) {
        df$part_size_sand_any_survey[i] <- TRUE
      }

      # Has "exch_ca" ever been reported?

      if (any(!is.na(df$exch_ca[vec]) &
              (df$exch_ca[vec] != ""))) {
        df$exch_ca_any_survey[i] <- TRUE
        }

      # Have "rea_al" and "rea_fe" ever been reported?

      if (any(!is.na(df$rea_al[vec]) &
              (df$rea_al[vec] != ""))) {
        df$rea_al_any_survey[i] <- TRUE
      }

      if (any(!is.na(df$rea_fe[vec]) &
              (df$rea_fe[vec] != ""))) {
        df$rea_fe_any_survey[i] <- TRUE
      }

      # Are stones absent? (<= 5 vol%)

      if ((!is.na(df$coarse_fragment_vol[i]) &&
           (df$coarse_fragment_vol[i] != "")) &&
          (df$coarse_fragment_vol[i] <= 5)) {
        df$non_stony[i] <- TRUE
        }

      # Is the pH_CaCl2 higher than 5.5?

      if ((!is.na(df$ph_cacl2[i]) &&
           (df$ph_cacl2[i] != "")) &&
          (df$ph_cacl2[i] > 5.5)) {
        df$calcareous_pH5[i] <- TRUE
        }

      # Is the pH_CaCl2 higher than 6?

      if ((!is.na(df$ph_cacl2[i]) &&
         (df$ph_cacl2[i] != "")) &&
        (df$ph_cacl2[i] > 6)) {
        df$calcareous_pH6[i] <- TRUE
      }
      }

  }

  # For each of the parameters for which this has to be tested

  for (i in seq_along(parameters_mandatory_names)) {

    # Check since when the parameter is mandatory
    # Not necessary to require mandatory parameters before 2004

    start_year <- parameters_mandatory_df$start_year_mandatory[i]
    if (start_year < 2004) {
      start_year <- 2004
      }

    # Check for which layers this parameter is mandatory

    layers_mandatory <- names(parameters_mandatory_df)[
        which(parameters_mandatory_df[i,] == TRUE)]

    # Retrieve the parameter unit

    parameter_unit <- parameters_mandatory_df$parameter_unit[i]


 # For the survey forms:
 # "som"

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {

      # Finetune the layers for which the parameter is mandatory
      # (depending on whether it concerns Level I or Level II)

      if (unlist(strsplit(survey_form, "_"))[1] == "s1") {
        layers_mandatory <-
          layers_mandatory[which(str_detect(layers_mandatory, "^LI_"))]
        } else

      if (unlist(strsplit(survey_form, "_"))[1] == "so") {
        layers_mandatory <-
          layers_mandatory[which(str_detect(layers_mandatory, "^LII_"))]
        }

      # Check if the parameter is still mandatory for any layers

      if (!identical(layers_mandatory, character(0))) {

      # Use the "parameters_layers" table to check the exact code_layers
      # and depths of the mandatory layers

      parameters_layers_mandatory <- parameters_layers[
        which(parameters_layers$layer_name_table %in% layers_mandatory), ]

      # Determine the depth range for which the parameter is mandatory

      layer_limit_upper <-
        min(parameters_layers_mandatory$layer_limit_superior)
      layer_limit_lower <-
        max(parameters_layers_mandatory$layer_limit_inferior)

      # "vec_inconsistency" lists the row indices of the records for which
      # the given parameter is mandatory but absent.
      # To start with, only select the years after the "start_year"

      vec_inconsistency <- which(df$survey_year >= start_year)

      # Update "vec_inconsistency" by selecting the layers for which
      # the parameter is mandatory

      # Select based on code_layer (starting with "OF" or "OH", or with "H"
      # if these layers are mandatory)
      # and/or depth ranges for which it is mandatory

      if ("OF" %in% parameters_layers_mandatory$code_layer ||
          "OH" %in% parameters_layers_mandatory$code_layer ||
          "H" %in% parameters_layers_mandatory$code_layer) {

        if (!is.na(layer_limit_upper) && !is.na(layer_limit_lower)) {
        vec_inconsistency <-
          vec_inconsistency[
            which((substr(df$code_layer[vec_inconsistency], 1, 2) %in%
                     c("OF", "OH")) |
                    (substr(df$code_layer[vec_inconsistency], 1, 1) == "H") |
                          (df$code_layer[vec_inconsistency] %in%
                             parameters_layers_mandatory$code_layer) |
                          ((df$layer_limit_superior[
                            vec_inconsistency] > layer_limit_upper) &
                             (df$layer_limit_inferior[vec_inconsistency] <
                                layer_limit_lower)))]
        } else

        {vec_inconsistency <-
          vec_inconsistency[
            which((substr(df$code_layer[vec_inconsistency], 1, 2) %in% c("OF",
                                                                       "OH")) |
                                    (substr(df$code_layer[
                                      vec_inconsistency], 1, 1) == "H") |
                                    (df$code_layer[vec_inconsistency] %in%
                                       parameters_layers_mandatory$code_layer))]
        }
        } else {


        if (!is.na(layer_limit_upper) && !is.na(layer_limit_lower)) {
          vec_inconsistency <-
            vec_inconsistency[which((df$code_layer[vec_inconsistency] %in%
                                       parameters_layers_mandatory$code_layer) |
                                    ((df$layer_limit_superior[
                                      vec_inconsistency] > layer_limit_upper) &
                                         (df$layer_limit_inferior[
                                           vec_inconsistency] <
                                            layer_limit_lower)))]
          } else {

         vec_inconsistency <-
           vec_inconsistency[which((df$code_layer[vec_inconsistency] %in%
                                      parameters_layers_mandatory$code_layer))]
         }
        }


      # Per parameter, finetune "vec_inconsistencies" based on the special
      # conditions using the extra columns that were added to "df"
      # (e.g. some parameters only have to be reported once, are only
      # mandatory in non-stony layers, etc), to select the exact row indices
      # for which the mandatory parameter is not reported

      if (parameters_mandatory_names[i] == "coarse_fragment_vol") {
        vec_inconsistency <- vec_inconsistency[
          which(is.na(df$coarse_fragment_vol_any_survey[vec_inconsistency]) |
                 (df$coarse_fragment_vol_any_survey[vec_inconsistency] == ""))]
        } else

      if (parameters_mandatory_names[i] == "bulk_density") {
        vec_inconsistency <- vec_inconsistency[
          which((is.na(df$bulk_density_any_survey[vec_inconsistency]) |
                   (df$bulk_density_any_survey[vec_inconsistency] == "")) &
                !is.na(df$non_stony[vec_inconsistency]))]
        } else

      if (parameters_mandatory_names[i] == "part_size_clay") {
        vec_inconsistency <- vec_inconsistency[
          which(is.na(df$part_size_clay_any_survey[vec_inconsistency]) |
                  (df$part_size_clay_any_survey[vec_inconsistency] == ""))]
        } else

      if (parameters_mandatory_names[i] == "part_size_silt") {
        vec_inconsistency <- vec_inconsistency[
          which(is.na(df$part_size_silt_any_survey[vec_inconsistency]) |
                  (df$part_size_silt_any_survey[vec_inconsistency] == ""))]
        } else

      if (parameters_mandatory_names[i] == "part_size_sand") {
        vec_inconsistency <- vec_inconsistency[
          which(is.na(df$part_size_sand_any_survey[vec_inconsistency]) |
                  (df$part_size_sand_any_survey[vec_inconsistency] == ""))]
        } else

      if (parameters_mandatory_names[i] == "carbonates") {
        vec_inconsistency <- vec_inconsistency[
          which(((is.na(df$carbonates[vec_inconsistency]) |
                    (df$carbonates[vec_inconsistency] == "")) &
                   !is.na(df$calcareous_pH5[vec_inconsistency]) &
                   (df$code_layer[vec_inconsistency] != "mineral")) |
                  ((is.na(df$carbonates[vec_inconsistency]) |
                      (df$carbonates[vec_inconsistency] == "")) &
                     !is.na(df$calcareous_pH6[vec_inconsistency]) &
                     (df$code_layer[vec_inconsistency] == "mineral")))]
        } else

      if (parameters_mandatory_names[i] == "exch_acidiy") {
        vec_inconsistency <- vec_inconsistency[
          which((is.na(df$exch_acidiy[vec_inconsistency]) |
                   (df$exch_acidiy[vec_inconsistency] == "")) &
                  is.na(df$calcareous_pH5[vec_inconsistency]))]
        } else

      if (parameters_mandatory_names[i] == "free_h") {
        vec_inconsistency <- vec_inconsistency[
          which((is.na(df$free_h[vec_inconsistency]) |
                   (df$free_h[vec_inconsistency] == "")) &
                  is.na(df$calcareous_pH5[vec_inconsistency]))]
        } else

      if (parameters_mandatory_names[i] == "exch_al") {
        vec_inconsistency <- vec_inconsistency[
          which((is.na(df$exch_al[vec_inconsistency]) |
                   (df$exch_al[vec_inconsistency] == "")) &
                  is.na(df$calcareous_pH5[vec_inconsistency]))]
        } else

      if (parameters_mandatory_names[i] == "exch_fe") {
        vec_inconsistency <- vec_inconsistency[
          which((is.na(df$exch_fe[vec_inconsistency]) |
                   (df$exch_fe[vec_inconsistency] == "")) &
                  is.na(df$calcareous_pH5[vec_inconsistency]))]
        } else

      if (parameters_mandatory_names[i] == "exch_mn") {
        vec_inconsistency <- vec_inconsistency[
          which((is.na(df$exch_mn[vec_inconsistency]) |
                   (df$exch_mn[vec_inconsistency] == "")) &
                  is.na(df$calcareous_pH5[vec_inconsistency]))]
        } else

      if (parameters_mandatory_names[i] == "exch_ca") {
        vec_inconsistency <- vec_inconsistency[
          which(((is.na(df$exch_ca[vec_inconsistency]) |
                    (df$exch_ca[vec_inconsistency] == "")) &
               (!df$code_layer[vec_inconsistency] %in% c("M24", "M48")) &
               (!df$layer_type[vec_inconsistency] %in% c("peat",
                                                         "forest_floor"))) |
            (is.na(df$exch_ca_any_survey[vec_inconsistency]) &
               (df$code_layer[vec_inconsistency] %in% c("M24", "M48"))) |
            ((is.na(df$exch_ca[vec_inconsistency]) |
                (df$exch_ca[vec_inconsistency] == "")) &
               (is.na(df$calcareous_pH5[vec_inconsistency])) &
               (df$layer_type[vec_inconsistency] == "peat" |
                  df$code_layer[vec_inconsistency] %in% c("OL", "OF"))))]
        } else

      if (parameters_mandatory_names[i] == "rea_al") {
        vec_inconsistency <- vec_inconsistency[
          which(((is.na(df$rea_al[vec_inconsistency]) |
                    (df$rea_al[vec_inconsistency] == "")) &
               (!df$code_layer[vec_inconsistency] %in% c("M24", "M48"))) |
            (is.na(df$rea_al_any_survey[vec_inconsistency]) &
               (df$code_layer[vec_inconsistency] %in% c("M24", "M48"))))]
        } else

     if (parameters_mandatory_names[i] == "rea_fe") {
       vec_inconsistency <- vec_inconsistency[
         which(((is.na(df$rea_fe[vec_inconsistency]) |
                   (df$rea_fe[vec_inconsistency] == "")) &
                  (!df$code_layer[vec_inconsistency] %in% c("M24", "M48"))) |
                 (is.na(df$rea_fe_any_survey[vec_inconsistency]) &
                    (df$code_layer[vec_inconsistency] %in% c("M24", "M48"))))]
       } else {

         vec_inconsistency <- vec_inconsistency[
            which(is.na(df[vec_inconsistency,
                           which(names(df) == parameters_mandatory_names[i])]) |
               (df[vec_inconsistency,
                   which(names(df) == parameters_mandatory_names[i])] == ""))]
         }


      # Store information about the layers and repetitions to be shown in the
      # inconsistency list for "som" survey forms

      if ("code_layer_original" %in% names(df)) {
          ind_layer_horizon <-
            as.character(df$code_layer_original[vec_inconsistency])
        } else {
          ind_layer_horizon <- as.character(df$code_layer[vec_inconsistency])
        }

      ind_repetition_profile_pit_id <- df$repetition[vec_inconsistency]

    }
    } else




 # For the survey forms:
 # "prf"

    if (unlist(strsplit(survey_form, "_"))[2] == "prf") {

      # Select the row indices for which the given parameter is mandatory
      # but absent

      vec_inconsistency <- which(df$survey_year >= start_year)
      vec_inconsistency <- vec_inconsistency[
          which(is.na(df[vec_inconsistency,
                         which(names(df) == parameters_mandatory_names[i])]) |
                  (df[vec_inconsistency,
                      which(names(df) ==
                              parameters_mandatory_names[i])] == ""))]

      # Store information about the layers and repetitions to be shown in the
      # inconsistency list for "prf" survey forms

      ind_layer_horizon <- rep(NA, length(vec_inconsistency))
      ind_repetition_profile_pit_id <- df$profile_pit_id[vec_inconsistency]

    } else





 # For the survey forms:
 # "pfh"

    if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

      # Select the row indices for which the given parameter is mandatory
      # but absent

      vec_inconsistency <- which(df$survey_year >= start_year &
                                   df$layer_type == "mineral")
      vec_inconsistency <- vec_inconsistency[
          which(is.na(df[vec_inconsistency,
                         which(names(df) == parameters_mandatory_names[i])]) |
                  (df[vec_inconsistency,
                      which(names(df) ==
                              parameters_mandatory_names[i])] == ""))]

      # Store information about the layers and repetitions to be shown in the
      # inconsistency list for "pfh" survey forms

      ind_layer_horizon <- df$horizon_master[vec_inconsistency]
      ind_repetition_profile_pit_id <- df$profile_pit_id[vec_inconsistency]
      }



    # If there are any records for which the mandatory parameter is absent

    if (!identical(vec_inconsistency, integer(0))) {

    # Store information about the inconsistency in "list_range_inconsistencies"

    vec_data <-
      df[vec_inconsistency,which(names(df) == parameters_mandatory_names[i])]
    if (is.factor(vec_data)) {vec_data <- as.character(vec_data)
    }

    rule_id <- "FSCC_15"
    inconsistency_reason <-
      inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]

    # If it concerns soil classification-related parameters:
    # mention that the parameters should ideally be reported according
    # to WRB 2006 or 2007

    if ((unlist(strsplit(survey_form, "_"))[2] == "prf") &&
        (parameters_mandatory_names[i] %in% c("code_wrb_soil_group",
                                              "code_wrb_qualifier_1",
                                              "code_wrb_publication"))) {

      inconsistency_reason <- paste0(inconsistency_reason,
                                     " ",
                                     start_year,
                       ". Preferably, the soil classification should",
                       " be reported according to the World Reference Base",
                       " (WRB) version of either 2006 or 2007.")

    } else {
      inconsistency_reason <- paste0(inconsistency_reason, " ",
                                     start_year, ".")
      }


    inconsistency_type <-
      inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

    list_range_inconsistencies <- rbind(
      list_range_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form,
                                             length(vec_inconsistency))),
                 partner = df$partner[vec_inconsistency],
                 partner_code = df$partner_code[vec_inconsistency],
                 country = df$country[vec_inconsistency],
                 code_country = df$code_country[vec_inconsistency],
                 survey_year = df$survey_year[vec_inconsistency],
                 code_plot = df$code_plot[vec_inconsistency],
                 plot_id = df$plot_id[vec_inconsistency],
                 code_layer_horizon_master = ind_layer_horizon,
                 repetition_profile_pit_id = ind_repetition_profile_pit_id,
                 code_line = df$code_line[vec_inconsistency],
                 parameter = as.factor(rep(parameters_mandatory_names[i],
                                           length(vec_inconsistency))),
                 parameter_unit = rep(parameter_unit,
                                      length(vec_inconsistency)),
                 parameter_value = vec_data,
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record =
                   rep(TRUE, length(vec_inconsistency)),
                 change_date = df$change_date[vec_inconsistency],
                 download_date =
                   rep(download_date_pir, length(vec_inconsistency))))

    }

    setTxtProgressBar(progress_bar, i)

  }
  }






  # FSCC_42: some parameters in "prf" are highly informative, even though they have
  # not always been mandatory to report

  # Check if this has to be tested for the given survey_form

  if (unlist(strsplit(survey_form, "_"))[2] == "prf") {

    # Identify these "informative" parameters based on the
    # "parameters_mandatory" table

    parameters_mandatory_df <- parameters_mandatory[
          which(parameters_mandatory$request_before_mandatory == TRUE), ]
    parameters_informative <- parameters_mandatory_df$parameter

    # For each of the informative parameters:

    for (i in seq_along(parameters_informative)) {

      # Only ask this for survey years before the year starting from which the
      # given parameter is mandatory

      start_year <- parameters_mandatory_df$start_year_mandatory[i]

      if (start_year < 2004) {
        start_year <- 2004
        }

      # Retrieve the parameter unit

      parameter_unit <- parameters_mandatory_df$parameter_unit[i]

      # Retrieve the column index of the given parameter

      col_ind <- which(names(df) == parameters_informative[i])

      # Evaluate for which row indices the given parameter is not reported

      vec_inconsistency <- which(df$survey_year < start_year &
                                 (is.na(df[, col_ind]) |
                                    df[, col_ind] == ""))

      # Check if there are any records for which this informative parameter can
      # be asked

      if (!identical(vec_inconsistency, integer(0))) {

      # Store information about the inconsistency in
      # "list_range_inconsistencies"

      rule_id <- "FSCC_42"
      inconsistency_reason <-
        inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_reason <- unlist(str_split(inconsistency_reason,"_"))
      inconsistency_reason <- paste0(inconsistency_reason[1],
                                     start_year, inconsistency_reason[2])

      # If it concerns soil classification-related parameters:
      # mention that the parameters should ideally be reported according
      # to WRB 2006 or 2007

      if (parameters_informative[i] %in% c("code_wrb_soil_group",
                                           "code_wrb_qualifier_1",
                                           "code_wrb_publication")) {

      inconsistency_reason <- paste0(inconsistency_reason,
        " Preferably, the soil classification should be reported according",
        " to the World Reference Base (WRB) version of either 2006 or 2007.")
      }

      inconsistency_type <-
        inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

      list_range_inconsistencies <- rbind(
        list_range_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form,
                                               length(vec_inconsistency))),
                   partner = df$partner[vec_inconsistency],
                   partner_code = df$partner_code[vec_inconsistency],
                   country = df$country[vec_inconsistency],
                   code_country = df$code_country[vec_inconsistency],
                   survey_year = df$survey_year[vec_inconsistency],
                   code_plot = df$code_plot[vec_inconsistency],
                   plot_id = df$plot_id[vec_inconsistency],
                   code_layer_horizon_master =
                     rep(NA,length(vec_inconsistency)),
                   repetition_profile_pit_id =
                     df$profile_pit_id[vec_inconsistency],
                   code_line = df$code_line[vec_inconsistency],
                   parameter = as.factor(rep(parameters_informative[i],
                                             length(vec_inconsistency))),
                   parameter_unit = rep(parameter_unit,
                                        length(vec_inconsistency)),
                   parameter_value = df[vec_inconsistency,col_ind],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record =
                     rep(TRUE, length(vec_inconsistency)),
                   change_date = df$change_date[vec_inconsistency],
                   download_date = rep(download_date_pir,
                                       length(vec_inconsistency))))

      }
    }
    }




  # FSCC_50: ask NFCs who reported soil classification according to FAO88
  # to resubmit this information according to WRB 2006 or 2007

  # Check if this has to be tested for the given survey_form

  if (unlist(strsplit(survey_form, "_"))[2] == "prf") {

    # Identify the row indices for which the soil classification was performed
    # according to FAO88

    vec_inconsistency <- which(df$code_wrb_publication %in% c("FAO88", "89en"))

    # Check if there are any row indices for which the soil classification was
    # reported according to FAO88

    if (!identical(vec_inconsistency, integer(0))) {

    # Store information about the inconsistency in "list_range_inconsistencies"

    rule_id <- "FSCC_50"
    inconsistency_reason <-
      inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
    inconsistency_type <-
      inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]


    for (j in vec_inconsistency) {

    # code_wrb_publication

    list_range_inconsistencies <- rbind(
      list_range_inconsistencies,
      data.frame(survey_form = (rep(survey_form, length(j))),
                 partner = df$partner[j],
                 partner_code = df$partner_code[j],
                 country = df$country[j],
                 code_country = df$code_country[j],
                 survey_year = df$survey_year[j],
                 code_plot = df$code_plot[j],
                 plot_id = df$plot_id[j],
                 code_layer_horizon_master = rep(NA, length(j)),
                 repetition_profile_pit_id = as.character(df$profile_pit_id[j]),
                 code_line = df$code_line[j],
                 parameter = (rep("code_wrb_publication", length(j))),
                 parameter_unit = rep("-", length(j)),
                 parameter_value =
                   as.character(df[j,
                                   which(names(df) == "code_wrb_publication")]),
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(TRUE, length(j)),
                 change_date = df$change_date[j],
                 download_date = rep(download_date_pir, length(j))))


    # code_wrb_soil_group

    list_range_inconsistencies <- rbind(
      list_range_inconsistencies,
      data.frame(survey_form = (rep(survey_form, length(j))),
                 partner = df$partner[j],
                 partner_code = df$partner_code[j],
                 country = df$country[j],
                 code_country = df$code_country[j],
                 survey_year = df$survey_year[j],
                 code_plot = df$code_plot[j],
                 plot_id = df$plot_id[j],
                 code_layer_horizon_master = rep(NA, length(j)),
                 repetition_profile_pit_id = as.character(df$profile_pit_id[j]),
                 code_line = df$code_line[j],
                 parameter = (rep("code_wrb_soil_group", length(j))),
                 parameter_unit = rep("-", length(j)),
                 parameter_value =
                   as.character(df[j,
                                   which(names(df) == "code_wrb_soil_group")]),
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(FALSE, length(j)),
                 change_date = df$change_date[j],
                 download_date = rep(download_date_pir, length(j))))


    # code_wrb_qualifier_1

    list_range_inconsistencies <- rbind(
      list_range_inconsistencies,
      data.frame(survey_form = (rep(survey_form, length(j))),
                 partner = df$partner[j],
                 partner_code = df$partner_code[j],
                 country = df$country[j],
                 code_country = df$code_country[j],
                 survey_year = df$survey_year[j],
                 code_plot = df$code_plot[j],
                 plot_id = df$plot_id[j],
                 code_layer_horizon_master = rep(NA,length(j)),
                 repetition_profile_pit_id = as.character(df$profile_pit_id[j]),
                 code_line = df$code_line[j],
                 parameter = (rep("code_wrb_qualifier_1", length(j))),
                 parameter_unit = rep("-", length(j)),
                 parameter_value =
                   as.character(df[j,
                                   which(names(df) == "code_wrb_qualifier_1")]),
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(FALSE, length(j)),
                 change_date = df$change_date[j],
                 download_date = rep(download_date_pir, length(j))))
    }
    }
    }





  # FSCC 49: Ask to report the European forest type if missing

  # Check if this has to be tested for the given survey_form

  if (survey_form %in% c("si_sta", "y1_st1")) {

    # Identify the plots which are reported in the soil survey of the given
    # level (Level I, i.e. "s1", versus Level II, i.e. "so")

    if (survey_form == "y1_st1") {
      data_availability_level <- get("data_availability_s1", envir = .GlobalEnv)
      } else
    if (survey_form == "si_sta") {
      data_availability_level <- get("data_availability_so", envir = .GlobalEnv)
      }

    data_availability_plots <- data_availability_level[which(
                  rowSums((data_availability_level[, 8:11]) == 1) > 0) ]

    # Create a table "data_availability_plots"
    # to store information about the (presence of) forest type information

    data_availability_plots <-
      data_availability_plots[!duplicated(data_availability_plots$plot_id),
                               which(names(data_availability_plots) %in%
                                       c("code_country", "partner_code",
                                         "code_plot", "plot_id",
                                         "country", "partner_short",
                                         "partner"))]
    data_availability_plots$code_forest_type_count <- NA
    data_availability_plots$code_forest_type <- NA

    # This function creates a character summary of the elements in a vector
    # For example, it turns a vector c(1, 2, 3, 1000) into "1, 2, 3 and 1000".

    source("./src/functions/as_character_summary.R")

    # For each of the rows (i.e. plots) in "data_availability_plots":

    for (j in seq_along(data_availability_plots$plot_id)) {

      # Retrieve the row indices of the records of the given plot j in the
      # given system installment survey form

      vec_forest_type <-
        which(df$plot_id == data_availability_plots$plot_id[j])

      # Determine which forest types have been reported for the given plot

      forest_type_j <- df$code_forest_type[vec_forest_type]

      # Count how many records in the given system installment survey form
      # contain forest type information (i.e. not an NA) and store it in
      # "data_availability_plots"

      data_availability_plots$code_forest_type_count[j] <-
        length(which(!is.na(forest_type_j)))

      # If any forest type has been reported: store the unique
      # "code_forest_type" code(s) which have been reported for the given plot

      if (any(!is.na(forest_type_j))) {
        forest_type_j <- forest_type_j[which(!is.na(forest_type_j))]
        data_availability_plots$code_forest_type[j] <-
          as_character_summary(unique(forest_type_j))
      }
      }

    # Identify the row indices in "data_availability_plots" of plots for which
    # no forest type has ever been reported

    vec_inconsistency <-
      which(data_availability_plots$code_forest_type_count == 0)

    # Store information about the inconsistency in "list_range_inconsistencies"

    # This is actually a general list with plots for which code_forest_type
    # is lacking
    # Hence I am not specifying "code_line" and "survey_year"
    # (since it can be filled in in either of these plot records)

      rule_id <- "FSCC_49"
      inconsistency_reason <-
        inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <-
        inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

      list_range_inconsistencies <- rbind(
        list_range_inconsistencies,
        data.frame(survey_form = (rep(survey_form, length(vec_inconsistency))),
                   partner = data_availability_plots$partner[vec_inconsistency],
                   partner_code =
                     data_availability_plots$partner_code[vec_inconsistency],
                   country = data_availability_plots$country[vec_inconsistency],
                   code_country =
                     data_availability_plots$code_country[vec_inconsistency],
                   survey_year = rep(NA, length(vec_inconsistency)),
                   code_plot =
                     data_availability_plots$code_plot[vec_inconsistency],
                   plot_id = data_availability_plots$plot_id[vec_inconsistency],
                   code_layer_horizon_master = rep(NA,
                                                   length(vec_inconsistency)),
                   repetition_profile_pit_id = rep(NA,
                                                   length(vec_inconsistency)),
                   code_line = rep(NA, length(vec_inconsistency)),
                   parameter = (rep("code_forest_type",
                                    length(vec_inconsistency))),
                   parameter_unit = rep("-", length(vec_inconsistency)),
                   parameter_value = rep(NA, length(vec_inconsistency)),
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record =
                     rep(TRUE, length(vec_inconsistency)),
                   change_date = rep(NA, length(vec_inconsistency)),
                   download_date = rep(download_date_pir,
                                       length(vec_inconsistency))))
  }






  # 3. FSCC_22: Check whether data have been reported in the correct units

  # Check if this has to be tested for the given survey_form

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "pfh", "swc")) {

  # Create a new column "unique_partner_survey", which is a combination of
  # "partner_code" and "survey_year"
  # Because of the assumption that, when a partner is submitting data for a
  # certain survey year, the data of a certain parameter are all reported
  # in the same units. So if a mistake in units has been made, it should be
  # the case for all data of the given "unique_partner_survey"

  df$unique_partner_survey <- paste0(df$partner_code, "_", df$survey_year)

  # For parameter for which units can be potentially wrong:

  for (i in seq_along(parameters_wrong_units)) {

    # Determine the column index in which this parameter is reported in df

    col_ind <- which(names(df) == parameters_wrong_units[i])

    # Depending on the given parameter, determine the plausible range for
    # organic versus mineral matrices in which data would probably be situated
    # if they would be reported in the wrong units. Save these as
    # "wrong_range_org" and "wrong_range_mineral"

    # In addition, create new columns in df for each parameter
    # (named with the parameter name and "_wrong_unit") in which we will store
    # the information whether a given observation was reported in the wrong
    # unit.

    if (parameters_wrong_units[i] %in% c("bulk_density",
                                         "horizon_bulk_dens_measure")) {
      df$bulk_density_wrong_unit <- NA
      wrong_range_org <- c(0, 2.65)
      wrong_range_mineral <- c(0, 2.65) # g cm-3
      } else

    if (parameters_wrong_units[i] == "horizon_bulk_dens_est") {
        df$bulk_density_est_wrong_unit <- NA
      wrong_range_org <- c(0, 2.65)
      wrong_range_mineral <- c(0, 2.65) # g cm-3
      } else

    if (parameters_wrong_units[i] %in% c("organic_carbon_total",
                                         "horizon_c_organic_total")) {
      df$organic_carbon_wrong_unit <- NA
      wrong_range_org <- c(0.04, 59)
      wrong_range_mineral <- c(0.04, 15) # %
      } else

    if (parameters_wrong_units[i] %in% c("n_total", "horizon_n_total")) {
      df$total_nitrogen_wrong_unit <- NA
      wrong_range_org <- c(0.005, 3.2)
      wrong_range_mineral <- c(0.005, 1) # %
      } else

    if (parameters_wrong_units[i] == "horizon_gypsum") {
      df$gypsum_wrong_unit <- NA
      wrong_range_org <- c(0, 100)
      wrong_range_mineral <- c(0, 100) # %
      } else

    if (parameters_wrong_units[i] == "horizon_caco3_total") {
      df$caco3_wrong_unit <- NA
      wrong_range_org <- c(0, 36)
      wrong_range_mineral <- c(0, 66) # %
  }

    # For each "unique_partner_survey":

    for (j in seq_along(unique(df$unique_partner_survey))) {

      # Determine the row indices, data and layer types of the records which do
      # contain data for the given parameter in the given
      # "unique_partner_survey"

      vec <- which(df$unique_partner_survey ==
                     unique(df$unique_partner_survey)[j])
      vec_nonempty <- vec[which(!is.na(df[vec, col_ind]))]
      vec_data <- df[vec, col_ind]
      vec_data <- vec_data[which(!is.na(vec_data))]
      vec_layer_type <- df$layer_type[vec_nonempty]

      # Check if there are any data

      if (!identical(vec_data, logical(0))) {

        # Check for each layer/matrix type (mineral versus organic) whether at
        # least 90 % of the data for the given unique_partner_survey are
        # within the plausible range with the wrong units.
        # For example, for total organic carbon: is at least 90 % of the data of
        # a given unique_partner_survey between 0.04 and 59 (organic layers) or
        # between 0.04 and 15 (mineral layers)?
        # Then, it is assumed that the data are reported in the wrong unit.
      
        # This is probably the most robust way to implement this test.
        # Indeed, for bulk density, data in the "wrong" units are obvious due to
        # the factor 1000 difference, but this is less straightforward for the
        # other parameters (factor 10 difference), because of which this needs
        # to be assessed by checking multiple observations. (This is why
        # this is only tested for "unique_partner_surveys" with at least two
        # observations)

        if ((length(which((vec_layer_type != "mineral" &
                   vec_data > wrong_range_org[1] &
                   vec_data < wrong_range_org[2]) |
                  (vec_layer_type == "mineral" &
                     vec_data > wrong_range_mineral[1] &
                     vec_data < wrong_range_mineral[2]))) >=
             0.9 * length(vec_data)) &&
            length(vec_data) >= 2) {

          # Store the information whether a given observation was reported in
          # the wrong unit in "df" (TRUE)
          # This is done so that the "plausible range" test (FSCC_14) would
          # exclude these values (since they are anyway likely to be outside
          # the plausible range)

          if (parameters_wrong_units[i] %in% c("bulk_density",
                                               "horizon_bulk_dens_measure")) {
            df$bulk_density_wrong_unit[vec_nonempty] <- TRUE
            } else

          if (parameters_wrong_units[i] == "horizon_bulk_dens_est") {
            df$bulk_density_est_wrong_unit[vec_nonempty] <- TRUE
            } else

          if (parameters_wrong_units[i] %in% c("organic_carbon_total",
                                               "horizon_c_organic_total")) {
            df$organic_carbon_wrong_unit[vec_nonempty] <- TRUE
            } else

          if (parameters_wrong_units[i] %in% c("n_total", "horizon_n_total")) {
            df$total_nitrogen_wrong_unit[vec_nonempty] <- TRUE
            } else

          if (parameters_wrong_units[i] == "horizon_caco3_total") {
            df$caco3_wrong_unit[vec_nonempty] <- TRUE
            } else

          if (parameters_wrong_units[i] == "horizon_gypsum") {
            df$gypsum_wrong_unit[vec_nonempty] <- TRUE
            }

          # Store information about the inconsistency in
          # "list_range_inconsistencies"

          if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {
            ind_layer_horizon <- df$horizon_master[vec_nonempty]
            ind_repetition_profile_pit_id <- df$profile_pit_id[vec_nonempty]
          } else

          if (unlist(strsplit(survey_form, "_"))[2] == "som")
          {if ("code_layer_original" %in% names(df)) {
            ind_layer_horizon <-
              as.character(df$code_layer_original[vec_nonempty])
            } else {
              ind_layer_horizon <- as.character(df$code_layer[vec_nonempty])
            }
          ind_repetition_profile_pit_id <- df$repetition[vec_nonempty]
          } else

          if (survey_form == "sw_swc") {
            ind_layer_horizon <- df$code_depth_layer[vec_nonempty]
            ind_repetition_profile_pit_id <- rep(NA, length(vec_nonempty))
          }

          rule_id <- "FSCC_22"
          inconsistency_reason <-
            inconsistency_catalogue$inconsistency_reason[
              which(inconsistency_catalogue$rule_id == rule_id)]

          if (parameters_wrong_units[i] %in% c("bulk_density",
                                               "horizon_bulk_dens_measure",
                                               "horizon_bulk_dens_est")) {

          ind_parameter_units <- as.factor(rep("kg m-3", length(vec_nonempty)))
          inconsistency_reason <- paste0(inconsistency_reason,
                  " (i.e. reported in g cm-3 instead of kg m-3).")
          } else

          if (parameters_wrong_units[i] %in% c("organic_carbon_total",
                                               "n_total",
                                               "horizon_c_organic_total",
                                               "horizon_n_total",
                                               "horizon_caco3_total",
                                               "horizon_gypsum")) {
          ind_parameter_units <- as.factor(rep("g kg-1", length(vec_nonempty)))
          inconsistency_reason <- paste0(inconsistency_reason,
                  " (i.e. reported in % instead of g kg-1).")
          }

          inconsistency_type <-
            inconsistency_catalogue$inconsistency_type[
              which(inconsistency_catalogue$rule_id == rule_id)]

          list_range_inconsistencies <- rbind(
            list_range_inconsistencies,
            data.frame(survey_form = as.factor(rep(survey_form,
                                                   length(vec_nonempty))),
                       partner = df$partner[vec_nonempty],
                       partner_code = df$partner_code[vec_nonempty],
                       country = df$country[vec_nonempty],
                       code_country = df$code_country[vec_nonempty],
                       survey_year = df$survey_year[vec_nonempty],
                       code_plot = df$code_plot[vec_nonempty],
                       plot_id = df$plot_id[vec_nonempty],
                       code_layer_horizon_master = ind_layer_horizon,
                       repetition_profile_pit_id =
                         ind_repetition_profile_pit_id,
                       code_line = df$code_line[vec_nonempty],
                       parameter = as.factor(rep(parameters_wrong_units[i],
                                                 length(vec_nonempty))),
                       parameter_unit = ind_parameter_units,
                       parameter_value = vec_data,
                       inconsistency_reason = inconsistency_reason,
                       inconsistency_type = inconsistency_type,
                       rule_id = rule_id,
                       non_duplicated_error_type_per_record =
                         rep(TRUE, length(vec_nonempty)),
                       change_date = df$change_date[vec_nonempty],
                       download_date = rep(download_date_pir,
                                           length(vec_nonempty))))


          # If solve is TRUE: convert the data in assumedly wrong units to the
          # correct units

          if (solve == TRUE) {

            # reported in g cm-3 instead of kg m-3
            if (parameters_wrong_units[i] %in% c("bulk_density",
                                                 "horizon_bulk_dens_measure",
                                                 "horizon_bulk_dens_est")) {
              df[vec_nonempty, col_ind] <- 1000 * df[vec_nonempty, col_ind]
              }

            # reported in % instead of g kg-1
            if (parameters_wrong_units[i] %in% c("organic_carbon_total",
                                                 "n_total",
                                                 "horizon_c_organic_total", 
                                                 "horizon_n_total", 
                                                 "horizon_caco3_total", 
                                                 "horizon_gypsum")) {
              df[vec_nonempty, col_ind] <- 10 * df[vec_nonempty, col_ind]
            }
            }

        }
      }
      }

    # Update the progress bar

    setTxtProgressBar(progress_bar, (length_parameters_mandatory + i))
  }
  }











  # 4. FSCC 38 and FSCC 14: Check whether values are in possible or plausible 
  # range

  # Check if this has to be tested for the given survey_form

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "pfh", "swc")) {

  # For each of the parameters for which this has to be tested

  for (i in seq_along(parameters)) {

    # Store the name of the given parameter/column, and rename the column in
    # df temporarily, so that it is easier to refer to this column throughout
    # this code

    column_name <- parameters[i]
    names(df)[which(names(df) == parameters[i])] <- "active_column"

    # Assign possible ranges for the given parameter
    # If no possible range has been provided, consider it possible to be
    # "infinite"

    range_min_possible <-
      ranges_qaqc$min_possible[which(ranges_qaqc$parameter == parameters[i])][1]
    range_max_possible <-
      ranges_qaqc$max_possible[which(ranges_qaqc$parameter == parameters[i])][1]

    if (is.na(range_min_possible)) {
      range_min_possible <- (-1e99)
    }

    if (is.na(range_max_possible)) {
      range_max_possible <- 1e99
    }

    # For each of the records of the given parameter column

    for (j in seq_along(df$active_column)) {

      # Assign plausible ranges for the given parameter and layer,
      # based on its layer type

      # Check if the layer type is specified

      if (!is.na(df$layer_type[j])) {

      # If the given survey form is "som"

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {

        # The plausible range of layer "M05" - which is located at the
        # transition between forest floor and mineral, and therefore still
        # contains a considerable influence by characteristics of organic
        # layers - is wider than the plausible ranges of mineral and
        # organic matrices for the parameters "bulk density",
        # "organic_carbon_total" and "n_total".

        if  (df$code_layer[j] == "M05" &&
             (column_name %in% c("bulk_density",
                                 "organic_carbon_total",
                                 "n_total"))) {

          range_min <- min(ranges_qaqc$min_org[
            which(ranges_qaqc$parameter == parameters[i])],
                           ranges_qaqc$min_mineral[
                             which(ranges_qaqc$parameter == parameters[i])])
          range_max <- max(ranges_qaqc$max_org[
            which(ranges_qaqc$parameter == parameters[i])],
                           ranges_qaqc$max_mineral[
                             which(ranges_qaqc$parameter == parameters[i])])

          } else

        # If the given layer is mineral: assign plausible ranges for mineral
        # matrices of the given parameter

        if (df$layer_type[j] == "mineral") {
          range_min <- ranges_qaqc$min_mineral[
            which(ranges_qaqc$parameter == parameters[i])]
          range_max <- ranges_qaqc$max_mineral[
            which(ranges_qaqc$parameter == parameters[i])]
          } else

        # If the given layer is organic: assign plausible ranges for organic
        # matrices of the given parameter

        if ((df$layer_type[j] == "forest_floor" ||
             df$layer_type[j] == "peat")) {
          range_min <- ranges_qaqc$min_org[
            which(ranges_qaqc$parameter == parameters[i])]
          range_max <- ranges_qaqc$max_org[
            which(ranges_qaqc$parameter == parameters[i])]
        }
      }


      # If the given survey form is "swc"

      if (unlist(strsplit(survey_form, "_"))[2] == "swc") {

        # If the given layer is mineral: assign plausible ranges for mineral
        # matrices of the given parameter

        if (df$layer_type[j] == "mineral") {
          range_min <- ranges_qaqc$min_mineral[
            which(ranges_qaqc$parameter == parameters[i])]
          range_max <- ranges_qaqc$max_mineral[
            which(ranges_qaqc$parameter == parameters[i])]
          } else

        # If the given layer is organic: assign plausible ranges for organic
        # matrices of the given parameter

        if ((df$layer_type[j] == "forest_floor" ||
             df$layer_type[j] == "peat")) {
          range_min <- ranges_qaqc$min_org[
            which(ranges_qaqc$parameter == parameters[i])]
          range_max <- ranges_qaqc$max_org[
            which(ranges_qaqc$parameter == parameters[i])]
        }
        }


      # If the given survey form is "pfh"

      if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

        # If the given layer is mineral and the parameter is not "horizon_ph"
        # (see below why):
        # assign plausible ranges for mineral matrices of the given parameter

        if (df$layer_type[j] == "mineral" && column_name != "horizon_ph") {
          range_min <- ranges_qaqc$min_mineral[
            which(ranges_qaqc$parameter == parameters[i])]
          range_max <- ranges_qaqc$max_mineral[
            which(ranges_qaqc$parameter == parameters[i])]
          } else

          # If the given layer is organic and the parameter is not
          # "horizon_ph" (see below why):
          # assign plausible ranges for organic matrices of the given parameter

          if ((df$layer_type[j] == "forest_floor" ||
               df$layer_type[j] == "peat") &&
              column_name != "horizon_ph") {

            range_min <- ranges_qaqc$min_org[
              which(ranges_qaqc$parameter == parameters[i])]
            range_max <- ranges_qaqc$max_org[
              which(ranges_qaqc$parameter == parameters[i])]
            } else

        # This is stated in the manual for the parameter "horizon_ph" in
        # "pfh" survey forms:
        # "Please specify in “other_observations” if distilled water or KCl
        # was used"
        # Hence, we need to figure out based on the column "other_obs",
        # whether the plausible ranges for pH_H2O versus pH_CaCl2 apply
        # for the column "horizon_ph"

        if (column_name == "horizon_ph") {
          ind_pH <- NA
          range_min <- NA
          range_max <- NA

        # Check if anything is written in the "other_obs" column

        if (!is.na(df$other_obs[j])) {

        # If a reference to H2O can be found in this column, consider the pH
        # as pH_H2O, and assign the corresponding row index in the
        # "ranges_qaqc" dataframe

        if (str_detect(df$other_obs[j], "H2O|H20|water")) {
          ind_pH <- which(ranges_qaqc$parameter_som == "ph_h2o")
          } else

          # If a reference to CaCl2 can be found in this column, consider the pH
          # as pH_CaCl2, and assign the corresponding row index in the
          # "ranges_qaqc" dataframe

          if (str_detect(df$other_obs[j], "CaCl2")) {
            ind_pH <- which(ranges_qaqc$parameter_som == "ph_cacl2")
            }

          # If any row index has been assigned (i.e. if we know which kind of
          # pH extraction occurred):

          if (!is.na(ind_pH)) {

          # Assign plausible ranges, depending on whether the given layer is
          # mineral or organic

          if (df$layer_type[j] == "mineral") {
            range_min <- ranges_qaqc$min_mineral[ind_pH]
            range_max <- ranges_qaqc$max_mineral[ind_pH]
            } else

          if (df$layer_type[j] == "forest_floor" ||
              df$layer_type[j] == "peat") {
            range_min <- ranges_qaqc$min_org[ind_pH]
            range_max <- ranges_qaqc$max_org[ind_pH]
          }
          }
        }
        }
      }
      }




    # Error: outside possible range

      # If a value was reported for the given parameter, and if we know the
      # possible range

      if (!is.na(range_min_possible) && !is.na(range_max_possible) &&
          !is.na(df$active_column[j])) {

        # Ignore the parameter value if a value of -1 was reported, because this
        # indicates that the measurement was below a limit of quantification.

        # If the parameter value was outside the possible range

        if (df$active_column[j] != -1 &&
            (df$active_column[j] < range_min_possible ||
             df$active_column[j] > range_max_possible)) {

          # Store information about the inconsistency in
          # "list_range_inconsistencies"

          if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {
            ind_layer_horizon <- df$horizon_master[j]
            ind_repetition_profile_pit_id <- df$profile_pit_id[j]
            } else

          if (unlist(strsplit(survey_form, "_"))[2] == "som") {
            if ("code_layer_original" %in% names(df)) {
              ind_layer_horizon <- as.character(df$code_layer_original[j])
            } else {
                ind_layer_horizon <- as.character(df$code_layer[j])
                }
          ind_repetition_profile_pit_id <- df$repetition[j]
          } else

          if (survey_form == "sw_swc") {
            ind_layer_horizon <- df$code_depth_layer[j]
            ind_repetition_profile_pit_id <- rep(NA, length(j))
            }

          rule_id <- "FSCC_38"
          inconsistency_reason <-
            inconsistency_catalogue$inconsistency_reason[
              which(inconsistency_catalogue$rule_id == rule_id)]
          inconsistency_reason <-
            paste0(inconsistency_reason, " (", range_min_possible,
                   " - ", range_max_possible, ").")
          inconsistency_type <-
            inconsistency_catalogue$inconsistency_type[
              which(inconsistency_catalogue$rule_id == rule_id)]

          list_range_inconsistencies <- rbind(
            list_range_inconsistencies,
            data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                       partner = df$partner[j],
                       partner_code = df$partner_code[j],
                       country = df$country[j],
                       code_country = df$code_country[j],
                       survey_year = df$survey_year[j],
                       code_plot = df$code_plot[j],
                       plot_id = df$plot_id[j],
                       code_layer_horizon_master = ind_layer_horizon,
                       repetition_profile_pit_id =
                         ind_repetition_profile_pit_id,
                       code_line = df$code_line[j],
                       parameter = as.factor(rep(parameters[i], length(j))),
                       parameter_unit = as.factor(rep(ranges_qaqc$unit[
                         which(parameters[i] == ranges_qaqc$parameter)],
                         length(j))),
                       parameter_value = df$active_column[j],
                       inconsistency_reason = inconsistency_reason,
                       inconsistency_type = inconsistency_type,
                       rule_id = rule_id,
                       non_duplicated_error_type_per_record = rep(TRUE,
                                                                  length(j)),
                       change_date = df$change_date[j],
                       download_date = rep(download_date_pir, length(j))))

        }
        }


     # Error: outside plausible range

     # If a value was reported for the given parameter, and if we know the
     # plausible range

     if (!is.na(range_min) && !is.na(range_max) &&
         !is.na(df$active_column[j])) {

     # Ignore the parameter value if a value of -1 was reported, because this
     # indicates that the measurement was below a limit of quantification.

     # If the parameter value was outside the plausible range (but still inside
     # the possible range)

     if (df$active_column[j] != -1 &&
         (df$active_column[j] < range_min ||
          df$active_column[j] > range_max) &&
         df$active_column[j] > range_min_possible &&
         df$active_column[j] < range_max_possible) {

     # Check whether the parameter value was possibly reported in the wrong
     # units because we can ignore these data for now

     j_wrong_units <- NA

       if (column_name %in% c("bulk_density", "horizon_bulk_dens_measure")) {
         j_wrong_units <- df$bulk_density_wrong_unit[j]
         } else
       if (column_name == "horizon_bulk_dens_est") {
         j_wrong_units <- df$bulk_density_est_wrong_unit[j]
         } else
       if (column_name %in% c("organic_carbon_total",
                              "horizon_c_organic_total")) {
         j_wrong_units <- df$organic_carbon_wrong_unit[j]
         } else
       if (column_name %in% c("n_total", "horizon_n_total")) {
         j_wrong_units <- df$total_nitrogen_wrong_unit[j]
         } else
       if (column_name == "horizon_caco3_total") {
         j_wrong_units <- df$caco3_wrong_unit[j]
         } else
       if (column_name == "horizon_gypsum") {
         j_wrong_units <- df$gypsum_wrong_unit[j]
         }

       # Only if the parameter value was probably reported in the correct units:

       if ((!column_name %in% parameters_wrong_units) ||
          ((column_name %in% parameters_wrong_units) &&
           (is.na(j_wrong_units)))) {

       # Store information about the inconsistency in
       # "list_range_inconsistencies"

       if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {
         ind_layer_horizon <- df$horizon_master[j]
         ind_repetition_profile_pit_id <- df$profile_pit_id[j]
         } else
          
         if (unlist(strsplit(survey_form, "_"))[2] == "som") {
           if ("code_layer_original" %in% names(df)) {
           ind_layer_horizon <- as.character(df$code_layer_original[j])
           } else {
             ind_layer_horizon <- as.character(df$code_layer[j])
             }
           ind_repetition_profile_pit_id <- df$repetition[j]
           } else

         if (survey_form == "sw_swc") {
           ind_layer_horizon <- df$code_depth_layer[j]
           ind_repetition_profile_pit_id <- rep(NA, length(j))
          }

       rule_id <- "FSCC_14"
       inconsistency_reason <-
         inconsistency_catalogue$inconsistency_reason[
           which(inconsistency_catalogue$rule_id == rule_id)]
       inconsistency_reason <-
         paste0(inconsistency_reason, " (", range_min, " - ", range_max, ").")
       inconsistency_type <-
         inconsistency_catalogue$inconsistency_type[
           which(inconsistency_catalogue$rule_id == rule_id)]

       list_range_inconsistencies <- rbind(
          list_range_inconsistencies,
          data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                     partner = df$partner[j],
                     partner_code = df$partner_code[j],
                     country = df$country[j],
                     code_country = df$code_country[j],
                     survey_year = df$survey_year[j],
                     code_plot = df$code_plot[j],
                     plot_id = df$plot_id[j],
                     code_layer_horizon_master = ind_layer_horizon,
                     repetition_profile_pit_id = ind_repetition_profile_pit_id,
                     code_line = df$code_line[j],
                     parameter = as.factor(rep(parameters[i], length(j))),
                     parameter_unit = as.factor(rep(ranges_qaqc$unit[
                       which(parameters[i] == ranges_qaqc$parameter)][1],
                       length(j))),
                     parameter_value = df$active_column[j],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record = rep(TRUE,
                                                                length(j)),
                     change_date = df$change_date[j],
                     download_date = rep(download_date_pir, length(j)))
       )
       }
     }
    }



      # FSCC_12: for pfh$horizon_ph: if there are pH values without specification
      # of the extraction method: caution

      # If the given column is "horizon_ph", and if a value has been reported
      # while no extraction method has been specified in the "other_obs" column:

      if (column_name == "horizon_ph") {
        if (!is.na(df$active_column[j]) &&
           (is.na(df$other_obs[j]) ||
            !str_detect(df$other_obs[j], "H2O|H20|water|CaCl2"))) {
      
      # Store information about the inconsistency in
      # "list_range_inconsistencies"

      rule_id <- "FSCC_12"
      inconsistency_reason <-
        inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <-
        inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

        list_range_inconsistencies <- rbind(
           list_range_inconsistencies,
           data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                      partner = df$partner[j],
                      partner_code = df$partner_code[j],
                      country = df$country[j],
                      code_country = df$code_country[j],
                      survey_year = df$survey_year[j],
                      code_plot = df$code_plot[j],
                      plot_id = df$plot_id[j],
                      code_layer_horizon_master = df$horizon_master[j],
                      repetition_profile_pit_id = df$profile_pit_id[j],
                      code_line = df$code_line[j],
                      parameter = as.factor(rep(parameters[i], length(j))),
                      parameter_unit = as.factor(rep(ranges_qaqc$unit[
                        which(parameters[i] == ranges_qaqc$parameter)][1],
                        length(j))),
                      parameter_value = df$active_column[j],
                      inconsistency_reason = inconsistency_reason,
                      inconsistency_type = inconsistency_type,
                      rule_id = rule_id,
                      non_duplicated_error_type_per_record = rep(TRUE,
                                                                 length(j)),
                      change_date = df$change_date[j],
                      download_date = rep(download_date_pir, length(j))))
      }

      }

      }


    # Rename the column with its original name

    names(df)[which(names(df) == "active_column")] <- column_name

    # Update the progress bar

    setTxtProgressBar(progress_bar, (length_parameters_mandatory +
                                       length_parameters_wrong_units + i))
  }
  }






  # 5. FSCC_41: Possible codes

  # Check if this has to be tested for the given survey_form

  if (unlist(strsplit(survey_form, "_"))[2] %in%
      c("som", "pfh", "prf", "pls")) {

  # For each of the parameters for which this has to be tested

  for (i in seq_along(parameters_code)) {

    # Determine which codes can possibly be assigned to the given parameter
    # according to the manual

    parameter_code <- parameters_code[i]
    possible_codes <- list_codes[[which(names(list_codes) == parameter_code)]]

    # Determine the column index of the given parameter in df

    col_ind <- which(names(df) == parameter_code)

    # Create a vector with the row indices of the records with a parameter value
    # which is not in the list of possible codes for this parameter

    vec_inconsistency <- which(!is.na(df[, col_ind]) &
                                 (df[, col_ind] != "") &
                                 (!df[, col_ind] %in% possible_codes))

    # If there are any records with a "non-existing" code

    if (!identical(vec_inconsistency, integer(0))) {

    # Store information about the inconsistency in "list_range_inconsistencies"

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {
      if ("code_layer_original" %in% names(df)) {

        ind_layer_horizon <-
          as.character(df$code_layer_original[vec_inconsistency])
      } else {
          ind_layer_horizon <- as.character(df$code_layer[vec_inconsistency])
      }

      ind_repetition_profile_pit_id <- df$repetition[vec_inconsistency]

    } else if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {
      ind_layer_horizon <- df$horizon_master[vec_inconsistency]
      ind_repetition_profile_pit_id <- df$profile_pit_id[vec_inconsistency]

    } else if (unlist(strsplit(survey_form, "_"))[2] == "prf") {
      ind_layer_horizon <- rep(NA, length(vec_inconsistency))
      ind_repetition_profile_pit_id <- df$profile_pit_id[vec_inconsistency]

    } else if (unlist(strsplit(survey_form, "_"))[2] == "pls") {
      ind_layer_horizon <- rep(NA, length(vec_inconsistency))
      ind_repetition_profile_pit_id <- rep(NA, length(vec_inconsistency))
      }

      vec_data <- df[vec_inconsistency, col_ind]

      if (is.factor(vec_data)) {
        vec_data <- as.character(vec_data)
      }

      rule_id <- "FSCC_41"
      inconsistency_reason <-
        inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <-
        inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

      list_range_inconsistencies <- rbind(
        list_range_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form,
                                               length(vec_inconsistency))),
                   partner = df$partner[vec_inconsistency],
                   partner_code = df$partner_code[vec_inconsistency],
                   country = df$country[vec_inconsistency],
                   code_country = df$code_country[vec_inconsistency],
                   survey_year = df$survey_year[vec_inconsistency],
                   code_plot = df$code_plot[vec_inconsistency],
                   plot_id = df$plot_id[vec_inconsistency],
                   code_layer_horizon_master = ind_layer_horizon,
                   repetition_profile_pit_id = ind_repetition_profile_pit_id,
                   code_line = df$code_line[vec_inconsistency],
                   parameter = as.factor(rep(parameter_code,
                                             length(vec_inconsistency))),
                   parameter_unit = rep("-", length(vec_inconsistency)),
                   parameter_value = vec_data,
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record =
                     rep(TRUE, length(vec_inconsistency)),
                   change_date = df$change_date[vec_inconsistency],
                   download_date = rep(download_date_pir,
                                       length(vec_inconsistency))))
      }

    # Update the progress bar

    setTxtProgressBar(progress_bar, (length_parameters_mandatory +
                      length_parameters_wrong_units +
                        length_parameters_range + i))
  }
  }





  # FSCC_46: one of the three texture classes (clay/silt/sand) is missing
  # while the others are not.

  # Check if this has to be tested for the given survey_form

  if (unlist(strsplit(survey_form, "_"))[2] %in% c("som", "pfh")) {

    # Create a dataframe "textures" which respectively contains the columns with
    # clay, silt and sand fractions

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {
    textures <- cbind(df$part_size_clay,
                      df$part_size_silt,
                      df$part_size_sand)
    }

    if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {
      textures <- cbind(df$horizon_clay,
                        df$horizon_silt,
                        df$horizon_sand)
      }

    # Create a vector with row indices with only two out of three texture
    # fractions reported

    vec_inconsistency <- which(rowSums(!is.na(textures)) == 2)

    # Store information about the inconsistency in "list_range_inconsistencies"

    if (!identical(vec_inconsistency, integer(0))) {

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {

      if ("code_layer_original" %in% names(df)) {
        ind_layer_horizon <- as.character(df$code_layer_original)
      } else {
          ind_layer_horizon <- as.character(df$code_layer)
          }

      ind_repetition_profile_pit_id <- df$repetition
      ind_parameter_clay <- "part_size_clay"
      ind_parameter_silt <- "part_size_silt"
      ind_parameter_sand <- "part_size_sand"
      }

    if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

      ind_layer_horizon <- df$horizon_master
      ind_repetition_profile_pit_id <- df$profile_pit_id
      ind_parameter_clay <- "horizon_clay"
      ind_parameter_silt <- "horizon_silt"
      ind_parameter_sand <- "horizon_sand"
      }

    rule_id <- "FSCC_46"
    inconsistency_reason <-
      inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
    inconsistency_type <-
      inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

    for (i in vec_inconsistency) {
    list_range_inconsistencies <- rbind(
      list_range_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form, length(i))),
                 partner = df$partner[i],
                 partner_code = df$partner_code[i],
                 country = df$country[i],
                 code_country = df$code_country[i],
                 survey_year = df$survey_year[i],
                 code_plot = df$code_plot[i],
                 plot_id = df$plot_id[i],
                 code_layer_horizon_master = ind_layer_horizon[i],
                 repetition_profile_pit_id = ind_repetition_profile_pit_id[i],
                 code_line = df$code_line[i],
                 parameter = ind_parameter_clay,
                 parameter_unit = rep("%wt", length(i)),
                 parameter_value = textures[i, 1],
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(TRUE, length(i)),
                 change_date = df$change_date[i],
                 download_date = rep(download_date_pir, length(i))))

    list_range_inconsistencies <- rbind(
      list_range_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form, length(i))),
                 partner = df$partner[i],
                 partner_code = df$partner_code[i],
                 country = df$country[i],
                 code_country = df$code_country[i],
                 survey_year = df$survey_year[i],
                 code_plot = df$code_plot[i],
                 plot_id = df$plot_id[i],
                 code_layer_horizon_master = ind_layer_horizon[i],
                 repetition_profile_pit_id = ind_repetition_profile_pit_id[i],
                 code_line = df$code_line[i],
                 parameter = ind_parameter_silt,
                 parameter_unit = rep("%wt", length(i)),
                 parameter_value = textures[i, 2],
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                 change_date = df$change_date[i],
                 download_date = rep(download_date_pir, length(i))))

    list_range_inconsistencies <- rbind(
      list_range_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form, length(i))),
                 partner = df$partner[i],
                 partner_code = df$partner_code[i],
                 country = df$country[i],
                 code_country = df$code_country[i],
                 survey_year = df$survey_year[i],
                 code_plot = df$code_plot[i],
                 plot_id = df$plot_id[i],
                 code_layer_horizon_master = ind_layer_horizon[i],
                 repetition_profile_pit_id = ind_repetition_profile_pit_id[i],
                 code_line = df$code_line[i],
                 parameter = ind_parameter_sand,
                 parameter_unit = rep("%wt", length(i)),
                 parameter_value = textures[i,3],
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(FALSE,length(i)),
                 change_date = df$change_date[i],
                 download_date = rep(download_date_pir, length(i))))

    }
    }
    }




  # FSCC_43: is horizon_coarse_weight a weight percentage?

  # For survey form "pfh"

  if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

    # Evaluate per partner

    for (i in seq_along(unique(df$partner_code))) {

      partner_code_i <- unique(df$partner_code)[i]

      # Create a vector with row indices of all records of the given partner
      # after survey_year 2000

      vec <- which((df$partner_code == partner_code_i) &
                     (df$survey_year >= 2000))

      # If this vector is not empty

      if (!identical(vec, integer(0))) {

      # Finetune the vector by selecting the records for which
      # "horizon_coarse_weight" is provided (and does not equal 0)

      vec <- vec[which(!is.na(df$horizon_coarse_weight[vec]) &
                        (df$horizon_coarse_weight[vec] != 0))]
      }

      # If this vector is not empty

      if (!identical(vec, integer(0))) {

        # Store information about the inconsistency in
        # "list_range_inconsistencies"

        # Create just one line per partner in the inconsistency report,
        # and summarise the survey years by providing the range of survey years

        change_date_i <-
          paste0(format(as.Date((min(df$change_date[vec]))),"%Y"), " - ",
                 format(as.Date((max(df$change_date[vec]))),"%Y"))

        rule_id <- "FSCC_43"
        inconsistency_reason <-
          inconsistency_catalogue$inconsistency_reason[
            which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <-
          inconsistency_catalogue$inconsistency_type[
            which(inconsistency_catalogue$rule_id == rule_id)]

        list_range_inconsistencies <- rbind(
          list_range_inconsistencies,
          data.frame(survey_form = as.factor(rep(survey_form, length(i))),
                     partner = unique(df$partner[vec]),
                     partner_code = partner_code_i,
                     country = unique(df$country[vec]),
                     code_country = unique(df$code_country[vec]),
                     survey_year = NA,
                     code_plot = NA,
                     plot_id = NA,
                     code_layer_horizon_master = NA,
                     repetition_profile_pit_id = NA,
                     code_line = NA,
                     parameter = "horizon_coarse_weight",
                     parameter_unit = "%wt",
                     parameter_value = NA,
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record = TRUE,
                     change_date = change_date_i,
                     download_date = download_date_pir))
      }
    }
  }

  # Close the progress bar

  if (unlist(strsplit(survey_form, "_"))[2] %in%
      c("som", "pfh", "prf", "pls", "swc")) {
  close(progress_bar)
    }

  # Remove the columns that are no longer needed in the data frame

  if (any(names(df) %in% c("unique_partner_survey",
                           "bulk_density_wrong_unit",
                           "bulk_density_est_wrong_unit",
                           "organic_carbon_wrong_unit",
                           "total_nitrogen_wrong_unit",
                           "gypsum_wrong_unit",
                           "caco3_wrong_unit",
                           "coarse_fragment_vol_any_survey",
                           "bulk_density_any_survey",
                           "part_size_clay_any_survey",
                           "part_size_silt_any_survey",
                           "part_size_sand_any_survey",
                           "exch_ca_any_survey",
                           "rea_al_any_survey",
                           "rea_fe_any_survey",
                           "non_stony",
                           "calcareous_pH5",
                           "calcareous_pH6"))) {

  df <- df[, -which(names(df) %in% c("unique_partner_survey",
                                    "bulk_density_wrong_unit",
                                    "bulk_density_est_wrong_unit",
                                    "organic_carbon_wrong_unit",
                                    "total_nitrogen_wrong_unit",
                                    "gypsum_wrong_unit",
                                    "caco3_wrong_unit",
                                    "coarse_fragment_vol_any_survey",
                                    "bulk_density_any_survey",
                                    "part_size_clay_any_survey",
                                    "part_size_silt_any_survey",
                                    "part_size_sand_any_survey",
                                    "exch_ca_any_survey",
                                    "rea_al_any_survey",
                                    "rea_fe_any_survey",
                                    "non_stony",
                                    "calcareous_pH5",
                                    "calcareous_pH6"))]
  }

  # Save the survey form and list_range_inconsistencies for the given survey
  # form to the global environment

  assign(survey_form, df, envir = globalenv())
  assign(paste0("list_range_inconsistencies_", survey_form),
                list_range_inconsistencies, envir = globalenv())

  # Return the duration of this function run

  duration_run_r <- Sys.time() - start_time_r
  print(duration_run_r)

}
