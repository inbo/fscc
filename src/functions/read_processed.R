
#' Read processed solid soil data for ICP Forests soil analysis
#'
#' This function imports the processed data forms (e.g. intermediate, "layer 1",
#' "layer 2") for specified survey forms and path (breakpoint) from local
#' folders.
#'
#' @param survey_forms Character vector of survey forms to import (either survey
#' codes, e.g. "so", or survey forms, e.g. "so_som"). Default is NULL,
#' which reads all solid soil-related survey forms.
#' @param path_name Path to the directory containing the data
#' files. Default is NULL, which reads the most recent path (breakpoint).
#' @param save_to_env Logical which indicates whether the output dataframes
#' can be saved to the global environment and override any existing objects
#' with the same name. Default is FALSE.
#'
#' @details
#' Not for layer 0
#'
#'
#' @return None
#'
#' @examples
#' # Read all solid soil-related survey forms
#' read_processed()
#'
#' # Read specific survey forms from a specific path
#' read_processed(c("so_som", "so_prf"), "./path/to/data/")

read_processed <- function(survey_forms = NULL,
                           path_name = NULL,
                           save_to_env = FALSE) {

  source("./src/functions/assign_env.R")

# Define columns that should be numeric

  vec_numeric <-
    c("survey_year", "code_country", "code_plot", "repetition",
      "layer_limit_superior", "layer_limit_inferior",
      "subsamples", "moisture_content", "part_size_clay", "part_size_silt",
      "part_size_sand", "bulk_density",
      "coarse_fragment_vol", "organic_layer_weight", "ph_cacl2", "ph_h2o",
      "organic_carbon_total", "n_total",
      "carbonates", "exch_acidiy", "exch_al", "exch_ca", "exch_fe", "exch_k",
      "exch_mg", "exch_mn", "exch_na",
      "free_h", "extrac_al", "extrac_ca", "extrac_cd", "extrac_cr",
      "extrac_cu", "extrac_fe", "extrac_hg",
      "extrac_k", "extrac_mg", "extrac_mn", "extrac_na", "extrac_ni",
      "extrac_p", "extrac_pb", "extrac_s",
      "extrac_zn", "tot_al", "tot_ca", "tot_fe", "tot_k", "tot_mg",
      "tot_mn", "tot_na", "rea_al", "rea_fe",
      "exch_bce", "exch_ace", "exch_cec", "elec_cond", "ni",
      "base_saturation", "code_soil_horizon_sample_c",
      "p_ox", "partner_code", "q_flag", "line_nr",
      "horizon_number", "code_horizon_discont", "horizon_vertical",
      "horizon_limit_up", "horizon_limit_low",
      "code_horizon_destinct", "code_horizon_topo", "code_soil_structure",
      "horizon_clay", "horizon_silt",
      "horizon_sand", "code_horizon_coarse_vol", "horizon_coarse_weight",
      "horizon_c_organic_total", "horizon_n_total",
      "horizon_caco3_total", "horizon_gypsum", "horizon_ph",
      "horizon_elec_cond", "horizon_exch_ca", "horizon_exch_mg",
      "horizon_exch_k", "horizon_exch_na", "horizon_cec",
      "code_horizon_porosity", "horizon_bulk_dens_measure",
      "horizon_bulk_dens_est", "code_roots_very_fine", "code_roots_fine",
      "code_roots_medium", "code_roots_coarse",
      "latitude", "longitude", "elevation_profile", "depth_diagnostic_1",
      "depth_diagnostic_2", "depth_diagnostic_3",
      "depth_diagnostic_4", "depth_diagnostic_5", "depth_diagnostic_6",
      "depth_diagnostic_7", "depth_diagnostic_8",
      "depth_diagnostic_9", "code_parent_material_1",
      "code_parent_material_2", "code_water_level_high", "code_water_level_low",
      "code_water_type", "code_water", "code_humus", "eff_soil_depth",
      "rooting_depth", "rock_depth", "obstacle_depth",
      "code_altitude", "quantification_limit", "control_chart_mean",
      "control_chart_std", "code_event_location",
      "code_tools", "trees_before", "trees_loss", "basal_area_before",
      "basal_area_loss", "code_logging_method",
      "code_extraction_tools", "code_extraction_method", "code_slash_disposal",
      "size_residues", "code_soil_compaction_ways",
      "code_soil_compaction_area", "soil_preparation_area", "chemics_quantity",
      "code_treatment_aim", "last_year",
      "relocated_plot", "altitude_m", "slope", "code_plot_design",
      "code_orientation", "plot_size", "code_stand_history",
      "code_prev_landuse", "code_stand_actual", "code_tree_species",
      "code_tree_species_mix", "top_height",
      "code_det_top_height", "code_forest_type", "code_mean_age",
      "code_tree_layers", "coverage_tree_layer", "canopy_closure",
      "code_status_mcpfe_class1", "code_status_mcpfe_class2",
      "code_status_mcpfe_class3", "code_fencing",
      "code_notimb_util_plot", "code_notimb_util_buffer", "code_manage_type",
      "code_manage_intensity_plot",
      "code_manage_intensity_buffer", "code_silvicult_system",
      "code_forest_owner", "cutting_year", "code_canopy_gaps",
      "stand_rotation", "code_plot_status", "code_nfi_status",
      "cc_tree_number", "azimuth", "distance",
      "latitude_dec", "longitude_dec")
  
# Define vectors with survey forms and codes to read ----

  survey_forms_all <- c("so_som", "so_prf", "so_pls", "so_pfh", "so_lqa",
                        "s1_som", "s1_prf", "s1_pls", "s1_pfh", "s1_lqa",
                        "si_eve", "si_plt", "si_sta", "si_tco", "y1_pl1",
                        "y1_st1", "sw_swa", "sw_swc")

  list_data_tables <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                           s1 = c("som", "prf", "pls", "pfh", "lqa"),
                           si = c("eve", "plt", "sta", "tco"),
                           y1 = c("pl1", "st1", "ev1"),
                           sw = c("swa", "swc"))

  # To remember the original input
  survey_forms_orig <- survey_forms

  # If no "survey_forms" argument is provided,
  # read all solid soil-related survey forms (by default)

  if (is.null(survey_forms_orig)) {
    survey_forms <- survey_forms_all
    }

  # If "survey_forms" argument is provided, replace any survey codes (e.g. "so")
  # by the actual survey forms (e.g. "so_som", "so_prf"...)

  if (!is.null(survey_forms_orig)) {

    survey_forms_extra <- NULL

    # Check for each of the reported characters whether they represent a code
    # of a survey (length = 1) or a survey form (length = 2)

    for (i in seq_along(survey_forms_orig)) {

      if (length(unlist(strsplit(survey_forms_orig[i], "_"))) == 1) {

        # Replace the code by the actual survey forms
        survey_forms_extra <- c(survey_forms_extra,
                                paste0(survey_forms_orig[i], "_",
                                       list_data_tables[[which(
                                         names(list_data_tables) ==
                                           survey_forms_orig[i])]]))

        survey_forms[i] <- NA
        }
      }

    if (!identical(survey_forms_extra, NULL)) {

      survey_forms <- c(survey_forms, survey_forms_extra)
      survey_forms <- survey_forms[which(!is.na(survey_forms))]
      }
    }




  # Create a vector with the codes of the different surveys

  survey_codes <- NULL

  for (i in seq_along(survey_forms)) {
    survey_codes <- c(survey_codes,
                      unlist(strsplit(survey_forms[i], "_"))[1])
    }

  survey_codes <- unique(survey_codes)



# Define path/breakpoint to read from ----

  # If no "path_name" argument is provided,
  # read from the most recent path (default)

if (is.null(path_name)) {

  data_paths <-   # TO KEEP UP-TO-DATE!
    data.frame(subfolder = c("raw_data",
                             "intermediate_data",
                             "0_01_intermediate_data",
                             "0_02_intermediate_data",
                             "layer1_data",
                             "layer2_data"),
               # Ignore "additional_data"
               subfolder_recursive_key = c("raw_data",
                                           NA,
                                           "intermediate_data",
                                           "intermediate_data",
                                           "layer1_data",
                                           "layer2_data"),
               path = c("./data/raw_data/",
                        "./data/intermediate_data/",
                        "./data/intermediate_data/0_01_intermediate_data/",
                        "./data/intermediate_data/0_02_intermediate_data/",
                        "./data/layer1_data/",
                        "./data/layer2_data/"))

  source("./src/functions/get_date_local.R")

  data_paths <- data_paths[which(!is.na(data_paths$subfolder_recursive_key)), ]
  data_paths$change_date <- as.Date(NA)

  for (i in seq_along(data_paths$subfolder)) {

    # If the folder exists:
    if (dir.exists(data_paths$path[i])) {

      # Verify whether the folder is empty
      files_i <- list.files(data_paths$path[i])

    if (!identical(files_i, character(0))) {

    data_paths$change_date[i] <- get_date_local(path = data_paths$path[i])
    }
    }
  }

# Determine indices of rows with most recent change_date

recent_ind <-
  which(data_paths$change_date ==
          max(as.Date(data_paths$change_date), na.rm = TRUE))

# Determine most recent path
path_name <- data_paths$path[max(recent_ind)]

cat(paste0("Most recent data were found in the '",
           data_paths$subfolder[max(recent_ind)],
           "' subfolder of the local project folders, with change date ",
           data_paths$change_date[max(recent_ind)],
           ".\n"))
}


  
# Remove extra dataframes from environment ----

  if (isTRUE(getOption("knitr.in.progress"))) {
    envir <- knitr::knit_global()
  } else {
    envir <- globalenv()
  }

  survey_forms_extended <- c(paste0("data_availability_", survey_codes),
                             paste0("coordinates_", survey_codes))

  if (save_to_env == TRUE) {
    
    for (i in seq_along(survey_forms_extended)) {
      
      if (exists(survey_forms_extended[i], envir = envir)) {
        rm(list = survey_forms_extended[i], envir = envir)
      }
    }
  }

# Check if global environment already contains data frames from this list ----

  if (save_to_env == TRUE) {

  # Check if each data frame exists and is of class "data.frame"
  survey_forms_existing <- data.frame(survey_form = NULL,
                                      change_date_google_drive = NULL)

  for (i in seq_along(survey_forms)) {
    if (exists(survey_forms[i]) &&
        is.data.frame(get(survey_forms[i]))) {

      if (!identical(unique(get(survey_forms[i])$change_date_google_drive),
                     NULL)) {
    survey_forms_existing <-
      rbind(survey_forms_existing,
            data.frame(survey_form = survey_forms[i],
                       change_date_google_drive =
                         unique(get(survey_forms[i])$change_date_google_drive)))
      } else {
      survey_forms_existing <-
        rbind(survey_forms_existing,
              data.frame(survey_form = survey_forms[i],
                         change_date_google_drive = NA))
      }

    }
  }

  if (nrow(survey_forms_existing) > 0) {
  survey_forms_existing <- survey_forms_existing %>%
    mutate(change_date_google_drive =
             as.character(as.Date(change_date_google_drive)))
  }

  # If any objects with the same names currently exist (from other
  # Google Drive change dates)

  if (nrow(survey_forms_existing > 0)) {

    # Notify user about survey forms currently in the global environment

  cat(paste0("The following survey forms (with date of the last Google ",
             "Drive update if applicable) currently exist in the Global ",
             "Environment:\n"))
  print(survey_forms_existing)

    # Ask permission to overwrite existing survey forms in global environment

  overwrite_permission <- FALSE

  confirmation <-
    readline(prompt = paste0("Do you grant ",
                             "permission to overwrite the current survey ",
                             "forms in the local folder? (Y/N): "))

  if (tolower(confirmation) == "y") {
    overwrite_permission <- TRUE
  }

  # If no survey forms currently exist in the global environment

  } else {
    overwrite_permission <- TRUE
  }

# Read all the required data ----

  if (overwrite_permission == TRUE) {

  # Get change_date of path
  source("./src/functions/get_date_local.R")
  change_date_gd <- get_date_local(path_name)

  for (i in seq_along(survey_codes)) {

    survey_forms_code <- grep(paste0("^", survey_codes[i]),
                              survey_forms, value = TRUE)

    path_name_survey <- paste0(path_name,
                               survey_codes[i], "/")

    # Read each of the survey forms

    for (j in seq_along(survey_forms_code)) {

    df <- read.csv2(paste0(path_name_survey, survey_forms_code[j], ".csv"))
                    # sep = ";")

    # Replace any "" values by NA
    df[df == ""] <- NA
    
    # Convert numeric columns to numeric
    vec_df <- names(df)[which(names(df) %in% vec_numeric)]
    vec_df <- vec_df[which(!vec_df %in%
                             names(df)[sapply(df, is.numeric)])]

    if (!identical(vec_df, character(0))) {
    df <- df %>%
      mutate(across(all_of(vec_df), as.numeric))
    }
    
      # Add change date of Google Drive version of imported data
      # (to keep track of which version you are working on internally)

    df$change_date_google_drive <- change_date_gd

    # Assign survey form to global environment

    assign_env(survey_forms_code[j], df)

    }

    # Read coordinates data form

    if (paste0("coordinates_", survey_codes[i], ".csv") %in%
          list.files(path_name_survey)) {

      df <- read.csv(paste0(path_name_survey, "coordinates_",
                            survey_codes[i], ".csv"),
                     sep = ";")

      assign_env(paste0("coordinates_", survey_codes[i]),
                 df)

      cat(paste0("Data form '",
                 paste0("coordinates_", survey_codes[i]),
                 "' imported in Global Environment.\n"))
      }

    # Read data availability form

    if (paste0("data_availability_", survey_codes[i], ".csv") %in%
        list.files(path_name_survey)) {

      df <- read.csv(paste0(path_name_survey, "data_availability_",
                            survey_codes[i], ".csv"),
                     sep = ";")

      assign_env(paste0("data_availability_", survey_codes[i]),
                 df)

      cat(paste0("Data form '",
                 paste0("data_availability_", survey_codes[i]),
                 "' imported in Global Environment.\n"))
    }
  }

  # Import some additional data forms which are always useful to have
  # in the global environment

  list_additional <- c("d_country", "d_partner")

  for (i in seq_along(list_additional)) {

  # Check whether this object already exists as a dataframe in the global
  # environment

  if (!(exists(list_additional[i]) &&
        is.data.frame(get(list_additional[i])))) {

    raw_data_paths <- list.files("./data/raw_data/", recursive = TRUE)
    ind <- grep(paste0(list_additional[i], "\\.csv$"),
                raw_data_paths)

    assertthat::assert_that(!identical(ind, integer(0)),
                            msg = paste0("'./data/raw_data/' does not contain ",
                                         "additional dictionaries among ",
                                         "its subfolders."))

    df <- read.csv(paste0("./data/raw_data/",
                          raw_data_paths[ind[1]]),
                   sep = ";")

    assign_env(list_additional[i],
               df)

    cat(paste0("Data form '",
               list_additional[i],
               "' imported in Global Environment.\n"))

  }
  }
}
}
}
