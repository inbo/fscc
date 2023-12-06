
#' Display profiles with soil data per plot_id
#'
#' This function displays profiles of "som" and "pfh" soil survey data
#' for a given (vector of) plot_id(s) in the R console, including values of
#' a specified parameter.
#'
#' @param code_survey A character string indicating the code for the
#' survey (e.g. "s1").
#' @param plot_ids A vector of plot_ids to filter the data. If NULL,
#' all unique plot IDs will be used.
#' @param parameter A character string specifying the parameter of
#' interest. Default is "organic_carbon_total".
#'#'
#' @examples
#' \dontrun{
#' show_profiles(code_survey = "s1", plot_ids = c("1_1"),
#' parameter = "organic_carbon_total")
#' }
#'
#' @export


show_profiles <- function(code_survey,
                          plot_ids = NULL,
                          parameter = "organic_carbon_total") {

  assertthat::assert_that(exists(paste0(code_survey, "_som")))
  assertthat::assert_that(exists(paste0(code_survey, "_pfh")))


  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"))

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  source("./src/functions/expand_unique_survey_vec_adjacent_years.R")

  # Parameters ----

  corresponding_parameters <- data.frame(
  som_parameter = c(
    "extrac_pb", "extrac_zn", "part_size_clay", "part_size_silt", "exch_mn",
    "carbonates", "extrac_al", "exch_mg", "exch_fe", "extrac_na",
    "exch_ca", "ph_cacl2", "n_total", "extrac_k", "exch_al",
    "tot_mg", "exch_k", "extrac_s", "organic_carbon_total", "exch_na",
    "extrac_cr", "extrac_fe", "tot_k", "ph_h2o", "part_size_sand",
    "extrac_cu", "free_h", "extrac_cd", "extrac_hg", "extrac_ca",
    "extrac_mg", "extrac_ni", "extrac_p", "extrac_al", "extrac_mn",
    "extrac_fe", "tot_ca", "p_ox", "exch_acidiy", "tot_na",
    "tot_mn", "tot_fe", "tot_al"),
  pfh_parameter = c(
    NA, NA, "horizon_clay", "horizon_silt", NA,
    "horizon_caco3_total", NA, "horizon_exch_mg", NA, NA,
    "horizon_exch_ca", NA, "horizon_n_total", NA, NA,
    NA, "horizon_exch_k", NA, "horizon_c_organic_total", "horizon_exch_na",
    NA, NA, NA, "horizon_ph", "horizon_sand",
    NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA,
    NA, NA, NA))

  assertthat::assert_that(
    parameter %in% corresponding_parameters$som_parameter ||
      parameter %in% corresponding_parameters$pfh_parameter)

  not_in_pfh <- FALSE

  if (parameter %in% corresponding_parameters$som_parameter) {

    parameter_som <- parameter
    parameter_pfh <-
      corresponding_parameters$pfh_parameter[which(
        corresponding_parameters$som_parameter == parameter)]

    if (is.na(parameter_pfh)) {
      not_in_pfh <- TRUE
    }
  } else {

    parameter_pfh <- parameter
    parameter_som <-
      corresponding_parameters$som_parameter[which(
        corresponding_parameters$pfh_parameter == parameter)]

  }




  # Prepare data ----

  if (is.null(plot_ids)) {
    plot_ids <-
    unique(c((get_env(paste0(code_survey, "_pfh"))[["plot_id"]]),
             (get_env(paste0(code_survey, "_som"))[["plot_id"]])))
  }


  pfh <- get_env(paste0(code_survey, "_pfh")) %>%
    filter(plot_id %in% plot_ids) %>%
    mutate(survey_form = paste0(code_survey, "_pfh")) %>%
    mutate(bulk_density = ifelse(!is.na(.data$horizon_bulk_dens_measure),
                                 .data$horizon_bulk_dens_measure,
                                 .data$horizon_bulk_dens_est)) %>%
    filter(!is.na(.data$layer_number)) %>%
    rename(profile_id = unique_survey_profile) %>%
    rename(depth_top = horizon_limit_up) %>%
    rename(depth_bottom = horizon_limit_low) %>%
    rename(code_layer = horizon_master) %>%
    rename(repetition = profile_pit_id) %>%
    mutate(repetition = as.character(.data$repetition),
           code_layer = as.character(.data$code_layer))

  if (not_in_pfh) {

    pfh <- pfh %>%
      mutate(parameter_col = NA) %>%
      select(survey_form,
             plot_id,
             survey_year,
             unique_survey,
             profile_id,
             repetition,
             code_layer,
             layer_number,
             layer_type,
             depth_top,
             depth_bottom,
             layer_number_ff,
             layer_number_bg,
             # colour_moist_hex,
             bulk_density,
             parameter_col)

    # names(pfh)[names(pfh) == "new_col"] <- parameter_som

  } else {

  pfh <- pfh %>%
    select(survey_form,
           plot_id,
           survey_year,
           unique_survey,
           profile_id,
           repetition,
           code_layer,
           layer_number,
           layer_type,
           depth_top,
           depth_bottom,
           layer_number_ff,
           layer_number_bg,
           # colour_moist_hex,
           bulk_density,
           !!!parameter_pfh)

    # names(pfh)[names(pfh) == parameter_pfh] <- parameter_som
  names(pfh)[names(pfh) == parameter_pfh] <- "parameter_col"

  }

  som <- get_env(paste0(code_survey, "_som")) %>%
    filter(plot_id %in% plot_ids) %>%
    mutate(colour_moist_hex = NA) %>%
    mutate(survey_form = paste0(code_survey, "_som")) %>%
    filter(!is.na(.data$layer_number)) %>%
    rename(profile_id = unique_survey_repetition) %>%
    rename(depth_top = layer_limit_superior) %>%
    rename(depth_bottom = layer_limit_inferior) %>%
    mutate(repetition = as.character(.data$repetition),
           code_layer = as.character(.data$code_layer)) %>%
    select(survey_form,
           plot_id,
           survey_year,
           unique_survey,
           profile_id,
           repetition,
           code_layer,
           layer_number,
           layer_type,
           depth_top,
           depth_bottom,
           layer_number_ff,
           layer_number_bg,
           # colour_moist_hex,
           bulk_density,
           !!!parameter_som)

  names(som)[names(som) == parameter_som] <- "parameter_col"


  # Evaluate for each of the plot surveys ----

  for (i in seq_along(plot_ids)) {

    unique_surveys_som_i <- som %>%
      filter(plot_id == plot_ids[i]) %>%
      distinct(unique_survey) %>%
      pull(unique_survey)

    for (j in seq_along(unique_surveys_som_i)) {

    unique_survey_j <- unique_surveys_som_i[j]

    survey_expanded_j <-
      expand_unique_survey_vec_adjacent_years(unique_survey_j, 3)

    profiles_j <-
      bind_rows(som %>%
                  filter(unique_survey == unique_survey_j),
                pfh %>%
                  filter(unique_survey %in% survey_expanded_j))  %>%
      mutate(prof_name = paste0(.data$survey_form, "_",
                                .data$survey_year, "_",
                                .data$repetition)) %>%
      # Some code_layers (especially in "pfh") are not unique within the profile
      # Add the layer_number for those
      group_by(.data$prof_name) %>%
      mutate(duplicated_code_layer =
               duplicated(code_layer) |
               duplicated(code_layer, fromLast = TRUE)) %>%
      ungroup() %>%
      mutate(code_layer_unique = ifelse(.data$duplicated_code_layer == TRUE,
                                        paste0(.data$code_layer, "_",
                                               .data$layer_number),
                                        .data$code_layer)) %>%
      relocate(code_layer_unique, .after = code_layer) %>%
      select(-duplicated_code_layer)





      ## Prepare to print the data ----

      profiles_j_summary <- profiles_j %>%
        mutate(layer_type_short = case_when(
          layer_type == "forest_floor" ~ "ff",
          layer_type == "mineral" ~ "min",
          layer_type == "peat" ~ "peat",
          TRUE ~ layer_type)) %>%
      mutate(layer_features = ifelse(grepl("pfh", .data$survey_form) &
                                       not_in_pfh,
                                     paste0(code_layer_unique, " (",
                                            layer_type_short, ") [from ",
                                            depth_top, " to ",
                                            depth_bottom, "]",
                                            "     "),
                                     paste0(code_layer_unique, " (",
                                            layer_type_short, ") [from ",
                                            depth_top, " to ",
                                            depth_bottom, "] → ",
                                            parameter_col,
                                            "     "))) %>%
        select(prof_name, layer_features) %>%
        # Create a unique identifier for each group
        group_by(prof_name) %>%
        mutate(row_ind = row_number(),
               row_ind_max_profile = max(.data$row_ind, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(row_ind_max = max(.data$row_ind, na.rm = TRUE),
               row_ind = .data$row_ind +
                 (.data$row_ind_max - .data$row_ind_max_profile)) %>%
        select(-row_ind_max_profile, -row_ind_max) %>%
        # Spread the data for easy printing
        pivot_wider(names_from = prof_name,
                    values_from = layer_features) %>%
        arrange(row_ind) %>%
        select(-row_ind) %>%
        mutate_all(function(x) ifelse(is.na(x), "", x)) %>%
        as.data.frame

      # Print the summary

      cat(paste0(unique_survey_j, " → ",
                 parameter_som, "\n \n"))

      # Print "som" columns
      print(profiles_j_summary[, grep("som", names(profiles_j_summary)),
                               drop = FALSE],
            row.names = FALSE,
            right = FALSE)

      cat(" \n")

      # Print "pfh" columns
      print(profiles_j_summary[, grep("pfh", names(profiles_j_summary)),
                               drop = FALSE],
            row.names = FALSE,
            right = FALSE)

      cat(" \n-------------------------------------------------------\n  \n")


  } # End of evaluation unique surveys

  }



}
