
#' Add derived variables and list related inconsistencies
#'
#' This function adds data for the following new variables:
#' - Layer thickness
#' - Bulk density based on the layer weight
#' - Sum of texture particle size fractions
#' - C-to-N ratio
#' - Sum of base cations (exchangeable Ca, Mg, K, Na)
#' - Sum of acid cations (exchangeable Al, Fe, Mn, free H+)
#'
#' @param survey_form Character string - Name of the survey form (lower case and
#' separated by '_') to be evaluated
#' @param save_to_env Logical which indicates whether the output dataframes
#' can be saved to the environment and override any existing objects
#' with the same name. Default is FALSE.
#'
#' @details
#' This function only applies to "som" and "pfh" survey forms.
#'
#' Outputs - This function generates and returns:
#' - an inconsistency report ("list_derived_inconsistencies")
#' - the original data form but with the new variables
#'
#' Which kind of inconsistencies are identified in this function?
#' - FSCC_20: Is the reported organic_layer_weight possible, considering the
#'   possible range of the derived bulk density (0-2650 kg m-3)?
#' - FSCC_16: Is the sum of the reported texture particle size fractions 100 %
#'   (between 97 and 103 %)?
#' - FSCC_23: Is the ratio betwene the reported total organic carbon and total
#'   nitrogen in a plausible range (1-100)?
#' - FSCC_44: Does the sum of the reported exchangeable base cations (Ca, Mg, K,
#'   Na) not exceed the reported cation exchange capacity?
#' - FSCC_45: Does the sum of the reported exchangeable acid cations (Al, Fe,
#'   Mn) and free H+ approximate the reported exchangeable acidity?
#'
#' How are the survey forms exactly updated in this function?
#' - "som" survey forms: Add columns with the following processed data:
#'   * "layer_thickness": difference between "layer_limit_superior" and
#'     "layer_limit_inferior" (where required data are available)
#'   * "bulk_density_layer_weight": "organic_layer_weight" / "layer_thickness"
#'     (where required data are available)
#'   * "sum_texture": "part_size_clay" + "part_size_silt" + "part_size_sand"
#'     (where required data are available). Data below the LOQ (i.e. -1) are
#'     replaced by half of the LOQ (i.e. by 0.5).
#'   * "c_to_n_ratio": "organic_carbon_total" / "n_total" (where required data
#'     are available and not lower than the LOQ, i.e. not equal to -1)
#'   * "sum_base_cations": "exch_ca" + "exch_mg" + "exch_k" + "exch_na" (where
#'     required data are available and where at least two of the parameters are
#'     not below the LOQ, i.e. not equal to -1). Data below the LOQ (i.e. -1)
#'     are replaced by half of the LOQ (i.e. by 0.015).
#'   * "sum_acid_cations": "exch_al" + "exch_fe" + "exch_mn" + "free_h" (where
#'     required data are available and where at least two of the parameters are
#'     not below the LOQ, i.e. not equal to -1). Data below the LOQ (i.e. -1)
#'     are replaced by half of the LOQ (i.e. by 0.01 for "exch_al", "exch_fe",
#'     "exch_mn" and by 0.05 for "free_h")
#' - "pfh" survey forms: Add columns with the following processed data:
#'   * "sum_texture": "horizon_clay" + "horizon_silt" + "horizon_sand" (where
#'     required data are available). Data below the LOQ (i.e. -1) are replaced
#'     by half of the LOQ (i.e. by 0.5).
#'   * "c_to_n_ratio": "horizon_c_organic_total" / "horizon_n_total" (where
#'     required data are available and not lower than the LOQ, i.e. not equal
#'     to -1)
#'   * "sum_base_cations": "horizon_exch_ca" + "horizon_exch_mg" +
#'     "horizon_exch_k" + "horizon_exch_na" (where required data are available
#'     and where at least two of the parameters are not below the LOQ, i.e. not
#'     equal to -1). Data below the LOQ (i.e. -1) are replaced by half of the
#'     LOQ (i.e. by 0.015).
#'
#'  WARNING - This function may not be optimally efficient and may ideally
#'  require refactoring for better performance.
#'
#' @examples
#' get_derived_variable_inconsistencies("s1_som")

get_derived_variable_inconsistencies <- function(survey_form,
                                                 data_frame = NULL,
                                                 save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  stopifnot(require("soiltexture"))

  cat(paste0(" \nSolve inconsistencies in derived variables in '",
             survey_form, "'\n"))



  # Specify date on which 'layer 0' data were downloaded ----
  # from ICP Forests website

  source("./src/functions/get_date_local.R")
  download_date <- get_date_local(path = "./data/raw_data/",
                                  save_to_env = TRUE,
                                  collapsed = TRUE)
  download_date_pir <- as.Date(parsedate::parse_iso_8601(download_date))

  # Import the inconsistency catalogue ----

  assertthat::assert_that(
    file.exists("./data/additional_data/inconsistency_catalogue.csv"),
    msg = paste0("There is no 'inconsistency catalogue' in the",
                 " '.data/additional_data/' folder."))

  inconsistency_catalogue <-
    read.csv("./data/additional_data/inconsistency_catalogue.csv", sep = ";")



  # Retrieve the survey_form data

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }




  # "som" survey forms ----

  if (unlist(strsplit(survey_form, "_"))[2] == "som") {

    # Add new variables ----

    df <- df %>%
      rowwise() %>%
      mutate(
        # Layer thickness
        layer_thickness =
               ifelse(!is.na(.data$layer_limit_superior) &
                        !is.na(.data$layer_limit_inferior) &
                        (.data$layer_limit_superior !=
                           .data$layer_limit_inferior),
                      abs(.data$layer_limit_superior -
                            .data$layer_limit_inferior),
                      NA_real_),
        # C to N ratio
        # This should be in a plausible range (1 to 100)
        c_to_n_ratio =
          ifelse(!is.na(.data$organic_carbon_total) &
                   !is.na(.data$n_total) &
                   !is.na(.data$organic_carbon_total_loq) &
                   !is.na(.data$n_total_loq) &
                   (.data$organic_carbon_total >=
                      .data$organic_carbon_total_loq) &
                   (.data$n_total >=
                      .data$n_total_loq),
                 round(.data$organic_carbon_total / .data$n_total, 2),
                 NA_real_),
        # Sum base cations
        # This should be smaller or equal to CEC (exch_cec)
        sum_base_cations =
          ifelse(!is.na(.data$exch_ca) &
                   !is.na(.data$exch_mg) &
                   !is.na(.data$exch_k) &
                   !is.na(.data$exch_na) &
                   !is.na(.data$exch_ca_loq) &
                   !is.na(.data$exch_mg_loq) &
                   !is.na(.data$exch_k_loq) &
                   !is.na(.data$exch_na_loq) &
                   # If only one value is above LOQ, but this value
                   # is significantly higher,
                   # it could still be justifiable to calculate a sum???
                   rowSums(cbind(.data$exch_ca, .data$exch_mg,
                                 .data$exch_k, .data$exch_na) >
                             cbind(.data$exch_ca_loq, .data$exch_mg_loq,
                                   .data$exch_k_loq,
                                   .data$exch_na_loq)) >= 2,
                 round(.data$exch_ca + .data$exch_mg +
                         .data$exch_k + .data$exch_na, 2),
                 NA_real_),
        # Sum acid cations
        # This should approximate the exchangeable acidity (exch_acidiy)
        sum_acid_cations =
          ifelse(!is.na(.data$exch_al) &
                   !is.na(.data$exch_fe) &
                   !is.na(.data$exch_mn) &
                   !is.na(.data$free_h) &
                   !is.na(.data$exch_al_loq) &
                   !is.na(.data$exch_fe_loq) &
                   !is.na(.data$exch_mn_loq) &
                   !is.na(.data$free_h_loq) &
                   # If only one value is above LOQ, but this value
                   # is significantly higher,
                   # it could still be justifiable to calculate a sum???
                   rowSums(cbind(.data$exch_al, .data$exch_fe,
                                 .data$exch_mn, .data$free_h) >
                             cbind(.data$exch_al_loq, .data$exch_fe_loq,
                                   .data$exch_mn_loq,
                                   .data$free_h_loq)) >= 2,
                 round(.data$exch_al + .data$exch_fe +
                         .data$exch_mn + .data$free_h, 2),
                 NA_real_),
        # Bulk density based on layer weight
        # to compare with plausibility range of bulk density
        bulk_density_layer_weight =
          ifelse(!is.na(.data$organic_layer_weight) &
                   (.data$organic_layer_weight != -1) &
                   !is.na(.data$layer_thickness) &
                   (.data$layer_type %in% c("forest_floor", "peat")),
                 # kg m-3
                 round(.data$organic_layer_weight /
                         (.data$layer_thickness * 1e-2), 2),
                 NA_real_),
        # Organic layer weight based on bulk density
        organic_layer_weight_bd =
          ifelse(!is.na(.data$bulk_density) &
                   !is.na(.data$layer_thickness) &
                   (.data$layer_type %in% c("forest_floor", "peat")),
                 # kg m-2
                 round(.data$bulk_density * (.data$layer_thickness * 1e-2), 3),
                 NA_real_),
        # Sum texture
        # This should be between 97 and 103 %
        sum_texture =
          ifelse(!is.na(.data$part_size_clay) &
                   !is.na(.data$part_size_silt) &
                   !is.na(.data$part_size_clay),
                 .data$part_size_clay + .data$part_size_silt +
                   .data$part_size_sand,
                 NA_real_))

  }


  # "pfh" survey forms ----

  if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

    # Import average coarse fragments vol% per volumetric class

    d_soil_coarse_fragments <-
      read.csv2("./data/additional_data/d_soil_coarse_fragments.csv") %>%
      select(code, coarse_fragment_vol_avg)


    # Add new variables ----

    df <- df %>%
      rowwise() %>%
      mutate(
        # Layer thickness
        layer_thickness =
          ifelse(!is.na(.data$horizon_limit_up) &
                   !is.na(.data$horizon_limit_low) &
                   (.data$horizon_limit_up !=
                      .data$horizon_limit_low),
                 abs(.data$horizon_limit_up -
                       .data$horizon_limit_low),
                 NA_real_),
        # C to N ratio
        # This should be in a plausible range (1 to 100)
        c_to_n_ratio =
          ifelse(!is.na(.data$horizon_c_organic_total) &
                   !is.na(.data$horizon_n_total) &
                   !is.na(.data$horizon_c_organic_total_loq) &
                   !is.na(.data$horizon_n_total_loq) &
                   (.data$horizon_c_organic_total >=
                      .data$horizon_c_organic_total_loq) &
                   (.data$horizon_n_total >=
                      .data$horizon_n_total_loq),
                 round(.data$horizon_c_organic_total / .data$horizon_n_total,
                       2),
                 NA_real_),
        # Sum base cations
        # This should be smaller or equal to CEC (exch_cec)
        sum_base_cations =
          ifelse(!is.na(.data$horizon_exch_ca) &
                   !is.na(.data$horizon_exch_mg) &
                   !is.na(.data$horizon_exch_k) &
                   !is.na(.data$horizon_exch_na) &
                   !is.na(.data$horizon_exch_ca_loq) &
                   !is.na(.data$horizon_exch_mg_loq) &
                   !is.na(.data$horizon_exch_k_loq) &
                   !is.na(.data$horizon_exch_na_loq) &
                   # If only one value is above LOQ, but this value
                   # is significantly higher,
                   # it could still be justifiable to calculate a sum???
                   rowSums(cbind(.data$horizon_exch_ca,
                                 .data$horizon_exch_mg,
                                 .data$horizon_exch_k,
                                 .data$horizon_exch_na) >
                             cbind(.data$horizon_exch_ca_loq,
                                   .data$horizon_exch_mg_loq,
                                   .data$horizon_exch_k_loq,
                                   .data$horizon_exch_na_loq)) >= 2,
                 round(.data$horizon_exch_ca + .data$horizon_exch_mg +
                         .data$horizon_exch_k + .data$horizon_exch_na, 2),
                 NA_real_),
        # Bulk density merged
        bulk_density_harm =
          coalesce(.data$horizon_bulk_dens_measure,
                   .data$horizon_bulk_dens_est),
        # Organic layer weight based on bulk density
        organic_layer_weight_bd =
          ifelse(!is.na(.data$bulk_density_harm) &
                   !is.na(.data$layer_thickness) &
                   (.data$layer_type %in% c("forest_floor", "peat")),
                 # kg m-2
                 round(.data$bulk_density_harm *
                         (.data$layer_thickness * 1e-2), 3),
                 NA_real_),
        # Sum texture
        sum_texture =
          ifelse(!is.na(.data$horizon_clay) &
                   !is.na(.data$horizon_silt) &
                   !is.na(.data$horizon_sand),
                 .data$horizon_clay + .data$horizon_silt +
                   .data$horizon_sand,
                 NA_real_),
        # Coarse fragments: vol% converted from weight%
        # ---
        # Convert weight percentages to volumetric percentages:
        # Imagine: 1 m³ of fine earth contains
        # e.g. 1300 kg fine earth (bulk density).
        # Then, imagine the weight percentage of coarse fragments
        # from that soil is 11 %.
        # That means that there is 1300 kg * 11/89 = 160.7 kg of coarse
        # fragments for 1 m³ of fine earth in this soil.
        # Imagine the coarse fragments have a particle density of 2650 kg
        # per m³.
        # Then, we can calculate that this 160.7 kg of coarse fragments
        # occupies 160.7/2650 = 0.061 m³.
        # As such, the vol % of coarse fragments will be 0.061 / (1 + 0.061)
        coarse_fragment_aid =
          ifelse(!is.na(bulk_density_harm) & !is.na(horizon_coarse_weight),
                 (.data$bulk_density_harm *
                    (.data$horizon_coarse_weight /
                       (100 - .data$horizon_coarse_weight))) / 2650,
                 NA),
        coarse_fragment_vol_converted =
          ifelse(!is.na(.data$coarse_fragment_aid),
                 round(as.numeric((.data$coarse_fragment_aid /
                               (1 + .data$coarse_fragment_aid)) * 100), 3),
                 NA)) %>%
      # Coarse fragments: average volume of class
      # Convert volumetric coarse fragment codes to actual average vol %
      left_join(d_soil_coarse_fragments,
                by = join_by(code_horizon_coarse_vol == code)) %>%
      select(-bulk_density_harm,
             -coarse_fragment_aid)

    }






  # Inconsistencies ----

  # The intention is to create a list_layer_inconsistencies of this format:
  list_derived_inconsistencies <-
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

  # FSCC_20: Is the reported organic_layer_weight possible, ----
  # i.e. is the derived bulk density within a possible range (0-2650 kg m-3)?

  if ("bulk_density_layer_weight" %in% names(df)) {

    range_min <- 0
    range_max <- 2650

    vec_inconsistency <- which(!is.na(df$bulk_density_layer_weight) &
                                 ((df$layer_type == "mineral" &
                                   ((df$bulk_density_layer_weight < range_min) |
                                 (df$bulk_density_layer_weight > range_max))) |
                                   # organic
                             (df$layer_type != "mineral" &
                                ((df$bulk_density_layer_weight < range_min) |
                                   (df$bulk_density_layer_weight > 1400)))
                                   ))

    if (!identical(vec_inconsistency, integer(0))) {

      if (unlist(strsplit(survey_form, "_"))[2] == "som") {

        if ("code_layer_orig" %in% names(df)) {
          ind_layer_horizon <- as.character(df$code_layer_orig)
        } else {
          ind_layer_horizon <- as.character(df$code_layer)}
          ind_repetition_profile_pit_id <- df$repetition
        }

      if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

        ind_layer_horizon <- df$horizon_master
        ind_repetition_profile_pit_id <- df$profile_pit_id
        }

      rule_id <- "FSCC_20"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      for (i in vec_inconsistency) {

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = as.factor(rep("organic_layer_weight",
                                             length(i))),
                   parameter_unit = rep("kg m-2", length(i)),
                   parameter_value = df$organic_layer_weight[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(TRUE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

    list_derived_inconsistencies <- rbind(
      list_derived_inconsistencies,
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
                 parameter = as.factor(rep("layer_limit_superior", length(i))),
                 parameter_unit = rep("cm", length(i)),
                 parameter_value = df$layer_limit_superior_orig[i],
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                 change_date = df$change_date[i],
                 download_date = rep(download_date_pir, length(i))))

    list_derived_inconsistencies <- rbind(
      list_derived_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form,length(i))),
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
                 parameter = as.factor(rep("layer_limit_inferior", length(i))),
                 parameter_unit = rep("cm", length(i)),
                 parameter_value = df$layer_limit_inferior_orig[i],
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                 change_date = df$change_date[i],
                 download_date = rep(download_date_pir, length(i))))

      }

      df$bulk_density_layer_weight[vec_inconsistency] <- NA
    }
    }




  # FSCC_16: Is the sum of the texture particle size fractions 100 %? ----

  if ("sum_texture" %in% names(df)) {

    range_min <- 97
    range_max <- 103

    vec_inconsistency <- which(!is.na(df$sum_texture) &
                                 ((df$sum_texture < range_min) |
                                 (df$sum_texture > range_max)))

    if (!identical(vec_inconsistency, integer(0))) {

      if (unlist(strsplit(survey_form, "_"))[2] == "som") {

        if ("code_layer_orig" %in% names(df)) {
          ind_layer_horizon <- as.character(df$code_layer_orig)
        } else {
            ind_layer_horizon <- as.character(df$code_layer)
        }

        ind_repetition_profile_pit_id <- df$repetition
        ind_parameter_clay <- "part_size_clay"
        ind_parameter_silt <- "part_size_silt"
        ind_parameter_sand <- "part_size_sand"
        ind_data_clay <- df$part_size_clay
        ind_data_silt <- df$part_size_silt
        ind_data_sand <- df$part_size_sand}

      if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

        ind_layer_horizon <- df$horizon_master
        ind_repetition_profile_pit_id <- df$profile_pit_id
        ind_parameter_clay <- "horizon_clay"
        ind_parameter_silt <- "horizon_silt"
        ind_parameter_sand <- "horizon_sand"
        ind_data_clay <- df$horizon_clay
        ind_data_silt <- df$horizon_silt
        ind_data_sand <- df$horizon_sand
        }

      rule_id <- "FSCC_16"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      for (i in vec_inconsistency) {

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter_value = ind_data_clay[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(TRUE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter_value = ind_data_silt[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter_value = ind_data_sand[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))
      }
    }

    # Solve problems with texture ----

    # Manual correction: in so_som 64_5, silt and sand seem to be switched
    # (based on both texture class and ESD texture class)

    if (survey_form == "so_som") {

      df <- df %>%
        mutate(
          part_size_silt2 = part_size_silt,
          part_size_silt = ifelse(
            unique_survey == "64_2004_5",
            part_size_sand,
            part_size_silt),
          part_size_sand = ifelse(
            unique_survey == "64_2004_5",
            part_size_silt2,
            part_size_sand)) %>%
        select(-part_size_silt2)

    }

    # If sum textures is between 85 and 115, it may be justifiable to
    # normalise the particle size fractions to 100
    # Any potential errors in assuming they are reported proportionally
    # would have minimal impact.
    # Else, replace all of them by NAs

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {

      df <- df %>%
        rowwise() %>%
        mutate(
          # Check conditions and normalise if needed
          part_size_clay_source = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            paste0(part_size_clay_source, " (normalised)"),
            part_size_clay_source),
          part_size_clay = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            part_size_clay / sum_texture * 100,
            part_size_clay),
          part_size_silt_source = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            paste0(part_size_silt_source, " (normalised)"),
            part_size_silt_source),
          part_size_silt = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            part_size_silt / sum_texture * 100,
            part_size_silt),
          part_size_sand_source = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            paste0(part_size_sand_source, " (normalised)"),
            part_size_sand_source),
          part_size_sand = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            part_size_sand / sum_texture * 100,
            part_size_sand)) %>%
        mutate(
          # Check conditions and replace by NA if needed
          part_size_clay_source = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_character_,
            part_size_clay_source),
          part_size_clay = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_real_,
            part_size_clay),
          part_size_silt_source = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_character_,
            part_size_silt_source),
          part_size_silt = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_real_,
            part_size_silt),
          part_size_sand_source = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_character_,
            part_size_sand_source),
          part_size_sand = ifelse(
            !is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_real_,
            part_size_sand)) %>%
        ungroup()

    } else if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

      df <- df %>%
        rowwise() %>%
        mutate(
          # Check conditions and normalise if needed
          horizon_clay_source = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            paste0(horizon_clay_source, " (normalised)"),
            horizon_clay_source),
          horizon_clay = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            horizon_clay / sum_texture * 100,
            horizon_clay),
          horizon_silt_source = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            paste0(horizon_silt_source, " (normalised)"),
            horizon_silt_source),
          horizon_silt = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            horizon_silt / sum_texture * 100,
            horizon_silt),
          horizon_sand_source = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            paste0(horizon_sand_source, " (normalised)"),
            horizon_sand_source),
          horizon_sand = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              ((sum_texture > 100 & sum_texture <= 115) |
                 (sum_texture < 100 & sum_texture >= 85)),
            horizon_sand / sum_texture * 100,
            horizon_sand)) %>%
        mutate(
          # Check conditions and replace by NA if needed
          horizon_clay_source = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_character_,
            horizon_clay_source),
          horizon_clay = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_real_,
            horizon_clay),
          horizon_silt_source = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_character_,
            horizon_silt_source),
          horizon_silt = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_real_,
            horizon_silt),
          horizon_sand_source = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_character_,
            horizon_sand_source),
          horizon_sand = ifelse(
            !is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              (sum_texture < 85 |
                 sum_texture > 115),
            NA_real_,
            horizon_sand)) %>%
        ungroup()

    }


    # If two out of three particle size fractions are known,
    # assume that the third one can be calculated by the difference with 100

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {

      df <- df %>%
        rowwise() %>%
        mutate(
          part_size_clay_source = ifelse(
            is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              (part_size_silt + part_size_sand < 100),
            "Estimated as remaining fraction",
            part_size_clay_source),
          part_size_clay = ifelse(
            is.na(part_size_clay) & !is.na(part_size_silt) &
              !is.na(part_size_sand) &
              (part_size_silt + part_size_sand < 100),
            100 - (part_size_silt + part_size_sand),
            part_size_clay),
          part_size_silt_source = ifelse(
            is.na(part_size_silt) & !is.na(part_size_clay) &
              !is.na(part_size_sand) &
              (part_size_clay + part_size_sand < 100),
            "Estimated as remaining fraction",
            part_size_silt_source),
          part_size_silt = ifelse(
            is.na(part_size_silt) & !is.na(part_size_clay) &
              !is.na(part_size_sand) &
              (part_size_clay + part_size_sand < 100),
            100 - (part_size_clay + part_size_sand),
            part_size_silt),
          part_size_sand_source = ifelse(
            is.na(part_size_sand) & !is.na(part_size_silt) &
              !is.na(part_size_clay) &
              (part_size_silt + part_size_clay < 100),
            "Estimated as remaining fraction",
            part_size_sand_source),
          part_size_sand = ifelse(
            is.na(part_size_sand) & !is.na(part_size_silt) &
              !is.na(part_size_clay) &
              (part_size_silt + part_size_clay < 100),
            100 - (part_size_silt + part_size_clay),
            part_size_sand)) %>%
        ungroup()

    } else if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

      df <- df %>%
        rowwise() %>%
        mutate(
          horizon_clay_source = ifelse(
            is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              (horizon_silt + horizon_sand < 100),
            "Estimated as remaining fraction",
            horizon_clay_source),
          horizon_clay = ifelse(
            is.na(horizon_clay) & !is.na(horizon_silt) &
              !is.na(horizon_sand) &
              (horizon_silt + horizon_sand < 100),
            100 - (horizon_silt + horizon_sand),
            horizon_clay),
          horizon_silt_source = ifelse(
            is.na(horizon_silt) & !is.na(horizon_clay) &
              !is.na(horizon_sand) &
              (horizon_clay + horizon_sand < 100),
            "Estimated as remaining fraction",
            horizon_silt_source),
          horizon_silt = ifelse(
            is.na(horizon_silt) & !is.na(horizon_clay) &
              !is.na(horizon_sand) &
              (horizon_clay + horizon_sand < 100),
            100 - (horizon_clay + horizon_sand),
            horizon_silt),
          horizon_sand_source = ifelse(
            is.na(horizon_sand) & !is.na(horizon_silt) &
              !is.na(horizon_clay) &
              (horizon_silt + horizon_clay < 100),
            "Estimated as remaining fraction",
            horizon_sand_source),
          horizon_sand = ifelse(
            is.na(horizon_sand) & !is.na(horizon_silt) &
              !is.na(horizon_clay) &
              (horizon_silt + horizon_clay < 100),
            100 - (horizon_silt + horizon_clay),
            horizon_sand)) %>%
        ungroup()
    }

    # If any particle size fraction is still unknown, replace everything by NA

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {

      df <- df %>%
        rowwise() %>%
        mutate(
          part_size_clay_source = ifelse(
            is.na(part_size_silt) | is.na(part_size_sand),
            NA_character_,
            part_size_clay_source),
          part_size_clay = ifelse(
            is.na(part_size_silt) | is.na(part_size_sand),
            NA_real_,
            part_size_clay),
          part_size_silt_source = ifelse(
            is.na(part_size_clay) | is.na(part_size_sand),
            NA_character_,
            part_size_silt_source),
          part_size_silt = ifelse(
            is.na(part_size_clay) | is.na(part_size_sand),
            NA_real_,
            part_size_silt),
          part_size_sand_source = ifelse(
            is.na(part_size_silt) | is.na(part_size_clay),
            NA_character_,
            part_size_sand_source),
          part_size_sand = ifelse(
            is.na(part_size_silt) | is.na(part_size_clay),
            NA_real_,
            part_size_sand)) %>%
        ungroup()

    } else if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

      df <- df %>%
        rowwise() %>%
        mutate(
          horizon_clay_source = ifelse(
            is.na(horizon_silt) | is.na(horizon_sand),
            NA_character_,
            horizon_clay_source),
          horizon_clay = ifelse(
            is.na(horizon_silt) | is.na(horizon_sand),
            NA_real_,
            horizon_clay),
          horizon_silt_source = ifelse(
            is.na(horizon_clay) | is.na(horizon_sand),
            NA_character_,
            horizon_silt_source),
          horizon_silt = ifelse(
            is.na(horizon_clay) | is.na(horizon_sand),
            NA_real_,
            horizon_silt),
          horizon_sand_source = ifelse(
            is.na(horizon_silt) | is.na(horizon_clay),
            NA_character_,
            horizon_sand_source),
          horizon_sand = ifelse(
            is.na(horizon_silt) | is.na(horizon_clay),
            NA_real_,
            horizon_sand)) %>%
        ungroup()

    }



    # Wallonia confirmed that they accidentally reported their particle
    # size fractions with 50 µm instead of 63 µm as boundary between
    # silt and sand.

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {

      ind_wal <- which(df$partner_code == 202 &
                         df$survey_year <= 2022 &
                         !is.na(df$part_size_sand))

      if (!identical(ind_wal, integer(0))) {

        for (i in ind_wal) {

          fractions_i <-
            TT.text.transf(tri.data = df[i, ] %>%
                             select(part_size_clay,
                                    part_size_silt,
                                    part_size_sand),
                           # Original limits
                           base.css.ps.lim = c(0, 2, 50, 2000),
                           # Target limits (sand fraction should increase)
                           dat.css.ps.lim = c(0, 2, 63, 2000),
                           css.names = c("part_size_clay",
                                         "part_size_silt",
                                         "part_size_sand"))

          # Clay remains the same
          df$part_size_silt[i] <- fractions_i$part_size_silt
          df$part_size_sand[i] <- fractions_i$part_size_sand
          df$part_size_silt_source[i] <-
            paste0(df$part_size_silt_source[i],
                   " (corrected to 63 µm silt-sand)")
          df$part_size_sand_source[i] <-
            paste0(df$part_size_sand_source[i],
                   " (corrected to 63 µm silt-sand)")

        }
      }

      df <- df %>%
        mutate(
          part_size_clay = round(part_size_clay, 1),
          part_size_silt = round(part_size_silt, 1),
          part_size_sand = round(part_size_sand, 1))




    } else if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

      ind_wal <- which(df$partner_code == 202 &
                         df$survey_year <= 2022 &
                         !is.na(df$horizon_sand))

      if (!identical(ind_wal, integer(0))) {

        for (i in ind_wal) {

          fractions_i <-
            TT.text.transf(tri.data = df[i, ] %>%
                             select(horizon_clay,
                                    horizon_silt,
                                    horizon_sand),
                           # Original limits
                           base.css.ps.lim = c(0, 2, 50, 2000),
                           # Target limits (sand fraction should increase)
                           dat.css.ps.lim = c(0, 2, 63, 2000),
                           css.names = c("horizon_clay",
                                         "horizon_silt",
                                         "horizon_sand"))

          # Clay remains the same
          df$horizon_silt[i] <- fractions_i$horizon_silt
          df$horizon_sand[i] <- fractions_i$horizon_sand
          df$horizon_silt_source[i] <-
            paste0(df$horizon_silt_source[i],
                   " (corrected to 63 µm silt-sand)")
          df$horizon_sand_source[i] <-
            paste0(df$horizon_sand_source[i],
                   " (corrected to 63 µm silt-sand)")


        }

      }

      df <- df %>%
        mutate(
          horizon_clay = round(horizon_clay, 1),
          horizon_silt = round(horizon_silt, 1),
          horizon_sand = round(horizon_sand, 1))

    }

    } # End of "sum_texture" in names(df)




  # FSCC_23: Is the C/N ratio plausible? ----

  if ("c_to_n_ratio" %in% names(df)) {

    # Somewhat arbitrary choice:

    range_min <- 1
    range_max <- 100

    vec_inconsistency <- which(!is.na(df$c_to_n_ratio) &
                                 ((df$c_to_n_ratio < range_min) |
                                    (df$c_to_n_ratio > range_max)))

    if (!identical(vec_inconsistency, integer(0))) {

      if (unlist(strsplit(survey_form, "_"))[2] == "som") {

        if ("code_layer_orig" %in% names(df)) {
            ind_layer_horizon <- as.character(df$code_layer_orig)
          } else {
            ind_layer_horizon <- as.character(df$code_layer)
          }

        ind_repetition_profile_pit_id <- df$repetition
        ind_parameter_c <- "organic_carbon_total"
        ind_parameter_n <- "n_total"
        ind_data_c <- df$organic_carbon_total
        ind_data_n <- df$n_total}

      if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

        ind_layer_horizon <- df$horizon_master
        ind_repetition_profile_pit_id <- df$profile_pit_id
        ind_parameter_c <- "horizon_c_organic_total"
        ind_parameter_n <- "horizon_n_total"
        ind_data_c <- df$horizon_c_organic_total
        ind_data_n <- df$horizon_n_total
        }

      rule_id <- "FSCC_23"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      for (i in vec_inconsistency) {

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = ind_parameter_c,
                   parameter_unit = rep("g kg-1", length(i)),
                   parameter_value = ind_data_c[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(TRUE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = ind_parameter_n,
                   parameter_unit = rep("g kg-1", length(i)),
                   parameter_value = ind_data_n[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      }
    }
    }




  # FSCC_44: Is the sum of the base cations smaller or equal to CEC ----
  # (exch_cec)

  if ("sum_base_cations" %in% names(df)) {

    cec <- NULL


    if (unlist(strsplit(survey_form, "_"))[2] == "som" &&
        ("exch_cec" %in% names(df))) {
      cec <- df$exch_cec
    }

    if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {
      cec <- df$horizon_cec
    }

    if (!is.null(cec)) {

    vec_inconsistency <- which(!is.na(df$sum_base_cations) &
                               !is.na(cec))
    range_max <- cec

    # Plot to have an idea of how common this inconsistency is:
    # plot(cec[vec_inconsistency], df$sum_base_cations[vec_inconsistency])

    if (!identical(vec_inconsistency, integer(0))) {

    vec_inconsistency <-
      vec_inconsistency[which((df$sum_base_cations[vec_inconsistency] >
                                 range_max[vec_inconsistency]) &
            range_max[vec_inconsistency] != -1)]
    }

    if (!identical(vec_inconsistency, integer(0))) {

      if (unlist(strsplit(survey_form, "_"))[2] == "som") {
      # Ca, Mg, K, Na

        if ("code_layer_orig" %in% names(df)) {
            ind_layer_horizon <- as.character(df$code_layer_orig)
          } else {
            ind_layer_horizon <- as.character(df$code_layer)
          }

        ind_repetition_profile_pit_id <- df$repetition
        ind_parameter_ca <- "exch_ca"
        ind_parameter_mg <- "exch_mg"
        ind_parameter_k <- "exch_k"
        ind_parameter_na <- "exch_na"
        ind_parameter_cec <- "exch_cec"
        ind_data_ca <- df$exch_ca
        ind_data_mg <- df$exch_mg
        ind_data_k <- df$exch_k
        ind_data_na <- df$exch_na
        ind_data_cec <- df$exch_cec}

      if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

        ind_layer_horizon <- df$horizon_master
        ind_repetition_profile_pit_id <- df$profile_pit_id
        ind_parameter_ca <- "horizon_exch_ca"
        ind_parameter_mg <- "horizon_exch_mg"
        ind_parameter_k <- "horizon_exch_k"
        ind_parameter_na <- "horizon_exch_na"
        ind_parameter_cec <- "horizon_cec"
        ind_data_ca <- df$horizon_exch_ca
        ind_data_mg <- df$horizon_exch_mg
        ind_data_k <- df$horizon_exch_k
        ind_data_na <- df$horizon_exch_na
        ind_data_cec <- df$horizon_cec}

      rule_id <- "FSCC_44"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      for (i in vec_inconsistency) {

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = ind_parameter_ca,
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = ind_data_ca[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(TRUE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = ind_parameter_mg,
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = ind_data_mg[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = ind_parameter_k,
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = ind_data_k[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = ind_parameter_na,
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = ind_data_na[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = ind_parameter_cec,
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = ind_data_cec[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))


      }
    }
  }
    }





  # FSCC_45: Does the sum of the acid cations approximate the exchangeable ----
  # acidity (exch_acidiy)?

  if ("sum_acid_cations" %in% names(df)) {

    vec_inconsistency <- which(!is.na(df$sum_acid_cations) &
                                 !is.na(df$exch_acidiy) &
                                 (df$exch_acidiy != -1))

    # "Approximate" is not well defined, and we had a look at the plot
    # to see which values could be considered close enough.
    # This rule is unfortunately quite arbitrary.

    # The plot was made using the following code:

        # plot(df$exch_acidiy[vec_inconsistency],
        #      df$sum_acid_cations[vec_inconsistency])
        # fit <- lm(df$exch_acidiy[vec_inconsistency] ~
        #             df$sum_acid_cations[vec_inconsistency])
        # abline(a = coef(fit)[1], b = coef(fit)[2], col = "red")

        # fit_up <- lm((2 + 1.3 * df$exch_acidiy[vec_inconsistency]) ~
        #                df$sum_acid_cations[vec_inconsistency])
        # abline(a = coef(fit_up)[1], b = coef(fit_up)[2], col = "blue")

        # fit_low <- lm((-2 + 0.7 * df$exch_acidiy[vec_inconsistency]) ~
        #                 df$sum_acid_cations[vec_inconsistency])
        # abline(a = coef(fit_low)[1], b = coef(fit_low)[2], col = "green")

        # abline(a = 0, b = 1, col = "red")
        # abline(a = 2, b = 1.3, col = "blue")
        # abline(a = -2, b = 0.7, col = "green")

    # Based on plot:
    range_min <- (-2) + df$exch_acidiy[vec_inconsistency] * 0.7
    range_max <- 2 + df$exch_acidiy[vec_inconsistency] * 1.3

    if (!identical(vec_inconsistency, integer(0))) {
      vec_inconsistency <- vec_inconsistency[which(
        df$sum_acid_cations[vec_inconsistency] < range_min |
        df$sum_acid_cations[vec_inconsistency] > range_max)]
      }

    if (!identical(vec_inconsistency, integer(0))) {

      if (unlist(strsplit(survey_form, "_"))[2] == "som") {
      # Al, Fe, Mn, free H+

        if ("code_layer_orig" %in% names(df)) {
            ind_layer_horizon <- as.character(df$code_layer_orig)
        } else {
            ind_layer_horizon <- as.character(df$code_layer)}
            ind_repetition_profile_pit_id <- df$repetition
        }

      if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {
        ind_layer_horizon <- df$horizon_master
        ind_repetition_profile_pit_id <- df$profile_pit_id
        }

      rule_id <- "FSCC_45"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]


      for (i in vec_inconsistency) {

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = rep("exch_al", length(i)),
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = df$exch_al[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(TRUE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = rep("exch_fe", length(i)),
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = df$exch_fe[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = rep("exch_mn", length(i)),
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = df$exch_mn[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = rep("free_h", length(i)),
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = df$free_h[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      list_derived_inconsistencies <- rbind(
        list_derived_inconsistencies,
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
                   parameter = rep("exch_acidiy", length(i)),
                   parameter_unit = rep("cmol+ kg-1", length(i)),
                   parameter_value = df$exch_acidiy[i],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(i)),
                   change_date = df$change_date[i],
                   download_date = rep(download_date_pir, length(i))))

      }
    }
  }


  # Remove redundant variables

  df <- df %>%
    select(-sum_texture)

  # Save the survey form and inconsistency list ----
  # for the given survey
  # form to the global environment

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)
    assign_env(paste0("list_derived_inconsistencies_", survey_form),
               list_derived_inconsistencies)
  } else {
    return(df)
  }

}
