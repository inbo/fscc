
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

  df$layer_thickness <- NA
  df$bulk_density_layer_weight <- NA
  df$sum_texture <- NA
  df$c_to_n_ratio <- NA
  df$sum_base_cations <- NA # Ca, Mg, K, Na
  df$sum_acid_cations <- NA # Al, Fe, Mn, free H+

  for (i in seq_len(nrow(df)))  {

    # layer_thickness

    if (!is.na(df$layer_limit_superior[i]) &&
       !is.na(df$layer_limit_inferior[i]) &&
       (df$layer_limit_superior[i] != df$layer_limit_inferior[i]))  {

      limits <- c(df$layer_limit_superior[i], df$layer_limit_inferior[i])
      df$layer_thickness[i] <- max(limits) - min(limits)
      }

    # bulk_density_layer_weight

    if (!is.na(df$organic_layer_weight[i]) &&
        !is.na(df$layer_thickness[i]))  {

      df$bulk_density_layer_weight[i] <-
        df$organic_layer_weight[i] / (df$layer_thickness[i] * 1e-2) #kg m^(-3)
        # to compare with plausibility range of bulk density
    }

    # sum_texture

    if (!is.na(df$part_size_clay[i]) &&
        !is.na(df$part_size_silt[i]) &&
        !is.na(df$part_size_sand[i])) {

      # LOQ = 1 %
      # if (df$part_size_clay[i] == -1) {
      #     clay <- 0.5
      # } else {
      #     clay <- df$part_size_clay[i]
      # }
      # if (df$part_size_silt[i] == -1) {
      #     silt <- 0.5
      # } else {
      #     silt <- df$part_size_silt[i]
      # }
      # if (df$part_size_sand[i] == -1) {
      #     sand <- 0.5
      # } else {
      #     sand <- df$part_size_sand[i]
      # }

      df$sum_texture[i] <- df$part_size_clay[i] +
        df$part_size_silt[i] +
        df$part_size_sand[i]
      # This should be between 97 and 103 %
  }

    # c_to_n_ratio

    if (!is.na(df$organic_carbon_total[i]) &&
        !is.na(df$n_total[i]) &&
        !is.na(df$organic_carbon_total_loq[i]) &&
        !is.na(df$n_total_loq[i]) &&
        (df$organic_carbon_total[i] >= df$organic_carbon_total_loq[i]) &&
        (df$n_total[i] >= df$n_total_loq[i])) {

      df$c_to_n_ratio[i] <- (df$organic_carbon_total[i]) / (df$n_total[i])
      # This should be in a plausible range (1 to 100)
  }

    # sum_base_cations

    # TO DO: update LOQ implementation

    if (!is.na(df$exch_ca[i]) &&
        !is.na(df$exch_mg[i]) &&
        !is.na(df$exch_k[i]) &&
        !is.na(df$exch_na[i]) &&
        (sum(c(df$exch_ca[i], df$exch_mg[i],
               df$exch_k[i], df$exch_na[i]) != (-1)) > 1)) {

      # LOQ = 0.03 cmol+ kg-1
      if (df$exch_ca[i] == -1) {
          exch_ca <- 0.015
      } else {
          exch_ca <- df$exch_ca[i]
      }
      if (df$exch_mg[i] == -1) {
          exch_mg <- 0.015
      } else {
          exch_mg <- df$exch_mg[i]
      }
      if (df$exch_k[i] == -1) {
          exch_k <- 0.015
      } else {
          exch_k <- df$exch_k[i]
      }
      if (df$exch_na[i] == -1) {
          exch_na <- 0.015
      } else {
          exch_na <- df$exch_na[i]
      }

      df$sum_base_cations[i] <- exch_ca + exch_mg + exch_k + exch_na
      # This should be smaller or equal to CEC (exch_cec)
    }

    # sum_acid_cations

    # TO DO: update LOQ implementation

    if (!is.na(df$exch_al[i]) &&
        !is.na(df$exch_fe[i]) &&
        !is.na(df$exch_mn[i]) &&
        !is.na(df$free_h[i]) &&
        (sum(c(df$exch_al[i], df$exch_fe[i],
               df$exch_mn[i], df$free_h[i]) != (-1)) > 1)) {

      # LOQ = 0.02 cmol+ kg-1
      if (df$exch_al[i] == -1) {
        exch_al <- 0.01
      } else {
        exch_al <- df$exch_al[i]
      }
      if (df$exch_fe[i] == -1) {
        exch_fe <- 0.01
      } else {
        exch_fe <- df$exch_fe[i]
      }
      if (df$exch_mn[i] == -1) {
        exch_mn <- 0.01
      } else {
        exch_mn <- df$exch_mn[i]
      }

      # LOQ = 0.1 cmol+ kg-1
      if (df$free_h[i] == -1) {
        free_h <- 0.05
      } else {
        free_h <- df$free_h[i]
      }

      df$sum_acid_cations[i] <- exch_al + exch_fe + exch_mn + free_h
      # This should approximate the exchangeable acidity (exch_acidiy)

    }
  }
  }


  # "pfh" survey forms ----

  if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

    # Add new variables ----

    df$sum_texture <- NA
    df$c_to_n_ratio <- NA
    df$sum_base_cations <- NA # Ca, Mg, K, Na

    for (i in 1:nrow(df)) {

      # sum_texture

      if (!is.na(df$horizon_clay[i]) &
          !is.na(df$horizon_silt[i]) &
          !is.na(df$horizon_sand[i])) {

        # LOQ = 1 %
        # if (df$horizon_clay[i] == -1) {
        #   clay <- 0.5
        # } else {
        #     clay <- df$horizon_clay[i]
        # }
        # if (df$horizon_silt[i] == -1) {
        #   silt <- 0.5
        # } else {
        #     silt <- df$horizon_silt[i]
        # }
        # if (df$horizon_sand[i] == -1) {
        #   sand <- 0.5
        # } else {
        #     sand <- df$horizon_sand[i]
        # }

        df$sum_texture[i] <- df$horizon_clay[i] +
          df$horizon_silt[i] +
          df$horizon_sand[i]
        }

      # c_to_n_ratio

      if (!is.na(df$horizon_c_organic_total[i]) &&
          !is.na(df$horizon_n_total[i]) &&
          !is.na(df$horizon_c_organic_total_loq[i]) &&
          !is.na(df$horizon_n_total_loq[i]) &&
          (df$horizon_c_organic_total[i] >=
             df$horizon_c_organic_total_loq[i]) &&
          (df$horizon_n_total[i] >=
            df$horizon_n_total_loq[i])) {

        df$c_to_n_ratio[i] <-
          (df$horizon_c_organic_total[i]) / (df$horizon_n_total[i])}

      # sum_base_cations

      # TO DO: update LOQ implementation

      if (!is.na(df$horizon_exch_ca[i]) &&
          !is.na(df$horizon_exch_mg[i]) &&
          !is.na(df$horizon_exch_k[i]) &&
          !is.na(df$horizon_exch_na[i]) &&
          (sum(c(df$horizon_exch_ca[i], df$horizon_exch_mg[i],
                 df$horizon_exch_k[i], df$horizon_exch_na[i]) != (-1)) > 1)) {

        # LOQ = 0.03 cmol+ kg-1

        if (df$horizon_exch_ca[i] == -1) {
          exch_ca <- 0.015
          } else {
          exch_ca <- df$horizon_exch_ca[i]
          }
        if (df$horizon_exch_mg[i] == -1) {
          exch_mg <- 0.015
          } else {
          exch_mg <- df$horizon_exch_mg[i]
          }
        if (df$horizon_exch_k[i] == -1) {
          exch_k <- 0.015
          } else {
          exch_k <- df$horizon_exch_k[i]
          }
        if (df$horizon_exch_na[i] == -1) {
          exch_na <- 0.015
          } else {
          exch_na <- df$horizon_exch_na[i]
          }

        df$sum_base_cations[i] <- exch_ca + exch_mg + exch_k + exch_na

        }
    }
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

        if ("code_layer_original" %in% names(df)) {
          ind_layer_horizon <- as.character(df$code_layer_original)
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

        if ("code_layer_original" %in% names(df)) {
          ind_layer_horizon <- as.character(df$code_layer_original)
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
    }




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

        if ("code_layer_original" %in% names(df)) {
            ind_layer_horizon <- as.character(df$code_layer_original)
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

        if ("code_layer_original" %in% names(df)) {
            ind_layer_horizon <- as.character(df$code_layer_original)
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

        if ("code_layer_original" %in% names(df)) {
            ind_layer_horizon <- as.character(df$code_layer_original)
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
