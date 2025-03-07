
#' Gap-fill data provided by partners in returned PIRs
#'
#' This function is designed to gap-fill data based on Partner Inconsistency
#' Reports (PIR). It takes a survey form or a list of survey forms,
#' imports the original and checked PIRs, identifies inconsistencies with
#' new information provided by the partners,
#' and updates the data accordingly if this has not yet been updated by
#' the partners in layer 0.
#'
#' @param code_survey The name or code of the survey form.
#'   If \code{survey_form} is provided, \code{name_survey_form} should
#'   be a character. If \code{survey_form} is not provided,
#'   \code{name_survey_form} should be a character vector representing
#'   the names or codes of the survey forms.
#' @param data_frame A data frame representing the survey form data.
#' @param save_to_env Logical, indicating whether to save the resulting data
#'   frames to the environment. Default is \code{FALSE}.
#'
#' @return If \code{survey_form} is provided, the function returns the
#'   updated data frame. If \code{survey_form} is not provided,
#'   and \code{save_to_env} is \code{TRUE}, the updated data frames are saved
#'   to the environment.
#'
#' @details
#' This function reads the original and checked PIRs, identifies new
#' information provided by partners in the PIRs,
#' and updates the provided data frame(s) if this has not yet been done by
#' the partner in layer 0.
#'#'
#' @examples
#' gapfill_from_pir(survey_form = so_som, name_survey_form = "so_som")
#' gapfill_from_pir(name_survey_form = c("so_som", "si_eve"),
#'                  save_to_env = TRUE)
#'

gapfill_from_pir <- function(code_survey,
                             data_frame = NULL,
                             save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(paste0(" \nGap-fill '", code_survey,
             "' from PIRs (data corrected by partners)\n"))

  # Arrange input dataframe(s) ----

  # Assert that input arguments are in correct classes

  assertthat::assert_that("character" %in% class(code_survey),
                          msg = paste0("Input variable 'code_survey' ",
                                       "should be a character."))

  # if survey_form is not NA,
  # assert that code_survey is not a code (e.g. "so")

  if (!is.null(data_frame)) {

    survey_forms <- code_survey

    assertthat::assert_that("data.frame" %in% class(data_frame),
                            msg = paste0("Input variable 'data_frame' ",
                                         "should be a dataframe."))

    assertthat::assert_that(length(code_survey) == 1 &&
                            length(unlist(strsplit(code_survey, "_"))) >
                              1,
                            msg = paste0("The name of the input survey form ",
                                         "should consist of two parts, e.g. ",
                                         "'so_som' and not 'so'."))
  }

  # if a survey code (e.g. "so") is given:

  list_data_tables <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                           s1 = c("som", "prf", "pls", "pfh", "lqa"),
                           si = c("eve", "plt", "sta", "tco"),
                           y1 = c("pl1", "st1", "ev1"),
                           sw = c("swa", "swc"))

  if (length(code_survey) == 1 &&
      length(unlist(strsplit(code_survey, "_"))) == 1 &&
      code_survey %in% names(list_data_tables)) {

    survey_forms <- paste0(code_survey, "_",
                           list_data_tables[[code_survey]])

  }




  # Import original pir ----

  if (!exists("pir_orig") ||
      (exists("pir_orig") &&
       any(!"data.frame" %in% class(pir_orig)))) {

    assertthat::assert_that(file.exists(paste0("./output/pirs/20230302_pir/",
                                               "20230302_inconsistency_report_",
                                               "all_partners",
                                               ".xlsx")),
                            msg = paste0("'./output/pirs/20230302_pir/",
                                         "20230302_inconsistency_report_",
                                         "all_partners",
                                         ".xlsx' does not exist."))

    pir_orig <-
      openxlsx::read.xlsx(paste0("./output/pirs/20230302_pir/",
                                 "20230302_inconsistency_report_all_partners",
                                 ".xlsx"),
                          sheet = 1)

    # Checked pir uses "partner_code" + "code_plot" as "plot_id"
    # We use "code_country" as "code_plot" as "plot_id"
    # Correct this
    pir_orig <- pir_orig %>%
      rename(plot_id = plot_ID) %>%
      rename(rule_id = rule_ID) %>%
      mutate(plot_id = paste0(code_country, "_",
                              code_plot))

    # Create column "unique_inconsistency_id"
    pir_orig <- pir_orig %>%
      mutate(unique_inconsistency_id =
               paste0(survey_form, "_",
                      partner_code, "_",
                      plot_id, "_",
                      survey_year, "_",
                      code_layer_horizon_master, "_",
                      repetition_profile_pit_id, "_",
                      code_line, "_",
                      parameter, "_",
                      rule_id))

    pir_orig <- pir_orig %>%
      mutate(parameter_value = ifelse(parameter_value == "",
                                      NA,
                                      parameter_value))

    assign_env("pir_orig",
               pir_orig)

  }




  # Import checked pir ----

  source("./src/functions/get_pir.R")

  if (!exists("pir_checked") ||
      (exists("pir_checked") &&
       any(!"data.frame" %in% class(pir_checked)) &&
       !"rule_id" %in% names(pir_checked))) {

  pir_checked <- get_pir()

  }

  # Rename rule_id if needed
  if ("rule_ID" %in% colnames(pir_checked)) {
    pir_checked <- rename(pir_checked,
                          rule_id = rule_ID)
  }








  # Create function is_the_same ----
  is_the_same <- function(value_1,
                          value_2,
                          tolerance = 0.0001) {
    #' Check if Two Values are essentially the same
    #'
    #' This function compares two values to determine if they are essentially
    #' the same. It accounts for cases where both values are NAs,
    #' both are non-numeric and identical,
    #' or both are numeric and within a specified tolerance of each other.
    #'
    #' @param value_1 The first value for comparison.
    #' @param value_2 The second value for comparison.
    #' @param tolerance Numeric tolerance level (relative) for considering
    #' numeric values as the same. Default is 0.0001.
    #'
    #' @return Logical: TRUE if the values are essentially the same,
    #' FALSE otherwise.
    #'
    #' @details
    #' If both \code{value_1} and \code{value_2} are NAs,
    #' the function returns TRUE.
    #' If both are non-numeric and identical, it returns TRUE.
    #' If both are numeric and the relative difference is within the
    #' specified tolerance, it returns TRUE.
    #' Otherwise, it returns FALSE.
    #'
    #' @examples
    #' is_the_same("a", NA) # FALSE
    #' is_the_same(5, 5.00009) # TRUE

    outcome <- FALSE

    # TRUE if both are NAs

    if (is.na(value_1) &&
        is.na(value_2)) {
      outcome <- TRUE

    } else {

      source("./src/functions/gives_warning.R")

      # TRUE if both are not numbers and exactly the same

      if (gives_warning(as.numeric(value_1)) &&
          gives_warning(as.numeric(value_2))) {

        if (identical(value_1, value_2)) {
          outcome <- TRUE
        }

        # If the conversion to a numeric does not give warnings
        # (which is also possible if the value is an NA)
      } else {

        # TRUE if both are numbers (not NAs) and roughly the same
        if (!gives_warning(as.numeric(value_1)) &&
            !gives_warning(as.numeric(value_2)) &&
            !is.na(value_1) &&
            !is.na(value_2)) {

          # If both values are 0 or
          # if both values are approximately the same
          # (not possible to divide by 0)

          if ((abs(as.numeric(value_1)) < tolerance &&
               abs(as.numeric(value_2)) < tolerance) ||
              (abs((as.numeric(value_1) / as.numeric(value_2)) - 1) <
               tolerance)) {

            outcome <- TRUE
          }
        }
      }
    }
    return(outcome)
  }



  # Specify columns that should be converted to numeric class:

  numeric_columns <- c(
    "layer_limit_superior",
    "layer_limit_inferior",
    "moisture_content",
    "part_size_clay",
    "part_size_silt",
    "part_size_sand",
    "bulk_density",
    "coarse_fragment_vol",
    "organic_layer_weight",
    "ph_cacl2",
    "ph_h2o",
    "organic_carbon_total",
    "n_total",
    "carbonates",
    "exch_acidiy",
    "exch_ca",
    "exch_fe",
    "exch_mg",
    "exch_mn",
    "exch_na",
    "free_h",
    "extrac_al",
    "extrac_ca",
    "extrac_cd",
    "extrac_cr",
    "extrac_cu",
    "extrac_fe",
    "extrac_hg",
    "extrac_k",
    "extrac_mg",
    "extrac_mn",
    "extrac_na",
    "extrac_ni",
    "extrac_p",
    "extrac_pb",
    "extrac_s",
    "extrac_zn",
    "tot_ca",
    "base_saturation",
    "p_ox",
    "horizon_limit_up",
    "horizon_limit_low",
    "horizon_silt",
    "horizon_sand",
    "horizon_clay",
    "horizon_coarse_weight",
    "horizon_c_organic_total",
    "horizon_caco3_total",
    "horizon_n_total",
    "horizon_ph",
    "horizon_elec_cond",
    "horizon_exch_ca",
    "horizon_exch_mg",
    "horizon_exch_k",
    "horizon_exch_na",
    "horizon_cec",
    "horizon_bulk_dens_measure")






  # Evaluate for each survey form ----

  for (j in seq_along(survey_forms)) {

    if (!is.null(data_frame) &&
        is.data.frame(data_frame)) {

      df <- data_frame

    } else {

      df <- get_env(survey_forms[j])

    }


  # Gap-fill change_date if missing (e.g. in "so_prf")
  # Assumption: 2009-12-06 (date on which database was established at PCC)

  df <- df %>%
    mutate(change_date = ifelse(is.na(.data$change_date),
                                as.character(as.Date("2009-12-06")),
                                .data$change_date))




  # Manipulate checked pir ----

  pir_checked_survey_form <-
    pir_checked %>%
    # Filter for inconsistencies of the given survey form
    filter(.data$survey_form == survey_forms[j]) %>%
    # Create column "unique_inconsistency_id"
    mutate(unique_inconsistency_id =
               paste0(survey_form, "_",
                      partner_code, "_",
                      plot_id, "_",
                      survey_year, "_",
                      code_layer_horizon_master, "_",
                      repetition_profile_pit_id, "_",
                      code_line, "_",
                      parameter, "_",
                      rule_id)) %>%
    # Create column "unique_layer_repetition"
    mutate(unique_layer_repetition =
             paste0(code_country, "_",
                    survey_year, "_",
                    # code_plot corrected (e.g. pir feedback from Poland
                    # for wrong code_plots)
                    sub(".*_", "", plot_id_response), "_",
                    code_layer_horizon_master, "_",
                    repetition_profile_pit_id)) %>%
    # Add a column with the original data
    left_join(pir_orig %>%
                  select(unique_inconsistency_id, parameter_value) %>%
                  rename(parameter_value_orig = parameter_value),
              by = "unique_inconsistency_id") %>%
    mutate(parameter_value_orig = gsub(" \\(estimated by FSCC\\)",
                                       "",
                                       parameter_value_orig)) %>%
    # Sometimes not clear whether the column "parameter_value" was updated
    # by the partner. So possibly, "updated_value"" does not contain all
    # newly delivered data from the pirs.
    mutate(updated_value =
             suppressWarnings(
             # If they are basically different (more than 0.1 % difference)
             ifelse((abs((as.numeric(parameter_value) /
                           as.numeric(parameter_value_orig)) - 1) > 0.0001) &
                    is.na(updated_value),
                    parameter_value,
                    updated_value))) %>%
    # Sometimes not clear whether the column "parameter_value" was updated
    # by the partner. So possibly, I wrongly placed the "parameter_value" info
    # into the "updated_value" column because I wrongly assumed the values
    # looked updated
    filter(is.na(updated_value) |
             updated_value != parameter_value_orig) %>%
    # Filter for inconsistencies with an actual response
    filter(!is.na(code_nfc_action_taken)) %>%
    # Ignore code_nfc_action_taken if updated_value is empty
    # (we can't do much with it anyway for now). Not relevant to add
    # "confirmations" (e.g. extreme but correct values; no data available...)
    # to the layer 1 data for now. We'll have to use statistics and
    # objective expert reasons to exclude values.
    filter(!is.na(updated_value)) %>%
    # if "updated_value" says "data_to_be_removed", partners indicated
    # that the value should be removed (i.e. replaced by NA)
    mutate(updated_value =
             ifelse(updated_value == "data_to_be_removed",
                    NA,
                    updated_value)) %>%
    # Create a new column with the current layer 0 data
    # (before any update from pir)
    mutate(parameter_value_current = NA) %>%
     # Create a new column in which you can store whether you use the pir info
    mutate(fscc_action = NA) %>%
    # Relocate
    relocate(plot_id_response, .before = code_nfc_action_taken) %>%
    relocate(unique_inconsistency_id, .before = code_nfc_action_taken) %>%
    relocate(unique_layer_repetition, .before = code_nfc_action_taken)



  # Replace values if not up to date ----

  for (i in seq_len(nrow(pir_checked_survey_form))) {

    col_ind <- which(colnames(df) == pir_checked_survey_form$parameter[i])
    row_ind <- which(df$code_line ==
                       pir_checked_survey_form$code_line[i])

    # Sometimes code_line doesn't work:
    # - if code_line changed for unclear reasons
    # - if data were resubmitted under another survey_year

    if (identical(row_ind, integer(0)) &&
        !identical(
        pir_checked_survey_form$updated_value[i], "record_to_be_removed")) {

      # Try to find the record using unique_layer_repetition or
      # unique_survey_profile

      assertthat::assert_that(
        grepl("som|pfh|prf", pir_checked_survey_form$survey_form[i]))

      key_i <- case_when(
        grepl("prf", pir_checked_survey_form$survey_form[i]) ~
          paste0(pir_checked_survey_form$code_country[i], "_",
                 pir_checked_survey_form$survey_year[i], "_",
                 sub(".*_", "",
                     pir_checked_survey_form$plot_id_response[i]), "_",
                 pir_checked_survey_form$repetition_profile_pit_id[i]),
        grepl("som|pfh", pir_checked_survey_form$survey_form[i]) ~
          pir_checked_survey_form$unique_layer_repetition[i])


      if (grepl("prf", pir_checked_survey_form$survey_form[i])) {
        row_ind <- which(df$unique_survey_profile == key_i)
      }

      if (grepl("pfh|som", pir_checked_survey_form$survey_form[i])) {
        row_ind <- which(df$unique_layer_repetition == key_i)
      }
    }


    if (identical(row_ind, integer(0)) &&
        !identical(
        pir_checked_survey_form$updated_value[i], "record_to_be_removed")) {

      # Try to find the record without the survey_year

      assertthat::assert_that(
        grepl("som|pfh|prf", pir_checked_survey_form$survey_form[i]))

      key_i <- case_when(
        grepl("prf", pir_checked_survey_form$survey_form[i]) ~
          paste0(pir_checked_survey_form$code_country[i], "_",
                 sub(".*_", "",
                     pir_checked_survey_form$plot_id_response[i]), "_",
                 pir_checked_survey_form$repetition_profile_pit_id[i]),
        grepl("som|pfh", pir_checked_survey_form$survey_form[i]) ~
          paste0(pir_checked_survey_form$code_country[i], "_",
                 sub(".*_", "",
                     pir_checked_survey_form$plot_id_response[i]), "_",
                 pir_checked_survey_form$code_layer_horizon_master[i], "_",
                 pir_checked_survey_form$repetition_profile_pit_id[i]))

      if (grepl("prf", pir_checked_survey_form$survey_form[i])) {

        match_col <- paste0(df$plot_id, "_",
                            df$profile_pit_id)
      }

      if (grepl("pfh", pir_checked_survey_form$survey_form[i])) {

        match_col <- paste0(df$plot_id, "_",
                            df$horizon_master, "_",
                            df$profile_pit_id)
      }

      if (grepl("som", pir_checked_survey_form$survey_form[i])) {

        match_col <- paste0(df$plot_id, "_",
                            df$code_layer, "_",
                            df$repetition)
      }

      row_ind <- which(match_col == key_i)

      if (!identical(row_ind, integer(0)) &&
          length(row_ind) > 1) {

        row_ind <- row_ind[which.min(abs(
          df$survey_year[row_ind] - pir_checked_survey_form$survey_year[i]))]

        assertthat::assert_that(length(row_ind == 1) ||
                                  grepl("pfh",
                                        pir_checked_survey_form$survey_form[i]))

        if (length(row_ind > 1) &&
            grepl("pfh", pir_checked_survey_form$survey_form[i])) {

          # This can be the case if two layers in the same profile have
          # the same horizon_master (e.g. C in 2_15)

          # Check if parameter_value_orig equals any of the
          # selected rows in df

          row_ind <- which(df[row_ind,
                              which(names(df) ==
                                      pir_checked_survey_form$parameter[i])] ==
                             pir_checked_survey_form$parameter_value_orig[i])

          assertthat::assert_that(
            identical(row_ind, integer(0)) ||
                        length(row_ind) == 1)

        }
      }
    }


    # If the records haven't been deleted meanwhile

    if (identical(row_ind, integer(0))) {

      pir_checked_survey_form$fscc_action[i] <-
        "Record no longer exists"

    } else {

      # If the error does not concern rule_id FSCC_12 (pH measurement method)

      if (pir_checked_survey_form$rule_id[i] != "FSCC_12") {

        assertthat::assert_that(
          length(pull(df[row_ind, col_ind])) == 1)

        pir_checked_survey_form$parameter_value_current[i] <-
          pull(df[row_ind, col_ind])

    # If the change_date was later than 2 March 2023 +
    # the value is different from parameter_value_orig:
    # We can assume that layer 0 was corrected in response to the PIR
    # So no need to update data using "updated_value"

    if (as.Date(df$change_date[row_ind]) >= as.Date("2023-03-02") &&
        !is_the_same(pull(df[row_ind, col_ind]),
                     pir_checked_survey_form$parameter_value_orig[i])) {
      pir_checked_survey_form$fscc_action[i] <- "Already updated in layer 0"
    }

    # If the change_date is before 2 March 2023 OR
    if (as.Date(df$change_date[row_ind]) < as.Date("2023-03-02") ||
        # If the values haven't been updated
        is_the_same(pull(df[row_ind, col_ind]),
                    pir_checked_survey_form$parameter_value_orig[i])) {

      # If the updated_value says "record_to_be_removed",
      # then the whole row needs to be removed

      if (pir_checked_survey_form$updated_value[i] == "record_to_be_removed") {

        df <- df[-row_ind, ]
        pir_checked_survey_form$fscc_action[i] <-
          "Record removed as specified in PIR"

      } else

      # If updated_value is not the same like the original value,
      # The value needs to be updated in the survey form ----

      if (!is_the_same(pull(df[row_ind, col_ind]),
                       pir_checked_survey_form$updated_value[i])) {

        if (pir_checked_survey_form$parameter[i] %in% numeric_columns) {

          df[row_ind, col_ind] <-
            as.numeric(pir_checked_survey_form$updated_value[i])

        } else {

        df[row_ind, col_ind] <-
          type.convert(pir_checked_survey_form$updated_value[i], as.is = TRUE)
        }

        pir_checked_survey_form$fscc_action[i] <-
          "Value updated as specified in PIR"


        # Update "source" column

        name_source_col <-
          paste0(pir_checked_survey_form$parameter[i], "_source")

        if (name_source_col %in% names(df)) {

          df[row_ind, which(names(df) == name_source_col)] <- "PIR"

        }
      }

      }
      } else {

        # If the inconsistency concerns FSCC_12 (pH measurement method)

        pir_checked_survey_form$parameter_value_current[i] <-
          pull(df[row_ind, which(names(df) == "other_obs")])


        if (as.Date(df$change_date[row_ind]) >= as.Date("2023-03-02") &&
            !is.na(df$other_obs[row_ind]) &&
            str_detect(df$other_obs[row_ind], "H2O|H20|water|CaCl2")) {

          pir_checked_survey_form$fscc_action[i] <-
            "Already updated in layer 0"
        }

        # If the change_date is before 2 March 2023 OR
        if (as.Date(df$change_date[row_ind]) < as.Date("2023-03-02") ||
            is.na(df$other_obs[row_ind]) ||
            # If the values haven't been updated
            !str_detect(df$other_obs[row_ind], "H2O|H20|water|CaCl2")) {

          df[row_ind, which(names(df) == "other_obs")] <-
            paste0(pull(df[row_ind, which(names(df) == "other_obs")]), "; ",
                   pir_checked_survey_form$updated_value[i])

          pir_checked_survey_form$fscc_action[i] <-
            "Value updated as specified in PIR"

        }

    }
    }
  }


  # Export ----

  if (save_to_env == TRUE) {
    assign_env(survey_forms[j],
               df)
    assign_env(paste0("pir_applied_", survey_forms[j]),
               pir_checked_survey_form)

  } else {
    return(df)
    assign_env(paste0("pir_applied_", survey_forms[j]),
               pir_checked_survey_form)

  }


  } # End of loop over survey forms


}
