
apply_additional_manual_corr <- function(survey_form,
                                           data_frame = NULL) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(paste0(" \nApply additional manual corrections in '", survey_form,
             "'.\n"))

  name_survey_form <- survey_form

  # Arrange input dataframe(s) ----

  # Assert that input arguments are in correct classes

  assertthat::assert_that("character" %in% class(name_survey_form),
                          msg = paste0("Input variable 'survey_form' ",
                                       "should be a character."))


  if (is.null(data_frame)) {
    df <- get_env(name_survey_form)
  } else {
    df <- data_frame
  }

    # Gap-fill change_date if missing (e.g. in "so_prf")
    # Assumption: 2009-12-06 (date on which database was established at PCC)

    df <- df %>%
      mutate(change_date = ifelse(is.na(.data$change_date),
                                  as.character(as.Date("2009-12-06")),
                                  .data$change_date))


    # Import additional manual corrections ----

      path <- "./data/additional_data/additional_manual_corrections_fscc.xlsx"

      assertthat::assert_that(file.exists(path))

      df_to_correct <-
        openxlsx::read.xlsx(path,
                            sheet = 1) %>%
        mutate(code_line = as.character(code_line)) %>%
        rename(observation_date = change_date) %>%
        # Assuming the Excel epoch is 1899-12-30
        mutate(observation_date = as.Date(.data$observation_date - 1,
                                            origin = "1899-12-30")) %>%
        mutate(observation_date =
                 as.Date(parsedate::parse_iso_8601(.data$observation_date)))



      assertthat::assert_that(length(which(!is.na(df_to_correct$code_line))) ==
                                nrow(df_to_correct))



    # Manipulate df_to_correct ----

    df_to_correct_survey_form <-
      df_to_correct %>%
        # Remove records related to "layer_number"
        # To do: systematically implement these issues in
        # ./src/functions/get_layer_inconsistencies.R
        filter(parameter != "layer_number") %>%
        # Remove records related to "code_layer"
        # To do: systematically implement these issues in
        # ./src/functions/get_layer_inconsistencies.R
        filter(parameter != "code_layer") %>%
        # Filter for inconsistencies of the given survey form
        filter(.data$survey_form == name_survey_form) %>%
        # Create column "unique_inconsistency_id"
        mutate(unique_inconsistency_id =
                 paste0(code_line, "_",
                        parameter, "_",
                        inconsistency_reason)) %>%
        # # Create column "unique_layer_repetition"
        # mutate(unique_layer_repetition =
        #          paste0(code_country, "_",
        #                 survey_year, "_",
        #                 # code_plot corrected (e.g. pir feedback from Poland
        #                 # for wrong code_plots)
        #                 sub(".*_", "", plot_id_response), "_",
        #                 code_layer_horizon_master, "_",
        #                 repetition_profile_pit_id)) %>%
        # Create a new column with the current data (before any update)
        mutate(parameter_value_current = NA) %>%
        # Create a new column in which you can store whether you use the info
        mutate(fscc_action = NA)




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





    # Columns that should be converted to numeric class:

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



    if (nrow(df_to_correct_survey_form) > 0) {

    # Evaluate each of the records and replace if not up to date ----

    for (i in seq_len(nrow(df_to_correct_survey_form))) {

      col_ind <- which(colnames(df) == df_to_correct_survey_form$parameter[i])
      row_ind <- which(df$code_line ==
                         df_to_correct_survey_form$code_line[i])

      # If the records haven't been deleted meanwhile

      if (identical(row_ind, integer(0))) {

        df_to_correct_survey_form$fscc_action[i] <-
          "record no longer exists"

      } else {

    assertthat::assert_that(
      length(pull(df[row_ind, col_ind])) == 1)

    df_to_correct_survey_form$parameter_value_current[i] <-
      pull(df[row_ind, col_ind])

    # If the change_date was later than than the observation date +
    # the value is different from parameter_value_orig:
    # We can assume that layer 0 was corrected in response to the PIR
    # So no need to update data using "parameter_value_updated"

    if (as.Date(df$change_date[row_ind]) >=
            df_to_correct_survey_form$observation_date[i] &&
        !is_the_same(pull(df[row_ind, col_ind]),
                     df_to_correct_survey_form$parameter_value_orig[i])) {

      df_to_correct_survey_form$fscc_action[i] <-
        "already updated in layer 0"

    }

    # If the change_date is before the observation date OR
    if (as.Date(df$change_date[row_ind]) <
          df_to_correct_survey_form$observation_date[i] ||
        # if the values haven't been updated
        is_the_same(pull(df[row_ind, col_ind]),
                    df_to_correct_survey_form$parameter_value_orig[i])) {

        # If parameter_value_updated is not the same like the original value
        if (!is_the_same(pull(df[row_ind, col_ind]),
                     df_to_correct_survey_form$parameter_value_updated[i])) {

          if (df_to_correct_survey_form$parameter[i] %in% numeric_columns) {

            df[row_ind, col_ind] <-
              as.numeric(df_to_correct_survey_form$parameter_value_updated[i])

          } else {

            df[row_ind, col_ind] <-
              type.convert(df_to_correct_survey_form$parameter_value_updated[i],
                           as.is = TRUE)
          }

          df_to_correct_survey_form$fscc_action[i] <- "updated"
        }

    }

      }
    } # End of evaluation correction records
}



    # Export ----

    assign_env(paste0("completed_manual_corr_", name_survey_form),
                 df_to_correct_survey_form)

    return(df)




}
