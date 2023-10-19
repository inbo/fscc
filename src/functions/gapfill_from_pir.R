
#' Gap-fill data provided by partners in returned PIRs
#'
#' This function is designed to gap-fill data based on Partner Inconsistency
#' Reports (PIR). It takes a survey form or a list of survey forms,
#' imports the original and checked PIRs, identifies inconsistencies with
#' new information provided by the partners,
#' and updates the data accordingly if this has not yet been updated by
#' the partners in layer 0.
#'
#' @param survey_form A data frame representing the survey form data.
#' @param name_survey_form The name or code of the survey form.
#'   If \code{survey_form} is provided, \code{name_survey_form} should
#'   be a character. If \code{survey_form} is not provided,
#'   \code{name_survey_form} should be a character vector representing
#'   the names or codes of the survey forms.
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

gapfill_from_pir <- function(survey_form = NA,
                             name_survey_form,
                             save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")
  
  # Arrange input dataframe(s) ----

  # Assert that input arguments are in correct classes
  
  assertthat::assert_that("character" %in% class(name_survey_form),
                          msg = paste0("Input variable 'name_survey_form' ",
                                       "should be a character."))
  
  # if survey_form is not NA,
  # assert that name_survey_form is not a code (e.g. "so")

  if (!is.na(survey_form)) {

    assertthat::assert_that("data.frame" %in% class(survey_form),
                            msg = paste0("Input variable 'survey_form' ",
                                         "should be a dataframe."))

    assertthat::assert_that(length(name_survey_form) == 1 &&
                            length(unlist(strsplit(name_survey_form, "_"))) >
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
  
  if (length(name_survey_form) == 1 &&
      length(unlist(strsplit(name_survey_form, "_"))) == 1 &&
      name_survey_form %in% names(list_data_tables)) {

    code_survey <- name_survey_form

    name_survey_form <- paste0(code_survey, "_", 
                               list_data_tables[[code_survey]])
    
  }
  
  

  for (j in seq_along(name_survey_form)) {

    if (!is.na(survey_form)) {

      df <- survey_form
      
    } else {
      
      df <- get_env(name_survey_form[j])
      
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
  }
  
  
  
  
  # Import checked pir ----
  
  if (!exists("pir_checked") ||
      (exists("pir_checked") &&
       any(!"data.frame" %in% class(pir_checked)))) {

    assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                               "20230302_checked_pirs.xlsx")),
                            msg = paste0("'./data/additional_data/",
                                         "20230302_checked_pirs.xlsx' ",
                                         "does not exist."))
    
  pir_checked <-
    openxlsx::read.xlsx("./data/additional_data/20230302_checked_pirs.xlsx",
                        sheet = 1)

  # Checked pir uses "partner_code" + "code_plot" as "plot_id"
  # We use "code_country" as "code_plot" as "plot_id"
  # Correct this
  pir_checked <- pir_checked %>%
    mutate(plot_id = paste0(code_country, "_",
                            code_plot)) %>%
    mutate(plot_id_response = paste0(code_country, "_",
                                     sub(".*_", "", plot_id_response)))

  # Rename rule_id if needed
  if ("rule_ID" %in% colnames(pir_checked)) {
    pir_checked <- rename(pir_checked,
                          rule_id = rule_ID)
  }

  # Remove " (estimated by FSCC)"
  pir_checked <- pir_checked %>%
    mutate(parameter_value = gsub(" \\(estimated by FSCC\\)",
                                       "",
                                       parameter_value))
  
  # "1 - The reported value is extreme but correct"
  # "2 - The correct value is resubmitted/the inconsistency is solved"
  # "3 - The reported value is removed (e.g. resubmitted as NA)"
  # "4 - No (good) data are available for this parameter"
  # "5 - Other"
  
  pir_checked$code_nfc_action_taken <-
    gsub("^1.*", "1", pir_checked$code_nfc_action_taken)
  pir_checked$code_nfc_action_taken <-
    gsub("^2.*", "2", pir_checked$code_nfc_action_taken)
  pir_checked$code_nfc_action_taken <-
    gsub("^3.*", "3", pir_checked$code_nfc_action_taken)
  pir_checked$code_nfc_action_taken <-
    gsub("^4.*", "4", pir_checked$code_nfc_action_taken)
  pir_checked$code_nfc_action_taken <-
    gsub("^5.*", "5", pir_checked$code_nfc_action_taken)
  pir_checked$code_nfc_action_taken <-
    as.integer(pir_checked$code_nfc_action_taken)
  }
  
  
  
  
  # Manipulate checked pir ----

  pir_checked_survey_form <-
    pir_checked %>%
    # Filter for inconsistencies of the given survey form
    filter(.data$survey_form == name_survey_form[j]) %>%
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
    filter(!is.na(code_nfc_action_taken) &
             !is.na(nfc_remark) &
             !is.na(updated_value)) %>%
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
    # Create a new column with the current data (before any update from pir)
    mutate(parameter_value_current = NA) %>%
     # Create a new column in which you can store whether you use the pir info
    mutate(fscc_action = NA) %>%
    # Relocate
    relocate(plot_id_response, .before = code_nfc_action_taken) %>%
    relocate(unique_inconsistency_id, .before = code_nfc_action_taken) %>%
    relocate(unique_layer_repetition, .before = code_nfc_action_taken)

  
  
  
  
  
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

          if (abs((as.numeric(value_1) / as.numeric(value_2)) - 1) <
              tolerance) {
            outcome <- TRUE
          }
        }
      }
    }
    return(outcome)
  }


  
  
  # Replace values if not up to date ----
  
  for (i in seq_len(nrow(pir_checked_survey_form))) {

    col_ind <- which(colnames(df) == pir_checked_survey_form$parameter[i])
    row_ind <- which(df$unique_layer_repetition ==
                       pir_checked_survey_form$unique_layer_repetition[i])

    if (!identical(row_ind, integer(0))) {
    if (length(row_ind) > 1) {
      row_ind <- row_ind[which(df$code_line[row_ind] ==
                                 pir_checked_survey_form$code_line[i])]
    }
    }

    # If the records haven't been deleted meanwhile

    if (identical(row_ind, integer(0))) {
      
      pir_checked_survey_form$fscc_action[i] <-
        "record no longer exists"
      
    } else {
    
      # if the error does not concern rule_id FSCC_12: pH measurement method
      
      if (pir_checked_survey_form$rule_id[i] != "FSCC_12") {
        
        pir_checked_survey_form$parameter_value_current[i] <-
          df[row_ind, col_ind]

    # If the change_date was later than 2 March 2023 +
    # the value is different from parameter_value_orig:
    # We can assume that layer 0 was corrected in response to the PIR
    # So no need to update data using "updated_value"
    
    if (as.Date(df$change_date[row_ind]) >= as.Date("2023-03-02") &&
        !is_the_same(df[row_ind, col_ind],
                     pir_checked_survey_form$parameter_value_orig[i])) {
      pir_checked_survey_form$fscc_action[i] <- "already updated in layer 0"
    }

    # If the change_date is before 2 March 2023 OR
    if (as.Date(df$change_date[row_ind]) < as.Date("2023-03-02") ||
        # if the values haven't been updated
        is_the_same(df[row_ind, col_ind],
                    pir_checked_survey_form$parameter_value_orig[i])) {
      
      # If the updated_value says "record_to_be_removed",
      # then the whole row needs to be removed
      
      if (pir_checked_survey_form$updated_value[i] == "record_to_be_removed") {
        
        df <- df[-row_ind, ]
        pir_checked_survey_form$fscc_action[i] <- "removed"
        
      } else

      # If updated_value is not the same like the original value
      if (!is_the_same(df[row_ind, col_ind],
                       pir_checked_survey_form$updated_value[i])) {

        df[row_ind, col_ind] <- pir_checked_survey_form$updated_value[i]

        pir_checked_survey_form$fscc_action[i] <- "updated"
      }
      
      }
      } else {
      
        # If the inconcistency concerns FSCC_12: pH measurement method

        pir_checked_survey_form$parameter_value_current[i] <-
          df[row_ind, which(names(df) == "other_obs")]
        
        
        if (as.Date(df$change_date[row_ind]) >= as.Date("2023-03-02") &&
            str_detect(df$other_obs[row_ind], "H2O|H20|water|CaCl2")) {
          pir_checked_survey_form$fscc_action[i] <- "already updated in layer 0"
        }
        
        # If the change_date is before 2 March 2023 OR
        if (as.Date(df$change_date[row_ind]) < as.Date("2023-03-02") ||
            # if the values haven't been updated
            !str_detect(df$other_obs[row_ind], "H2O|H20|water|CaCl2")) {
          
          df[row_ind, which(names(df) == "other_obs")] <-
            paste0(df[row_ind, which(names(df) == "other_obs")], "; ",
                   pir_checked_survey_form$updated_value[i])
          
          pir_checked_survey_form$fscc_action[i] <- "updated"
          
        }
        
    }
    }
  }
  
  
  # Export ----
  
  if (!is.na(survey_form)) {
    
    return(df)
    
    assign_env(paste0("pir_applied_", name_survey_form[j]),
               pir_checked_survey_form)
    
  } else {
    
    if (save_to_env == TRUE) {
      assign_env(name_survey_form[j],
                 df)
    }
    assign_env(paste0("pir_applied_", name_survey_form[j]),
               pir_checked_survey_form)
  }
  
  } # End of loop over survey forms
  
  
}
