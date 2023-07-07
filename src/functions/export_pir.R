
#' Save newly generated inconsistencies to Excel file(s)
#'
#' This function saves the newly generated inconsistencies to one or multiple
#' Excel file(s), based on the arguments of the function.
#'
#' @param inconsistency_report_name Character string - Name of newly generated
#' inconsistency report in global environment
#' @param per_partner Logical - Indicates whether the inconsistencies have to
#' be split in Excels per partner (based on partner_code)
#' @param per_level Logical - Indicates whether the inconsistencies have to be
#' split in Excels per level (Level I versus Level II)
#' @param ignore_checked_inconsistencies Logical - If FALSE, any
#' inconsistencies which have been confirmed as correct or impossible to solve
#' by partners are excluded from the Excel files
#' @param version Character string - Any characters that need to be attached
#' to the name of the exported Excel files to indicate a version
#'
#' @examples
#' export_pir("inconsistency_report", per_partner = TRUE, per_level = TRUE,
#' ignore_checked_inconsistencies = TRUE)


export_pir <- function(inconsistency_report_name,
                       per_partner = NULL,
                       per_level = NULL,
                       ignore_checked_inconsistencies = NULL,
                       version = NULL) {

  # Attribute default values to input parameters

  if (is.null(version)) {
    version <- NA
  }
  if (is.null(per_partner)) {
    per_partner <- FALSE
  }
  if (is.null(per_level)) {
    per_level <- FALSE
  }
  if (is.null(ignore_checked_inconsistencies)) {
    ignore_checked_inconsistencies <- TRUE
  }


# 1. Inconsistency report: prepare dataframe ----

  # Retrieve inconsistency report

  inconsistency_report_orig <-
    get(inconsistency_report_name, envir = .GlobalEnv)

  # Correct the name of the column "non_duplicated_inconsistency_per_record"

  if ("non_duplicated_error_type_per_record" %in%
      names(inconsistency_report_orig)) {

  names(inconsistency_report_orig)[
    which(names(inconsistency_report_orig) ==
            "non_duplicated_error_type_per_record")] <-
    "non_duplicated_inconsistency_per_record"
  }


  # Convert the dates in "download_date" to a character string.
  # This is the best option to keep the dates homogenous, because in FSCC_43,
  # change_date is a date range

  inconsistency_report_orig$download_date <-
    as.character(inconsistency_report_orig$download_date)

  # Add columns "code_nfc_action_taken" and "nfc_remark"

  inconsistency_report_orig$code_nfc_action_taken <- NA
  inconsistency_report_orig$nfc_remark <- NA

  # Remove inconsistencies which were previously confirmed as correct or
  # impossible to solve by the partners (to avoid bothering them again with
  # the same inconsistencies)

  if (ignore_checked_inconsistencies == FALSE) {

    # Retrieve the "unique_rule_id's" for inconsistencies for which partners
    # returned option 1, 4 or 5 in their response

    # "1 - The reported value is extreme but correct"
    # "2 - The correct value is resubmitted/the inconsistency is solved"
    # "3 - The reported value is removed (e.g. resubmitted as NA)"
    # "4 - No (good) data are available for this parameter"
    # "5 - Other"

    # "unique_rule_id" is a combination of the survey_form, plot_id,
    # survey_year, code_layer_horizon_master, repetition_profile_pit_id,
    # parameter, parameter_value and rule_id.

    source("./src/functions/get_checked_inconsistencies.R")
    inconsistencies_ignore <- get_checked_inconsistencies()

    # Add a column "unique_rule_id" to the current (i.e. new) inconsistency
    # report

    inconsistency_report_orig$unique_rule_id <-
                paste0(inconsistency_report_orig$survey_form, "_",
                       inconsistency_report_orig$plot_id, "_",
                       inconsistency_report_orig$survey_year, "_",
                       inconsistency_report_orig$code_layer_horizon_master, "_",
                       inconsistency_report_orig$repetition_profile_pit_id, "_",
                       inconsistency_report_orig$parameter, "_",
                       inconsistency_report_orig$parameter_value, "_",
                       inconsistency_report_orig$rule_id)

    # Replace double underscores with "_NA_"
    # because in the dataframes that were exported to .xlsx and then
    # reimported (after being checked by the partners),
    # fields with nothing but not an NA (i.e. "") are replaced by NA
    inconsistency_report_orig$unique_rule_id <-
      gsub("__", "_NA_", inconsistency_report_orig$unique_rule_id)

    # Retrieve the row indices of inconsistencies in the new inconsistency
    # report which should be removed since they were previously confirmed
    # as correct or impossible to solve by the partner

    vec_ignore <- which(inconsistency_report_orig$unique_rule_id %in%
                          inconsistencies_ignore)

    # Remove the previously confirmed inconsistencies of the new inconsistency
    # report and remove the "unique_rule_id" column

    inconsistency_report_orig <- inconsistency_report_orig[-vec_ignore, ]
    inconsistency_report <-
      inconsistency_report_orig[,-which(names(inconsistency_report_orig) ==
                                          "unique_rule_id")]
  } else {
      inconsistency_report <- inconsistency_report_orig
      }



  # Create folder to save

  path <- "./output/pir_files/"
  path <- paste0(path, download_date, "_pir")

  # If no character string to indicate the version in the name of the exported
  # Excels was given as input argument

  if (is.na(version)) {

    # Assert that this folder does not exist yet
    assertthat::assert_that(!file.exists(path),
                            msg = paste0("There is already a folder for",
                                         " the given PIR."))

    # Create the folder
    if (!file.exists(path)) {
      dir.create(path)
    }

    } else {

      # If a version character string was given as input argument

      # Assert that this folder does not exist yet
      assertthat::assert_that(!file.exists(paste0(path, "_", version)),
                              msg = paste0("There is already a folder for",
                                           " the given PIR version."))

      # Create the folder
      if (!file.exists(paste0(path, "_", version))) {
        path <- paste0(path, "_", version)
        dir.create(path)
      }
      }



# 2. Attribute catalogue: prepare dataframe ----

  assertthat::assert_that(
    file.exists("./data/additional_data/attribute_catalogue_pir.csv"),
    msg = paste0("There is no 'attribute catalogue' in the",
                 " '.data/additional_data/' folder."))

  attribute_catalogue_pir <-
    read.csv("./data/additional_data/attribute_catalogue_pir.csv", sep = ";")


# 3. Inconsistency catalogue: prepare dataframe ----

  # Import the inconsistency catalogue ----

  assertthat::assert_that(
    file.exists("./data/additional_data/inconsistency_catalogue.csv"),
    msg = paste0("There is no 'inconsistency catalogue' in the",
                 " '.data/additional_data/' folder."))

  inconsistency_catalogue <-
    read.csv("./data/additional_data/inconsistency_catalogue.csv", sep = ";")

  # Remove inconsistency types which do not exist in the new inconsistency
  # report and select the relevant columns

  possible_inconsistencies <- 
    inconsistency_catalogue[
      which((inconsistency_catalogue$to_report_in_PIR == TRUE) &
              (inconsistency_catalogue$rule_id %in%
                 unique(inconsistency_report$rule_id))),
      which(names(inconsistency_catalogue) %in%
              c("check_type", "rule_id",
                "inconsistency_reason",
                "suggested_action_nfc"))]

# 4. Export as a workbook ----

  wb <- createWorkbook()
  addWorksheet(wb, "Inconsistency report")
  addWorksheet(wb, "Attribute catalogue")
  addWorksheet(wb, "Inconsistency catalogue")


  writeData(wb, 1, inconsistency_report)
    addFilter(wb, 1, row = 1, cols = 1:ncol(inconsistency_report))
    dataValidation(wb, 1, col = which(names(inconsistency_report) ==
                                        "code_nfc_action_taken"),
                   rows = 2:(nrow(inconsistency_report)+1),
                   type = "list",
                   value = '"1 - The reported value is extreme but correct, 2 - The correct value is resubmitted/the inconsistency is solved, 3 - The reported value is removed (e.g. resubmitted as NA), 4 - No (good) data are available for this parameter, 5 - Other"') # nolint


    freezePane(wb, 1, firstActiveRow = 2, firstActiveCol = 1)
  writeData(wb, 2, attribute_catalogue_pir)
   protectWorksheet(wb, 2, protect = TRUE)
  writeData(wb, 3, possible_inconsistencies)
    protectWorksheet(wb, 3, protect = TRUE)
    addCreator(wb, "ICP Forests - FSCC")

  openxlsx::saveWorkbook(wb,
                         file = paste0(path, "/", download_date,
                                    "_inconsistency_report_all_partners.xlsx"),
                         overwrite = TRUE)


# 5. Export per partner and not per level ----

  if (per_partner == TRUE &&
      per_level == FALSE) {

  d_partner <- get("d_partner", envir = .GlobalEnv)

  # For each partner

  for (i in seq_along(unique(inconsistency_report$partner_code))) {

   # Get partner_code, partner and partner_short

   partner_code <- unique(inconsistency_report$partner_code)[i]
   partner <- d_partner$description[which(d_partner$code == partner_code)]
   partner_short <- d_partner$desc_short[which(d_partner$code == partner_code)]

   # Select inconsistencies for the given partner

   inconsistency_report_partner <- inconsistency_report[
            which(inconsistency_report$partner == partner), ]
   possible_inconsistencies_partner <- possible_inconsistencies[
            which(possible_inconsistencies$rule_id %in%
                    unique(inconsistency_report_partner$rule_id)), ]

   # Save as Excel

   wb <- createWorkbook()
   addWorksheet(wb, "Inconsistency report")
   addWorksheet(wb, "Attribute catalogue")
   addWorksheet(wb, "Inconsistency catalogue")

   writeData(wb, 1, inconsistency_report_partner)
     addFilter(wb, 1, row = 1, cols = 1:ncol(inconsistency_report_partner))
     dataValidation(wb, 1, col = which(names(inconsistency_report_partner) ==
                                         "code_nfc_action_taken"),
                    rows = 2:(nrow(inconsistency_report_partner)+1),
                    type = "list",
                    value = '"1 - The reported value is extreme but correct, 2 - The correct value is resubmitted/the inconsistency is solved, 3 - The reported value is removed (e.g. resubmitted as NA), 4 - No (good) data are available for this parameter, 5 - Other"') # nolint
     freezePane(wb, 1, firstActiveRow = 2, firstActiveCol = 1)
   writeData(wb, 2, attribute_catalogue_pir)
    protectWorksheet(wb, 2, protect = TRUE)
   writeData(wb, 3, possible_inconsistencies_partner)
     protectWorksheet(wb, 3, protect = TRUE)
     addCreator(wb, "ICP Forests - FSCC")

   openxlsx::saveWorkbook(wb,
                          file = paste0(path, "/", download_date, "_",
                                        partner_code, "_inconsistency_report_",
                                        partner_short, ".xlsx"),
                          overwrite = TRUE)
  }
  }




# 6. Export per level and not per partner ----
# Level I versus Level II

  if (per_partner == TRUE &&
      per_level == FALSE) {

  LI <- c("s1_som", "s1_prf", "s1_pls", "s1_pfh", "s1_lqa",
          "y1_pl1", "y1_st1")

  LII <- c("so_som", "so_prf", "so_pls", "so_pfh", "so_lqa",
           "si_eve", "si_plt", "si_sta", "si_tco",
           "sw_swa", "sw_swc")

  # For each level

  for (i in 1:2) {

   # Retrieve the survey forms which are associated with the given level
  
   if (i == 1) { #LI
     level_survey_forms <- LI
     level_name <- "LI"
     }
   if (i == 2) { #LII
     level_survey_forms <- LII
     level_name <- "LII"
     }

  # Select inconsistencies for the given level

  inconsistency_report_level <- inconsistency_report[
    which(inconsistency_report$survey_form %in% level_survey_forms), ]
  possible_inconsistencies_level <- possible_inconsistencies[
    which(possible_inconsistencies$rule_id %in%
            unique(inconsistency_report_level$rule_id)), ]

  # Save as Excel

  wb <- createWorkbook()
  addWorksheet(wb, "Inconsistency report")
  addWorksheet(wb, "Attribute catalogue")
  addWorksheet(wb, "Inconsistency catalogue")

  writeData(wb, 1, inconsistency_report_level)
  addFilter(wb, 1, row = 1, cols = 1:ncol(inconsistency_report_level))
  dataValidation(wb, 1, col = which(names(inconsistency_report_level) ==
                                      "code_nfc_action_taken"),
                 rows = 2:(nrow(inconsistency_report_level)+1),
                 type = "list",
                 value = '"1 - The reported value is extreme but correct, 2 - The correct value is resubmitted/the inconsistency is solved, 3 - The reported value is removed (e.g. resubmitted as NA), 4 - No (good) data are available for this parameter, 5 - Other"') # nolint
  freezePane(wb, 1, firstActiveRow = 2, firstActiveCol = 1)
  writeData(wb, 2, attribute_catalogue_pir)
  protectWorksheet(wb, 2, protect = TRUE)
  writeData(wb, 3, possible_inconsistencies_level)
  protectWorksheet(wb, 3, protect = TRUE)
  addCreator(wb, "ICP Forests - FSCC")

  openxlsx::saveWorkbook(wb,
                         file = paste0(path, "/", download_date, "_",
                                       level_name,
                                       "_inconsistency_report.xlsx"),
                         overwrite = TRUE)
  }
  }





# 7. Export per level and per partner ----

  if (per_partner == TRUE &&
      per_level == TRUE) {

    LI <- c("s1_som", "s1_prf", "s1_pls", "s1_pfh", "s1_lqa",
            "y1_pl1", "y1_st1")

    LII <- c("so_som", "so_prf", "so_pls", "so_pfh", "so_lqa",
             "si_eve", "si_plt", "si_sta", "si_tco",
             "sw_swa", "sw_swc")

    d_partner <- get("d_partner", envir = .GlobalEnv)

    # For each level

    for (i in 1:2) {

      # Retrieve the survey forms which are associated with the given level
      if (i == 1) { #LI
        level_survey_forms <- LI
        level_name <- "LI"
        }
      if (i == 2) { #LII
        level_survey_forms <- LII
        level_name <- "LII"
        }

      # Create subfolders per level

      path_level <- paste0(path, "/", level_name)
      if (!file.exists(path_level)) {
        dir.create(path_level)
        }

      # Select inconsistencies for the given level

      inconsistency_report_level <- inconsistency_report[
        which(inconsistency_report$survey_form %in% level_survey_forms), ]

      # For each partner

      for (j in seq_along(unique(inconsistency_report_level$partner_code))) {

      # Get partner_code, partner and partner_short

      partner_code <- unique(inconsistency_report_level$partner_code)[j]
      partner <- d_partner$description[which(d_partner$code == partner_code)]
      partner_short <-
        d_partner$desc_short[which(d_partner$code == partner_code)]

      # Select inconsistencies for the given partner

      inconsistency_report_level_partner <- inconsistency_report_level[
        which(inconsistency_report_level$partner == partner), ]

      possible_inconsistencies_level_partner <- possible_inconsistencies[
        which(possible_inconsistencies$rule_id %in%
                unique(inconsistency_report_level_partner$rule_id)), ]

      # Save as Excel

      wb <- createWorkbook()
      addWorksheet(wb, "Inconsistency report")
      addWorksheet(wb, "Attribute catalogue")
      addWorksheet(wb, "Inconsistency catalogue")

      writeData(wb, 1, inconsistency_report_level_partner)
      addFilter(wb, 1, row = 1,
                cols = 1:ncol(inconsistency_report_level_partner))
      dataValidation(wb, 1,
                     col = which(names(inconsistency_report_level_partner) ==
                                   "code_nfc_action_taken"),
                     rows = 2:(nrow(inconsistency_report_level_partner)+1),
                     type = "list",
                     value = '"1 - The reported value is extreme but correct, 2 - The correct value is resubmitted/the inconsistency is solved, 3 - The reported value is removed (e.g. resubmitted as NA), 4 - No (good) data are available for this parameter, 5 - Other"') # nolint
      freezePane(wb, 1, firstActiveRow = 2, firstActiveCol = 1)
      writeData(wb, 2, attribute_catalogue_pir)
      protectWorksheet(wb, 2, protect = TRUE)
      writeData(wb, 3, possible_inconsistencies_level_partner)
      protectWorksheet(wb, 3, protect = TRUE)
      addCreator(wb, "ICP Forests - FSCC")

      openxlsx::saveWorkbook(wb,
                             file = paste0(path_level, "/", download_date, "_",
                                           level_name, "_", partner_code,
                                           "_inconsistency_report_",
                                           partner_short, ".xlsx"),
                             overwrite = TRUE)
      }
    }
    }

}
