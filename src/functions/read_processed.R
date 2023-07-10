
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
#' @param save_to_global Logical which indicates whether the output dataframes
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
                           save_to_global = FALSE) {

# Define vectors with survey forms and codes to read ----

  survey_forms_all <- c("so_som", "so_prf", "so_pls", "so_pfh", "so_lqa",
                        "s1_som", "s1_prf", "s1_pls", "s1_pfh", "s1_lqa",
                        "si_eve", "si_plt", "si_sta", "si_tco", "y1_pl1",
                        "y1_st1", "sw_swa", "sw_swc")

  list_data_tables <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                           s1 = c("som", "prf", "pls", "pfh", "lqa"),
                           si = c("eve", "plt", "sta", "tco"),
                           y1 = c("pl1", "st1"),
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





# Check if global environment already contains data frames from this list----

  if (save_to_global == TRUE) {

  # Check if each data frame exists and is of class "data.frame"
  survey_forms_existing <- data.frame(survey_form = NULL,
                                      change_date_google_drive = NULL)

  for (i in seq_along(survey_forms)) {
    if (exists(survey_forms[i]) &&
        is.data.frame(get(survey_forms[i]))) {

    survey_forms_existing <-
      rbind(survey_forms_existing,
            data.frame(survey_form = survey_forms[i],
                       change_date_google_drive =
                         unique(get(survey_forms[i])$change_date_google_drive)))

    }
  }

  # If any objects with the same names currently exist (from other
  # Google Drive change dates)

  if (nrow(survey_forms_existing > 0)) {

    # Notify user about survey forms currently in the global environment

  cat(paste0("The following survey forms (with date of the last Google ",
             "Drive update) currently exist in the Global Environment:\n"))
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

    df <- read.csv(paste0(path_name_survey, survey_forms_code[j], ".csv"),
                   sep = ";")

      # Add change date of Google Drive version of imported data
      # (to keep track of which version you are working on internally)

    df$change_date_google_drive <- change_date_gd

    # Assign survey form to global environment

    assign(survey_forms_code[j], df, envir = globalenv())

    cat(paste0("Survey form '",
               survey_forms_code[j],
               "' imported in Global Environment.\n"))
    }

    # Read coordinates data form

    if (paste0("coordinates_", survey_codes[i], ".csv") %in%
          list.files(path_name_survey)) {

      df <- read.csv(paste0(path_name_survey, "coordinates_",
                            survey_codes[i], ".csv"),
                     sep = ";")

      assign(paste0("coordinates_", survey_codes[i]),
             df, envir = globalenv())

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

      assign(paste0("data_availability_", survey_codes[i]),
             df, envir = globalenv())

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

    assign(list_additional[i],
           df, envir = globalenv())

    cat(paste0("Data form '",
               list_additional[i],
               "' imported in Global Environment.\n"))

  }
  }
}
}
}
