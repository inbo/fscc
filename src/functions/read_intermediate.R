
#' Read intermediate solid soil data
#'
#' Reads intermediate data files for specified survey forms and path.
#'
#' @param survey_forms Character vector of survey forms to read (either survey
#' codes, e.g. "so", or survey forms, e.g. "so_som"). Default is NULL,
#' which reads all solid soil-related survey forms.
#' @param path_name Path to the directory containing the intermediate data
#' files. Default is NULL, which reads the most recent path.
#' 
#' @return None
#'
#' @examples
#' # Read all solid soil-related survey forms
#' read_intermediate()
#'
#' # Read specific survey forms from a specific path
#' read_intermediate(c("so_som", "so_prf"), "./path/to/data/")

read_intermediate <- function(survey_forms = NULL, path_name = NULL) {


  survey_forms_all <- c("so_som", "so_prf", "so_pls", "so_pfh", "so_lqa",
                        "s1_som", "s1_prf", "s1_pls", "s1_pfh", "s1_lqa",
                        "si_eve", "si_plt", "si_sta", "si_tco", "y1_pl1",
                        "y1_st1", "sw_swa", "sw_swc")

  list_data_tables <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                           s1 = c("som", "prf", "pls", "pfh", "lqa"),
                           si = c("eve", "plt", "sta", "tco"),
                           y1 = c("pl1", "st1"),
                           sw = c("swa", "swc"))

  # If no "survey_forms" argument is provided,
  # read all solid soil-related survey forms (by default)

  if (is.null(survey_forms)) {
    survey_forms <- survey_forms_all
    }  

  # If "survey_forms" argument is provided, replace any survey codes (e.g. "so")
  # by the actual survey forms (e.g. "so_som", "so_prf"...)

  if (!is.null(survey_forms)) {

    survey_forms_extra <- NULL

    # Check for each of the reported characters whether they represent a code
    # of a survey (length = 1) or a survey form (length = 2)

    for (i in seq_along(survey_forms)) {

      if (length(unlist(strsplit(survey_forms[i], "_"))) == 1) {

        # Replace the code by the actual survey forms
        survey_forms_extra <- c(survey_forms_extra,
                                paste0(survey_forms[i], "_",
                                       list_data_tables[[which(
                                         names(list_data_tables) ==
                                           survey_forms[i])]]))

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


  # If no "path_name" argument is provided,
  # read from the most recent path (default)

  if (is.null(path_name)) {

    list_files_path <- list.files("./output/intermediate_data/")

    # Extract the dates from file names
    dates <- sub("^([0-9]+).*", "\\1", list_files_path)

    # Convert dates to actual dates
    dates <- as.Date(parsedate::parse_iso_8601(dates), format = "%Y%m%d")

    # Find the most recent date
    ind_most_recent_date <- which(dates == max(dates))

    # path name of most recent download date
    path_name <- paste0("./output/intermediate_data/",
                        list_files_path[ind_most_recent_date], "/")
    }

  # Determine download date of the given path

  vec_path <- unlist(strsplit(path_name, "/"))
  date <- unlist(strsplit(vec_path[length(vec_path)], "_"))[1]

  # Read all the required data

  for (i in seq_along(survey_codes)) {

    survey_forms_code <- grep(paste0("^", survey_codes[i]),
                              survey_forms, value = TRUE)

    path_name_survey <- paste0(path_name, date, "_",
                               survey_codes[i], "/")

    # Read each of the survey forms

    for (j in seq_along(survey_forms_code)) {
    df <- read.csv(paste0(path_name_survey, survey_forms_code[j], ".csv"),
                   sep = ";")
    assign(survey_forms_code[j], df, envir = globalenv())
    }

    # Read coordinates data form

    if (paste0("coordinates_", survey_codes[i], ".csv") %in%
          list.files(path_name_survey)) {

      df <- read.csv(paste0(path_name_survey, "coordinates_",
                            survey_codes[i], ".csv"),
                     sep = ";")
      assign(paste0("coordinates_", survey_codes[i]),
             df, envir = globalenv())
      }

    # Read data availability form

    if (paste0("data_availability_", survey_codes[i], ".csv") %in%
        list.files(path_name_survey)) {

      df <- read.csv(paste0(path_name_survey, "data_availability_",
                            survey_codes[i], ".csv"),
                     sep = ";")
      assign(paste0("data_availability_", survey_codes[i]),
             df, envir = globalenv())
    }
    }


  if (paste0(date, "_additional_data") %in% list.files(path_name)) {

    path_additional <- paste0(path_name, date, "_additional_data/")
    list_additional <- sub("\\.csv$", "", list.files(path_additional))

    for (i in list_additional) {
      df <- read.csv(paste0(path_additional,
                            list_additional[i], ".csv"), sep = ";")
      assign(list_additional[i],
             df, envir=globalenv())
      }
    }

}
