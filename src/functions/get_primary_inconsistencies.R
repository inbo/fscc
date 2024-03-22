
#' List inconsistencies related to primary key
#'
#' This function evaluates the consistencies in primary key data.
#'
#' @param code_survey Character string - Name of the survey form
#' (lower case and separated by '_', e.g. 'so_som') or survey (lower case,
#' e.g. 'so') to be evaluated
#' @param solve Logical - Indicates whether the function should rename
#' "code_layer" in 's1_som' in case of ambiguous (non-unique) code_layers
#' (if TRUE). Default is FALSE.   # This argument is only relevant for "som"
#' survey forms
#' @param save_to_env Logical which indicates whether the output dataframes
#' can be saved to the environment and override any existing objects
#' with the same name. Default is FALSE.
#'
#' @details
#' For any survey (form) that contains "survey_year", "code_plot",
#' "horizon_master", "code_layer", "profile_pit_id",
#' so survey forms within "s1", "so", "sw", but also "y1", "si"
#'
#' Outputs - This function generates and returns:
#' - an inconsistency report ("list_primary_inconsistencies")
#' - in case of non-unique code_layers (i.e. possibly for "s1_som"
#'   and "so_som") (FSCC_47): the original data form, but with renamed
#'   "code_layer"
#'
#' Which kind of inconsistencies are identified in this function?
#' FSCC_47: Are all code_layers unique within a given profile?
#'          (not the case for some plots in 's1_som' and 'so_som')
#' FSCC_34: Is the survey_year reported?
#' FSCC_35: Is the survey_year within a possible range?
#' FSCC_36: Is the code_plot reported?
#' FSCC_33: Is the code_layer or horizon_master reported?
#' FSCC_37: Is the profile_pit_id reported?
#'
#' How are the survey forms exactly updated in this function
#' (if "solve" == TRUE)?
#' - "som" survey forms:
#'   If there are non-unique primary keys due to multiple layers with the same
#'   "code_layer" within a given profile
#'   (i.e. if there is a duplicated unique_layer_repetition
#'   for which either the layer limits are different, or the layer limits are NA
#'   while column "origin" is the same or non-existent):
#'   > Copy the original column "code_layer" to a new column
#'     "code_layer_orig"
#'   > Change the "code_layer" column for these ambiguous layers to
#'     (first letters, e.g. "M", "H", "OL") + abs(layer_limit_superior) +
#'     abs(layer_limit_inferior), e.g. "M5060", "M8590", "OL74" etc
#'   > Update unique identifiers that include "code_layer",
#'     i.e. "unique_survey_layer" and "unique_layer_repetition"
#'   Note: this is only for internal use (to avoid conflicts in primary key),
#'   and needs to be harmonised according to the manual in the future
#'
#' WARNING - This function may not be optimally efficient and may ideally
#' require refactoring for better performance.
#'
#' @examples
#' get_primary_inconsistencies("s1", solve = TRUE)
#'

get_primary_inconsistencies <- function(code_survey,
                                        solve = FALSE,
                                        save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(paste0(" \nSolve primary inconsistencies in '", code_survey, "'\n"))

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

# Identify the survey (form)s to be evaluated ----

  # Create a list with names of the different survey forms per survey

  list_data_tables <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                           s1 = c("som", "prf", "pls", "pfh", "lqa"),
                           si = c("eve", "plt", "sta", "tco"),
                           y1 = c("pl1", "st1"),
                           sw = c("swa", "swc"),
                           ss = c("lqa", "pss", "ssm"))

  # If one survey from (instead of a complete survey) is given as input:
  # Change "list_data_tables" so that the list of the corresponding survey code
  # contains just one survey form (the one given as input)

  survey_form_as_input <- code_survey

  # if the input is a survey form (e.g. s1_pfh) rather than a survey (e.g. s1)

  if (length(unlist(strsplit(code_survey, "_"))) > 1) {

    # Rename "survey_form" and "code_survey"
    survey_form <- code_survey
    code_survey <- unlist(strsplit(survey_form, "_"))[1]

    # Split the input "survey_form"
    string_split <- strsplit(survey_form, "_", fixed = TRUE)[[1]]

    # Rename the list of the corresponding "code_survey" with the name of
    # the input survey form, without the initial "code_survey"
    # e.g. "pfh" for "s1_pfh"; "pfh_outtake" for "s1_pfh_outtake"

    list_data_tables[[which(names(list_data_tables) == code_survey)]] <-
      paste(string_split[-1], collapse = "_")
    }



  # The intention is to create a list_primary_inconsistencies of the following
  # format to store inconsistencies found in the survey data:

    list_primary_inconsistencies <-
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


# Evaluate inconsistencies per survey form for the given code_survey ----
  # (i.e. for one survey form if the name of a survey form is given as input)

  for (i in seq_along(list_data_tables[[which(names(list_data_tables) ==
                                             code_survey)]])) {

    # Retrieve the survey data

    survey_form <- paste0(code_survey, "_",
                          list_data_tables[[which(names(list_data_tables) ==
                                                    code_survey)]][i])

    df <- get_env(survey_form)


# FSCC_47: Check for "som" data forms whether code_layer is unique ----
    # within a given profile
    # This is possibly not the case for some records:

    # in s1_som: e.g. multiple "M48", "Mxx", "Hxx" layers
    # (8 plots in Sweden; 1 plot in Finland)

    # in so_som: e.g. multiple "OL" layers (1 plot in Slovakia)


    # Check if the survey form is "som"

    if (unlist(strsplit(survey_form, "_"))[2] == "som") {

    # If "solve" is TRUE,
    # duplicate the "code_layer" column (to keep the original "code_layer")
    # Only the "code_layer" column without "_original" can be altered

    if (solve == TRUE) {
      df$code_layer_orig <- as.character(df$code_layer)
    df$code_layer <- as.character(df$code_layer)
    }

    if ("code_layer_orig" %in% names(df)) {
      col_code_layer <- df$code_layer_orig
      } else {
        col_code_layer <- df$code_layer
        }

    # Determine where unique_layer_repetition is not unique

    ind_duplicated <- which(duplicated(df$unique_layer_repetition))

    if (!identical(ind_duplicated, integer(0))) {

    layers_duplicated <- unique(df$unique_layer_repetition[ind_duplicated])

    # Set up a progress bar to track processing

    if (!isTRUE(getOption("knitr.in.progress"))) {
    progress_bar <- txtProgressBar(min = 0, max = length(layers_duplicated),
                                   style = 3)
    }

    # For each of the duplicate unique_layer_repetition

    for (j in seq_along(layers_duplicated)) {

      # Determine indices of j in layers_duplicated

      j_dupl <- which(df$unique_layer_repetition == layers_duplicated[j])

      # If there are different values in layer_limit_superior or
      # layer_limit_inferior, or if they are all NA

      if (((length(unique(df$layer_limit_superior[j_dupl])) ==
            length(j_dupl)) ||
          (length(unique(df$layer_limit_inferior[j_dupl])) ==
           length(j_dupl)) ||
          (all(is.na(df$layer_limit_superior[j_dupl])) &&
           all(is.na(df$layer_limit_inferior[j_dupl])) &&
           (!"origin" %in% names(df) ||
            (length(unique(df$origin[j_dupl])) == 1))))) {

        # Then it makes sense to rename "code_layer" in the working data
        # (internally within FSCC) to ensure its uniqueness if "solve" == TRUE

        # - If layer limits are known for a layer: rename "code_layer" to
        #   [first letter, i.e. "M" or "H"] + [layer_limit_superior] +
        #   [layer_limit_inferior], e.g. "M5060", "M8590" etc
        # - Else, add unique numbers to the original code, e.g. "Mxx1", "Mxx2"

        if (solve == TRUE) {

          # Determine the first letter(s) of the new code_layer

          initial_letters <- gsub("[0-9x]+$", "",
                                  unique(as.character(
                                    df$code_layer_orig[j_dupl])))

          # If the layer limits are known:
          # (only if not forest floor)

          if ((all(!is.na(df$layer_limit_superior[j_dupl])) &&
               all(!is.na(df$layer_limit_inferior[j_dupl]))) &&
              (!identical(unique(as.character(df$layer_type[j_dupl])),
                          "forest_floor"))) {

          df$code_layer[j_dupl] <-
            paste0(initial_letters,
              abs(df$layer_limit_superior[j_dupl]),
              abs(df$layer_limit_inferior[j_dupl]))
          } else {

          # If the layer limits are not known or if it concerns forest floor:

          df$code_layer[j_dupl] <-
            paste0(initial_letters,
              seq_along(j_dupl))
          }

          # Update the columns "unique_survey_layer" and
          # "unique_layer_repetition" too

          df[j_dupl,] <- df[j_dupl,] %>%
            rowwise() %>%
            mutate(
              unique_survey_layer = paste0(
                code_country, "_",
                survey_year, "_",
                code_plot, "_",
                code_layer))

          df[j_dupl,] <- df[j_dupl,] %>%
            rowwise() %>%
            mutate(
              unique_layer_repetition = paste0(
                code_country, "_",
                survey_year, "_",
                code_plot, "_",
                code_layer, "_",
                repetition))

          df[j_dupl,] <- df[j_dupl,] %>%
            rowwise() %>%
            mutate(
              unique_layer = paste0(
                code_country, "_",
                code_plot, "_",
                code_layer))
          }


        # Store information about the inconsistency in
        # "list_primary_inconsistencies"

        non_duplicated_error_type_inconsistency <-
            c(TRUE, rep(FALSE, (length(j_dupl) - 1)))

        rule_id <- "FSCC_47"
        inconsistency_reason <-
          inconsistency_catalogue$inconsistency_reason[
            which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <-
          inconsistency_catalogue$inconsistency_type[
            which(inconsistency_catalogue$rule_id == rule_id)]

        list_primary_inconsistencies <- rbind(
          list_primary_inconsistencies,
          data.frame(survey_form = (rep(survey_form, length(j_dupl))),
                     partner = df$partner[j_dupl],
                     partner_code = df$partner_code[j_dupl],
                     country = df$country[j_dupl],
                     code_country = df$code_country[j_dupl],
                     survey_year = df$survey_year[j_dupl],
                     code_plot = df$code_plot[j_dupl],
                     plot_id = df$plot_id[j_dupl],
                     code_layer_horizon_master = col_code_layer[j_dupl],
                     repetition_profile_pit_id = df$repetition[j_dupl],
                     code_line = df$code_line[j_dupl],
                     parameter = (rep("code_layer", length(j_dupl))),
                     parameter_unit = (rep(NA, length(j_dupl))),
                     parameter_value = col_code_layer[j_dupl],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       non_duplicated_error_type_inconsistency,
                     change_date = df$change_date[j_dupl],
                     download_date = rep(download_date_pir,
                                         length(j_dupl))))


      }

      # Update the progress bar

      if (!isTRUE(getOption("knitr.in.progress"))) {
      setTxtProgressBar(progress_bar, j)
      }
    }



    # If there are two OL layers in Slovakia: plot_id 54_208 (repetition 1)
    # And the same unique plot survey contains repetitions with only one OL
    # layer:
    # Rename the "common" OL layer of this unique plot survey (i.e. the layer
    # with layer_limit_inferior -1) to "OL"

    vec <- which(df$unique_survey_repetition == "54_2007_208_1" &
                   df$layer_type == "forest_floor" &
                   df$layer_limit_inferior == -1 &
                   df$code_layer == "OL2")

    vec_other <- which(df$unique_survey == "54_2007_208" &
                         df$unique_survey_repetition != "54_2007_208_1" &
                         df$layer_type == "forest_floor" &
                         df$code_layer == "OL" &
                         df$layer_limit_inferior == -1)

    if (!identical(vec, integer(0)) &&
        !identical(vec_other, integer(0))) {

      if (solve == TRUE) {

        df$code_layer[vec] <- "OL"
        df$unique_survey_layer[vec] <-
          paste0(df[vec, which(names(df) == "code_country")], "_",
                 df[vec, which(names(df) == "survey_year")], "_",
                 df[vec, which(names(df) == "code_plot")], "_",
                 df[vec, which(names(df) == "code_layer")])
        df$unique_layer_repetition[vec] <-
          paste0(df[vec, which(names(df) == "code_country")], "_",
                 df[vec, which(names(df) == "survey_year")], "_",
                 df[vec, which(names(df) == "code_plot")], "_",
                 df[vec, which(names(df) == "code_layer")], "_",
                 df[vec, which(names(df) == "repetition")])
        df$unique_layer[vec] <-
          paste0(df[vec, which(names(df) == "code_country")], "_",
                 df[vec, which(names(df) == "code_plot")], "_",
                 df[vec, which(names(df) == "code_layer")])
      }
    }



    if (!isTRUE(getOption("knitr.in.progress"))) {
    close(progress_bar)
    }
    }

    # Remove the column "code_layer_orig"
    # if the survey form does not contain any ambiguous layers
    # (e.g. for "so_som")

    if (all(df$code_layer == df$code_layer_orig)) {
      df <- df[, -which(names(df) == "code_layer_orig")]
      }

    }








    # Check for the presence of primary key information ----

    # Store the column indices of columns with primary key information

    vec_primary <- which(names(df) %in%
                             c("survey_year", "code_plot", "horizon_master",
                               "code_layer", "profile_pit_id"))

    column_code_layer_horizon_master <- rep(NA, nrow(df))
    column_repetition_profile_pit_id <- rep(NA, nrow(df))

    if ("code_layer_orig" %in% names(df)) {
      column_code_layer_horizon_master <-
        pull(df[, which(names(df) == "code_layer_orig")])
      } else
    if ("code_layer" %in% names(df)) {
      column_code_layer_horizon_master <-
        pull(df[, which(names(df) == "code_layer")])
      } else
    if ("horizon_master" %in% names(df)) {
      column_code_layer_horizon_master <-
        pull(df[, which(names(df) == "horizon_master")])
      }

    if ("repetition" %in% names(df)) {
      column_repetition_profile_pit_id <-
        pull(df[, which(names(df) == "repetition")])
      } else
    if ("profile_pit_id" %in% names(df)) {
      column_repetition_profile_pit_id <-
        pull(df[, which(names(df) == "profile_pit_id")])
      }



    # Evaluate each of the columns with primary key information

    for (j in vec_primary) {

      # FSCC_34
      # survey_year

      if (names(df)[j] == "survey_year") {

        # Identify row indices for which this information is lacking

      if (any(is.na(df[, j]) |
            (df[, j] == ""))) {
        vec_inconsistency <- which(is.na(df[, j]) | (df[, j] == ""))

        # Store information about the inconsistency in
        # "list_primary_inconsistencies"

        rule_id <- "FSCC_34"
        inconsistency_reason <-
          inconsistency_catalogue$inconsistency_reason[
            which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <-
          inconsistency_catalogue$inconsistency_type[
            which(inconsistency_catalogue$rule_id == rule_id)]

        list_primary_inconsistencies <- rbind(
          list_primary_inconsistencies,
          data.frame(survey_form = (rep(survey_form,
                                        length(vec_inconsistency))),
                     partner = df$partner[vec_inconsistency],
                     partner_code = df$partner_code[vec_inconsistency],
                     country = df$country[vec_inconsistency],
                     code_country = df$code_country[vec_inconsistency],
                     survey_year = df$survey_year[vec_inconsistency],
                     code_plot = df$code_plot[vec_inconsistency],
                     plot_id = df$plot_id[vec_inconsistency],
                     code_layer_horizon_master =
                       column_code_layer_horizon_master[vec_inconsistency],
                     repetition_profile_pit_id =
                       column_repetition_profile_pit_id[vec_inconsistency],
                     code_line = df$code_line[vec_inconsistency],
                     parameter = (rep("survey_year",
                                      length(vec_inconsistency))),
                     parameter_unit = (rep(NA, length(vec_inconsistency))),
                     parameter_value = df$survey_year[vec_inconsistency],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(vec_inconsistency)),
                     change_date = df$change_date[vec_inconsistency],
                     download_date = rep(download_date_pir,
                                         length(vec_inconsistency))))
        }


        # FSCC_35: "survey_year" outside possible range ----

        # Determine the minimum and maximum possible "survey_year"

        year_min <- 1985
        year_max <- as.numeric(format(Sys.Date(), "%Y"))

        # Within row indices with known survey_year:
        # identify row indices for which the survey_year is outside the possible
        # range of survey years (e.g. 1900)

        vec_nonempty <- which(!is.na(df[, j]))
        if (any((df[vec_nonempty, j] < year_min) |
                (df[vec_nonempty, j] > year_max))) {
          vec_inconsistency <- which((df[vec_nonempty, j] < year_min) |
                                       (df[vec_nonempty, j] > year_max))

          # Store information about the inconsistency in
          # "list_primary_inconsistencies"

          rule_id <- "FSCC_35"
          inconsistency_reason <-
            inconsistency_catalogue$inconsistency_reason[
              which(inconsistency_catalogue$rule_id == rule_id)]
          inconsistency_type <-
            inconsistency_catalogue$inconsistency_type[
              which(inconsistency_catalogue$rule_id == rule_id)]

          list_primary_inconsistencies <- rbind(
            list_primary_inconsistencies,
            data.frame(survey_form = (rep(survey_form,
                                          length(vec_inconsistency))),
                       partner = df$partner[vec_inconsistency],
                       partner_code = df$partner_code[vec_inconsistency],
                       country = df$country[vec_inconsistency],
                       code_country = df$code_country[vec_inconsistency],
                       survey_year = df$survey_year[vec_inconsistency],
                       code_plot = df$code_plot[vec_inconsistency],
                       plot_id = df$plot_id[vec_inconsistency],
                       code_layer_horizon_master =
                         column_code_layer_horizon_master[vec_inconsistency],
                       repetition_profile_pit_id =
                         column_repetition_profile_pit_id[vec_inconsistency],
                       code_line = df$code_line[vec_inconsistency],
                       parameter = (rep("survey_year",
                                        length(vec_inconsistency))),
                       parameter_unit = (rep(NA, length(vec_inconsistency))),
                       parameter_value = df$survey_year[vec_inconsistency],
                       inconsistency_reason = inconsistency_reason,
                       inconsistency_type = inconsistency_type,
                       rule_id = rule_id,
                       non_duplicated_error_type_per_record =
                         rep(TRUE, length(vec_inconsistency)),
                       change_date = df$change_date[vec_inconsistency],
                       download_date = rep(download_date_pir,
                                           length(vec_inconsistency))))
        }
        }


      ## FSCC_36: this record does not have a 'code_plot' ----

      if (names(df)[j] == "code_plot") {

      # Identify row indices for which this information is lacking

      if (any(is.na(df[, j]) |
                (df[, j] == ""))) {
        vec_inconsistency <- which(is.na(df[, j]) | (df[, j] == ""))

        # Store information about the inconsistency in
        # "list_primary_inconsistencies"

        rule_id <- "FSCC_36"
        inconsistency_reason <-
          inconsistency_catalogue$inconsistency_reason[
            which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <-
          inconsistency_catalogue$inconsistency_type[
            which(inconsistency_catalogue$rule_id == rule_id)]

        list_primary_inconsistencies <- rbind(
          list_primary_inconsistencies,
          data.frame(survey_form = (rep(survey_form,
                                        length(vec_inconsistency))),
                     partner = df$partner[vec_inconsistency],
                     partner_code = df$partner_code[vec_inconsistency],
                     country = df$country[vec_inconsistency],
                     code_country = df$code_country[vec_inconsistency],
                     survey_year = df$survey_year[vec_inconsistency],
                     code_plot = df$code_plot[vec_inconsistency],
                     plot_id = df$plot_id[vec_inconsistency],
                     code_layer_horizon_master =
                       column_code_layer_horizon_master[vec_inconsistency],
                     repetition_profile_pit_id =
                       column_repetition_profile_pit_id[vec_inconsistency],
                     code_line = df$code_line[vec_inconsistency],
                     parameter = (rep("code_plot", length(vec_inconsistency))),
                     parameter_unit = (rep(NA, length(vec_inconsistency))),
                     parameter_value = df$code_plot[vec_inconsistency],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(vec_inconsistency)),
                     change_date = df$change_date[vec_inconsistency],
                     download_date = rep(download_date_pir,
                                         length(vec_inconsistency))))
      }
      }


      # FSCC_33: missing "horizon_master" or "code_layer" ----
      # horizon_master

      if (names(df)[j] == "horizon_master") {

      # Identify row indices for which this information is lacking

      if (any(is.na(df[, j]) |
                (df[, j] == ""))) {
        vec_inconsistency <- which(is.na(df[, j]) | (df[, j] == ""))

        # Store information about the inconsistency in
        # "list_primary_inconsistencies"

        rule_id <- "FSCC_33"
        inconsistency_reason <-
          inconsistency_catalogue$inconsistency_reason[
            which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <-
          inconsistency_catalogue$inconsistency_type[
            which(inconsistency_catalogue$rule_id == rule_id)]

        list_primary_inconsistencies <- rbind(
          list_primary_inconsistencies,
          data.frame(survey_form = (rep(survey_form,
                                        length(vec_inconsistency))),
                     partner = df$partner[vec_inconsistency],
                     partner_code = df$partner_code[vec_inconsistency],
                     country = df$country[vec_inconsistency],
                     code_country = df$code_country[vec_inconsistency],
                     survey_year = df$survey_year[vec_inconsistency],
                     code_plot = df$code_plot[vec_inconsistency],
                     plot_id = df$plot_id[vec_inconsistency],
                     code_layer_horizon_master =
                       column_code_layer_horizon_master[vec_inconsistency],
                     repetition_profile_pit_id =
                       column_repetition_profile_pit_id[vec_inconsistency],
                     code_line = df$code_line[vec_inconsistency],
                     parameter = (rep("horizon_master",
                                      length(vec_inconsistency))),
                     parameter_unit = (rep(NA, length(vec_inconsistency))),
                     parameter_value = df$horizon_master[vec_inconsistency],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(vec_inconsistency)),
                     change_date = df$change_date[vec_inconsistency],
                     download_date = rep(download_date_pir,
                                         length(vec_inconsistency))))

        if (solve == TRUE) {

          if (survey_form == "so_pfh") {

          # Complete missing Serbian horizon_master

          df <- df %>%
            mutate(horizon_master = ifelse(code_country == 67 &
                                         code_plot == 2 &
                                         survey_year == 2010 &
                                         (is.na(horizon_master) |
                                            horizon_master == ""),
                                       "O",
                                       horizon_master),
                   unique_survey_layer = ifelse(code_country == 67 &
                                                  code_plot == 2 &
                                                  survey_year == 2010 &
                                                  layer_type == "forest_floor",
                                                paste0(code_country, "_",
                                                       survey_year, "_",
                                                       code_plot, "_",
                                                       horizon_master),
                                                unique_survey_layer),
                   unique_layer_repetition = ifelse(code_country == 67 &
                                                      code_plot == 2 &
                                                      survey_year == 2010 &
                                                      layer_type ==
                                                         "forest_floor",
                                                    paste0(code_country, "_",
                                                           survey_year, "_",
                                                           code_plot, "_",
                                                           horizon_master, "_",
                                                           profile_pit_id),
                                                    unique_layer_repetition),
                   unique_layer = ifelse(code_country == 67 &
                                          code_plot == 2 &
                                          survey_year == 2010 &
                                          layer_type == "forest_floor",
                                        paste0(code_country, "_",
                                               code_plot, "_",
                                               horizon_master),
                                        unique_layer))
          }



          if (survey_form == "s1_pfh") {

            # Missing horizon_master in Hungary
            # (Manually determined)

            # Plot 51_1

            layers_to_check <- df %>%
              filter(plot_id == "51_1") %>%
              filter(survey_year == 2006) %>%
              filter(profile_pit_id == 1)

            assertthat::assert_that(
              all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")))

            if (all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == ""))) {

              df <- df %>%
                mutate(horizon_master =
                         ifelse(unique_survey_profile ==
                                  unique(layers_to_check$unique_survey_profile),
                                case_when(
                                  horizon_limit_up == 0 ~ "A",
                                  horizon_limit_up == 10 ~ "AB",
                                  horizon_limit_up %in% c(30, 60) ~ "B",
                                  horizon_limit_up == 90 ~ "BC",
                                  horizon_limit_up == 125 ~ "C",
                                  TRUE ~ horizon_master),
                                horizon_master),
                       unique_survey_layer =
                         ifelse(unique_survey_profile ==
                                  unique(layers_to_check$unique_survey_profile),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_survey_layer),
                       unique_layer_repetition =
                         ifelse(unique_survey_profile ==
                                  unique(layers_to_check$unique_survey_profile),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master, "_",
                                       profile_pit_id),
                                unique_layer_repetition),
                       unique_layer =
                         ifelse(unique_survey_profile ==
                                  unique(layers_to_check$unique_survey_profile),
                                paste0(code_country, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_layer)) %>%
                mutate(layer_type =
                         ifelse(unique_survey_profile ==
                                  unique(layers_to_check$unique_survey_profile),
                                "mineral",
                                .data$layer_type))
            }






            # Plot 51_61 and 51_364

            layers_to_check <- df %>%
              filter(plot_id %in% c("51_61", "51_364")) %>%
              filter(survey_year == 2006) %>%
              filter(profile_pit_id == 1)

            layers_fine <- layers_to_check %>%
              filter(horizon_limit_up >= 0)

            layers_to_check <- layers_to_check %>%
              filter(horizon_limit_up < 0)

            assertthat::assert_that(
              all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                      (layers_fine$horizon_master != "")))

            if (all(is.na(layers_to_check$horizon_master) |
                     (layers_to_check$horizon_master == "")) &&
                 all(!is.na(layers_fine$horizon_master) &
                     (layers_fine$horizon_master != ""))) {

              df <- df %>%
                mutate(horizon_master =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                case_when(
                                  horizon_limit_up < 0 ~ "O",
                                  TRUE ~ horizon_master),
                                .data$horizon_master),
                       unique_survey_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_survey_layer),
                       unique_layer_repetition =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master, "_",
                                       profile_pit_id),
                                unique_layer_repetition),
                       unique_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_layer)) %>%
                mutate(layer_type =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line) &
                                horizon_limit_up < 0,
                                "forest_floor",
                                .data$layer_type))
            }


            # Plot 51_83

            layers_to_check <- df %>%
              filter(plot_id %in% c("51_83")) %>%
              filter(survey_year == 2006) %>%
              filter(profile_pit_id == 1)

            layers_fine <- layers_to_check %>%
              filter(horizon_limit_up <= 120)

            layers_to_check <- layers_to_check %>%
              filter(horizon_limit_up > 120)

            assertthat::assert_that(
              all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                      (layers_fine$horizon_master != "")))

            if (all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                    (layers_fine$horizon_master != ""))) {

              df <- df %>%
                mutate(horizon_master =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                case_when(
                                  horizon_limit_up > 120 ~ "C",
                                  TRUE ~ horizon_master),
                                .data$horizon_master),
                       unique_survey_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_survey_layer),
                       unique_layer_repetition =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master, "_",
                                       profile_pit_id),
                                unique_layer_repetition),
                       unique_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_layer)) %>%
                mutate(layer_type =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line) &
                                  horizon_limit_up > 120,
                                "mineral",
                                .data$layer_type))
            }



            # Plot 51_140

            layers_to_check <- df %>%
              filter(plot_id %in% c("51_140")) %>%
              filter(survey_year == 2006) %>%
              filter(profile_pit_id == 1)

            layers_fine <- layers_to_check %>%
              filter(horizon_limit_up <= 15)

            layers_to_check <- layers_to_check %>%
              filter(horizon_limit_up > 15)

            assertthat::assert_that(
              all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                      (layers_fine$horizon_master != "")))

            if (all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                    (layers_fine$horizon_master != ""))) {

              df <- df %>%
                mutate(horizon_master =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                case_when(
                                  horizon_limit_up == 40 ~ "B",
                                  horizon_limit_up == 62 ~ "B",
                                  horizon_limit_up == 84 ~ "BC",
                                  horizon_limit_up == 108 ~ "C",
                                  horizon_limit_up == 135 ~ "C",
                                  TRUE ~ horizon_master),
                                .data$horizon_master),
                       unique_survey_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_survey_layer),
                       unique_layer_repetition =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master, "_",
                                       profile_pit_id),
                                unique_layer_repetition),
                       unique_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_layer)) %>%
                mutate(layer_type =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line) &
                                  horizon_limit_up > 15,
                                "mineral",
                                .data$layer_type))
            }



            # Plot 51_252

            layers_to_check <- df %>%
              filter(plot_id %in% c("51_252")) %>%
              filter(survey_year == 2006) %>%
              filter(profile_pit_id == 1)

            layers_fine <- layers_to_check %>%
              filter(horizon_limit_up >= 185)

            layers_to_check <- layers_to_check %>%
              filter(horizon_limit_up < 185)

            assertthat::assert_that(
              all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                      (layers_fine$horizon_master != "")))

            if (all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                    (layers_fine$horizon_master != ""))) {

              df <- df %>%
                mutate(horizon_master =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                case_when(
                                  horizon_limit_up == 0 ~ "A",
                                  horizon_limit_up == 20 ~ "B",
                                  horizon_limit_up == 95 ~ "B",
                                  horizon_limit_up == 140 ~ "B",
                                  TRUE ~ horizon_master),
                                .data$horizon_master),
                       unique_survey_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_survey_layer),
                       unique_layer_repetition =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master, "_",
                                       profile_pit_id),
                                unique_layer_repetition),
                       unique_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_layer)) %>%
                mutate(layer_type =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line) &
                                  horizon_limit_up < 185 &
                                  horizon_limit_up >= 0,
                                "mineral",
                                .data$layer_type))
            }





            # Plot 51_287

            layers_to_check <- df %>%
              filter(plot_id %in% c("51_287")) %>%
              filter(survey_year == 2006) %>%
              filter(profile_pit_id == 1)

            layers_fine <- layers_to_check %>%
              filter(horizon_limit_up >= 18)

            layers_to_check <- layers_to_check %>%
              filter(horizon_limit_up < 18)

            assertthat::assert_that(
              all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                      (layers_fine$horizon_master != "")))

            if (all(is.na(layers_to_check$horizon_master) |
                    (layers_to_check$horizon_master == "")) &&
                all(!is.na(layers_fine$horizon_master) &
                    (layers_fine$horizon_master != ""))) {

              df <- df %>%
                mutate(horizon_master =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                case_when(
                                  horizon_limit_up < 18 ~ "A",
                                  TRUE ~ horizon_master),
                                .data$horizon_master),
                       unique_survey_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_survey_layer),
                       unique_layer_repetition =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       survey_year, "_",
                                       code_plot, "_",
                                       horizon_master, "_",
                                       profile_pit_id),
                                unique_layer_repetition),
                       unique_layer =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line),
                                paste0(code_country, "_",
                                       code_plot, "_",
                                       horizon_master),
                                unique_layer)) %>%
                mutate(layer_type =
                         ifelse(code_line %in%
                                  pull(layers_to_check, code_line) &
                                  horizon_limit_up < 18 &
                                  horizon_limit_up >= 0,
                                "mineral",
                                .data$layer_type))
            }



          } # End of s1_pfh

          # Assert that all horizon_masters/code_layers and layer_types
          # are known

          assertthat::assert_that(
            all(!is.na(df$horizon_master) & (df$horizon_master != "")),
            msg = paste0("In survey_form '", survey_form,
                         "', not all horizon_masters are known.")
          )

          assertthat::assert_that(
            all(!is.na(df$layer_type)),
            msg = paste0("In survey_form '", survey_form,
                         "', not all layer_types are known.")
          )

        }

      }

      }


      if (names(df)[j] == "code_layer") {

        # Evaluate the presence of "code_layer_orig" if "code_layer" has
        # been renamed (in case of ambiguous code_layer)

      if ("code_layer_orig" %in% names(df)) {
        col <- which(names(df) == "code_layer_orig")
        } else {
        col <- j
        }

        # Identify row indices for which this information is lacking

        if (any(is.na(df[, col]) |
                (df[, col] == ""))) {
        vec_inconsistency <- which(is.na(df[, col]) | (df[, col] == ""))

        # Store information about the inconsistency in
        # "list_primary_inconsistencies"

        rule_id <- "FSCC_33"
        inconsistency_reason <-
          inconsistency_catalogue$inconsistency_reason[
            which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <-
          inconsistency_catalogue$inconsistency_type[
            which(inconsistency_catalogue$rule_id == rule_id)]

        list_primary_inconsistencies <- rbind(
          list_primary_inconsistencies,
          data.frame(survey_form = (rep(survey_form,
                                        length(vec_inconsistency))),
                     partner = df$partner[vec_inconsistency],
                     partner_code = df$partner_code[vec_inconsistency],
                     country = df$country[vec_inconsistency],
                     code_country = df$code_country[vec_inconsistency],
                     survey_year = df$survey_year[vec_inconsistency],
                     code_plot = df$code_plot[vec_inconsistency],
                     plot_id = df$plot_id[vec_inconsistency],
                     code_layer_horizon_master =
                       column_code_layer_horizon_master[vec_inconsistency],
                     repetition_profile_pit_id =
                       column_repetition_profile_pit_id[vec_inconsistency],
                     code_line = df$code_line[vec_inconsistency],
                     parameter = (rep("code_layer", length(vec_inconsistency))),
                     parameter_unit = (rep(NA, length(vec_inconsistency))),
                     parameter_value =
                       column_code_layer_horizon_master[vec_inconsistency],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(vec_inconsistency)),
                     change_date = df$change_date[vec_inconsistency],
                     download_date = rep(download_date_pir,
                                         length(vec_inconsistency))))


        if (solve == TRUE) {

          # Special case: Latvian records without code_layer
          # There are two Latvian records without code_layer, layer_limits,
          # and with only one organic_carbon_total value
          # Assumption: since the other records of the same
          # unique_survey_repetition are M01, M12, M24, M48, and
          # organic_layer_weight is reported, we will assume that these are
          # forest floor layers.
          # Because of the organic_layer_weight information, these records
          # are still valuable for C stock calculations.
          # Deriving which of the records is on top
          # and which is below can only happen based on
          # analysis of the organic_layer_weight in so_som profiles
          # with at least two forest floor layers. This teaches us that
          # it is the most likely that the inferior layer usually has a
          # higher organic_layer_weight than the superior layer.
          # As such, we will name the code_layer of these records so that
          # they will be sorted accordingly.
          # This also matches with the values in the columns "code_line" and
          # "line_nr".

          vec_ff <- which(df$plot_id == "64_5" &
                            df$survey_year == 2004 &
                            df$layer_type == "forest_floor" &
                            is.na(df$code_layer))

          if (!identical(vec_ff, integer(0))) {

                ind_superior <-
                  vec_ff[which(df$organic_layer_weight[vec_ff] ==
                                 min(df$organic_layer_weight[vec_ff]))]

                ind_inferior <-
                  vec_ff[which(df$organic_layer_weight[vec_ff] ==
                                 max(df$organic_layer_weight[vec_ff]))]

                df$code_layer[ind_superior] <- "OL"
                df$code_layer[ind_inferior] <- "OFH"

                df$unique_survey_layer[vec_ff] <-
                  paste0(df$code_country[vec_ff], "_",
                         df$survey_year[vec_ff], "_",
                         df$code_plot[vec_ff], "_",
                         df$code_layer[vec_ff])

                df$unique_layer_repetition[vec_ff] <-
                  paste0(df$code_country[vec_ff], "_",
                         df$survey_year[vec_ff], "_",
                         df$code_plot[vec_ff], "_",
                         df$code_layer[vec_ff], "_",
                         df$repetition[vec_ff])

                df$unique_layer[vec_ff] <-
                  paste0(df$code_country[vec_ff], "_",
                         df$code_plot[vec_ff], "_",
                         df$code_layer[vec_ff])

          }


          # Assert that all horizon_masters/code_layers and layer_types
          # are known

          assertthat::assert_that(
            all(!is.na(df$code_layer) & (df$code_layer != "")),
            msg = paste0("In survey_form '", survey_form,
                         "', not all code_layers are known.")
          )

          assertthat::assert_that(
            all(!is.na(df$layer_type)),
            msg = paste0("In survey_form '", survey_form,
                         "', not all layer_types are known.")
          )

        }
        }
        }



      # FSCC_37: missing "profile_pit_id" ----


      if (names(df)[j] == "profile_pit_id") {

        # Identify row indices for which this information is lacking

      if (any(is.na(df[, j]) |
                (df[, j] == ""))) {
        vec_inconsistency <- which(is.na(df[, j]) | (df[, j] == ""))

        # Store information about the inconsistency in
        # "list_primary_inconsistencies"

        rule_id <- "FSCC_37"
        inconsistency_reason <-
          inconsistency_catalogue$inconsistency_reason[
            which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <-
          inconsistency_catalogue$inconsistency_type[
            which(inconsistency_catalogue$rule_id == rule_id)]

        list_primary_inconsistencies <- rbind(
          list_primary_inconsistencies,
          data.frame(survey_form = (rep(survey_form,
                                        length(vec_inconsistency))),
                     partner = df$partner[vec_inconsistency],
                     partner_code = df$partner_code[vec_inconsistency],
                     country = df$country[vec_inconsistency],
                     code_country = df$code_country[vec_inconsistency],
                     survey_year = df$survey_year[vec_inconsistency],
                     code_plot = df$code_plot[vec_inconsistency],
                     plot_id = df$plot_id[vec_inconsistency],
                     code_layer_horizon_master =
                       column_code_layer_horizon_master[vec_inconsistency],
                     repetition_profile_pit_id =
                       column_repetition_profile_pit_id[vec_inconsistency],
                     code_line = df$code_line[vec_inconsistency],
                     parameter = (rep("profile_pit_id",
                                      length(vec_inconsistency))),
                     parameter_unit = (rep(NA, length(vec_inconsistency))),
                     parameter_value = df$profile_pit_id[vec_inconsistency],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(vec_inconsistency)),
                     change_date = df$change_date[vec_inconsistency],
                     download_date = rep(download_date_pir,
                                         length(vec_inconsistency))))
      }
      }
    }


    # Specific issues ----

    if (survey_form == "so_pfh") {

      # Issue with Romanian profile_pit_ids:
      # Some of the plots have a different profile_pit_id for every layer
      # existing in the plot.
      # This probably arises from a "pull" problem in Excel.
      # It is quite obvious that these layer all belong to the same profile.

      surveys_to_check <- df %>%
        filter(code_country == 52) %>%
        filter(survey_year <= 2010) %>%
        distinct(unique_survey_profile, .keep_all = TRUE) %>%
        group_by(unique_survey) %>%
        summarise(profile_count = n()) %>%
        filter(profile_count > 2) %>%
        pull(unique_survey)

      if (!identical(surveys_to_check, character(0))) {

        assertthat::assert_that(
          all(surveys_to_check %in%
                c("52_2009_11", "52_2009_13", "52_2009_5", "52_2009_9")))

        df <- df %>%
          mutate(profile_pit_id = ifelse(unique_survey %in%
                                           surveys_to_check,
                                         1,
                                         .data$profile_pit_id),
                 unique_survey_profile = ifelse(unique_survey %in%
                                                  surveys_to_check,
                                                paste0(.data$unique_survey, "_",
                                                       .data$profile_pit_id),
                                                .data$unique_survey_profile),
                 unique_layer_repetition =
                   ifelse(unique_survey %in% surveys_to_check,
                          paste0(.data$unique_survey, "_",
                                 .data$horizon_master, "_",
                                 .data$profile_pit_id),
                          .data$unique_layer_repetition))
      }

    }


    if (survey_form == "s1_pfh") {

      # The one Swedish record in 13_2005_624 clearly belongs to
      # the records in survey_year 2006 rather than 2005

      line_to_correct <- df %>%
        filter(unique_survey == "13_2005_624") %>%
        pull(code_line)

      df <- df %>%
        mutate(
          layer_number = ifelse(
            (!is.na(.data$code_line) & code_line == line_to_correct),
            5,
            layer_number),
          survey_year = ifelse(
            (!is.na(.data$code_line) & code_line == line_to_correct),
            2006,
            survey_year),
          unique_survey = ifelse(
            (!is.na(.data$code_line) & code_line == line_to_correct),
            "13_2006_624",
            unique_survey),
          unique_survey_profile = ifelse(
            (!is.na(.data$code_line) & code_line == line_to_correct),
            "13_2006_624_1",
            unique_survey_profile),
          unique_survey_layer = ifelse(
            (!is.na(.data$code_line) & code_line == line_to_correct),
            "13_2006_624_B",
            unique_survey_layer),
          unique_layer_repetition = ifelse(
            (!is.na(.data$code_line) & code_line == line_to_correct),
            "13_2006_624_B_1",
            unique_layer_repetition)) %>%
        arrange(country,
                code_plot,
                survey_year,
                profile_pit_id,
                layer_number)

    }







      if (save_to_env == TRUE) {
        assign_env(survey_form, df)
      }


  } # End of loop along survey forms




  if (save_to_env == TRUE) {

  if (length(unlist(strsplit(survey_form_as_input, "_"))) > 1) {

    assign_env(paste0("list_primary_inconsistencies_", survey_form_as_input),
               list_primary_inconsistencies)

    } else {

    assign_env(paste0("list_primary_inconsistencies_", code_survey),
           list_primary_inconsistencies)
    }
  }

}
