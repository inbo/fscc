
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
#'     "code_layer_original"
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
      df$code_layer_original <- as.character(df$code_layer)
    df$code_layer <- as.character(df$code_layer)
    }

    if ("code_layer_original" %in% names(df)) {
      col_code_layer <- df$code_layer_original
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
                                    df$code_layer_original[j_dupl])))

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

    # Remove the column "code_layer_original"
    # if the survey form does not contain any ambiguous layers
    # (e.g. for "so_som")

    if (all(df$code_layer == df$code_layer_original)) {
      df <- df[, -which(names(df) == "code_layer_original")]
      }

    }








    # Check for the presence of primary key information

    # Store the column indices of columns with primary key information

    vec_primary <- which(names(df) %in%
                             c("survey_year", "code_plot", "horizon_master",
                               "code_layer", "profile_pit_id"))

    column_code_layer_horizon_master <- rep(NA, nrow(df))
    column_repetition_profile_pit_id <- rep(NA, nrow(df))

    if ("code_layer_original" %in% names(df)) {
      column_code_layer_horizon_master <-
        pull(df[, which(names(df) == "code_layer_original")])
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


        # FSCC_35: "survey_year" outside possible range

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


      # FSCC_36
      # code_plot

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


      # FSCC_33: missing "horizon_master" or "code_layer"
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
      }
      }


      if (names(df)[j] == "code_layer") {

        # Evaluate the presence of "code_layer_original" if "code_layer" has
        # been renamed (in case of ambiguous code_layer)

      if ("code_layer_original" %in% names(df)) {
        col <- which(names(df) == "code_layer_original")
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
        }
        }


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

    if ((unlist(strsplit(survey_form, "_"))[2] == "som") &&
        (any("FSCC_47" %in% unique(list_primary_inconsistencies$rule_id)))) {

      if (save_to_env == TRUE) {
        assign_env(survey_form, df)
      }
      }

  }

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
