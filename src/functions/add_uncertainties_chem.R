
add_uncertainties_chem <- function(survey_form,
                                   data_frame = NULL,
                                   parameters = NULL,
                                   save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(paste0(" \nAdd uncertainty ranges of chemical parameters to '",
             survey_form, "'\n"))

  code_survey <- unlist(str_split(survey_form, "_"))[1]
  survey_form_type <- unlist(str_split(survey_form, "_"))[2]


  # Import survey forms ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }


  # Uncertainties (in the required parameter units)

  # TOC:
  # layer_type organic and survey_year < 2000: +-11.8 g kg-1
  # layer_type organic and survey_year >= 2000: +- 5.2 g kg-1
  # layer_type mineral and survey_year < 2000: +- 3.5 g kg-1
  # layer_type mineral and survey_year >= 2000: +- 1.5 g kg-1

  uncertainties <- data.frame(
    parameter_som = c("organic_carbon_total"),
    parameter_pfh = c("horizon_c_organic_total"),
    org_before_2000 = c(11.8),
    org_after_2000 = c(5.2),
    min_before_2000 = c(3.5),
    min_after_2000 = c(1.5),
    source = c("Confidence interval of central lab reanalyses by JRC (2011)")
  )


  # Define parameters

  if (survey_form_type == "som") {

    if (is.null(parameters)) {
      parameters <- c("organic_carbon_total")
    }

    assertthat::assert_that(
      all(parameters %in% names(df)))

    assertthat::assert_that(
      all(parameters %in% uncertainties$parameter_som))

  }

  if (survey_form_type == "pfh") {

    if (is.null(parameters)) {
      parameters <- c("horizon_c_organic_total")
    }

    assertthat::assert_that(
      all(parameters %in% names(df)))

    assertthat::assert_that(
      all(parameters %in% uncertainties$parameter_pfh))

  }


  for (i in seq_along(parameters)) {

    parameter_i <- parameters[i]
    parameter_min_i <- paste0(parameter_i, "_min")
    parameter_max_i <- paste0(parameter_i, "_max")



    if (!parameter_min_i %in% names(df)) {

      df[[parameter_min_i]] <- df[[parameter_i]]

    } else {

      df <- df %>%
        mutate(
          !!parameter_min_i := coalesce(.data[[parameter_min_i]],
                                        .data[[parameter_i]]))
    }



    if (!parameter_max_i %in% names(df)) {

      df[[parameter_max_i]] <- df[[parameter_i]]

    } else {

      df <- df %>%
        mutate(
          !!parameter_max_i := coalesce(.data[[parameter_max_i]],
                                        .data[[parameter_i]]))
    }



    if (survey_form_type == "som") {

    unc_i <- uncertainties %>%
      filter(parameter_som == parameter_i)
    }

    if (survey_form_type == "pfh") {

      unc_i <- uncertainties %>%
        filter(parameter_pfh == parameter_i)
    }

    assertthat::assert_that(nrow(unc_i) == 1)

    unc_i <- as.data.frame(unc_i)

    df <- df %>%
      # Uncertainty ranges
      mutate(
        !!parameter_min_i := ifelse(
          !is.na(.data[[parameter_i]]),
          case_when(
            layer_type %in% c("forest_floor", "peat") & survey_year < 2000 ~
              pmax(.data[[parameter_min_i]] - unc_i$org_before_2000, 0),
            layer_type %in% c("forest_floor", "peat") & survey_year >= 2000 ~
              pmax(.data[[parameter_min_i]] - unc_i$org_after_2000, 0),
            layer_type == "mineral" & survey_year < 2000 ~
              pmax(.data[[parameter_min_i]] - unc_i$min_before_2000, 0),
            layer_type == "mineral" & survey_year >= 2000 ~
              pmax(.data[[parameter_min_i]] - unc_i$min_after_2000, 0)),
          NA_integer_),
        !!parameter_max_i := ifelse(
          !is.na(.data[[parameter_i]]),
          case_when(
            layer_type %in% c("forest_floor", "peat") & survey_year < 2000 ~
              pmin(.data[[parameter_max_i]] + unc_i$org_before_2000, 1000),
            layer_type %in% c("forest_floor", "peat") & survey_year >= 2000 ~
              pmin(.data[[parameter_max_i]] + unc_i$org_after_2000, 1000),
            layer_type == "mineral" & survey_year < 2000 ~
              pmin(.data[[parameter_max_i]] + unc_i$min_before_2000, 1000),
            layer_type == "mineral" & survey_year >= 2000 ~
              pmin(.data[[parameter_max_i]] + unc_i$min_after_2000, 1000)),
          NA_integer_)) %>%
      relocate({{parameter_min_i}}, .after = {{parameter_i}}) %>%
      relocate({{parameter_max_i}}, .after = {{parameter_min_i}})





  }



  # Save ----

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)
  } else {
    return(df)
  }


}
