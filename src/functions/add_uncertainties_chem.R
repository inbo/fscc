
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

  # Based on BioSoil reanalysis of '90s and '00s soil samples by a
  # central lab
  # Hiederer R, Durrant Houston T, Micheli E. Evaluation of BioSoil
  # Demonstration Project - Soil Data Analysis. EUR 24729 EN. Luxembourg
  # (Luxembourg): Publications Office of the European Union; 2011. JRC63301

  # Total N:

  lqa <- get_env(paste0(unlist(str_split(survey_form, "_"))[1],
                        "_lqa")) %>%
    filter(code_parameter == "Total_N") %>% # "P_extr", "Total_N", "org_C"
    filter(!is.na(control_chart_std)) %>%
    arrange(control_chart_std) %>%
    pull(control_chart_std) %>%
    # There seem to be some extremely high values which do not seem
    # reliable.
    # Use the upper 80 % quantile (because higher ones may be unreliable,
    # e.g. due to calibration errors or mistakes in calculations)
    quantile(0.80)

  # 4 g kg-1


  uncertainties <- data.frame(
    parameter_som = c("organic_carbon_total", "n_total",
                      "extrac_p"),
    parameter_pfh = c("horizon_c_organic_total", "horizon_n_total",
                      NA),
    org_before_2000 = c(11.8, 4, 4.9),
    org_after_2000 = c(5.2, 4, 4.9),
    min_before_2000 = c(3.5, 4, 4.9),
    min_after_2000 = c(1.5, 4, 4.9),
    source = c("Confidence interval of central lab reanalyses by JRC (2011)",
               "LQA", "LQA")
  )


  # Define parameters

  if (survey_form_type == "som") {

    if (is.null(parameters)) {
      parameters <- c("organic_carbon_total", "n_total", "extrac_p")
    }

    assertthat::assert_that(
      all(parameters %in% names(df)))

    assertthat::assert_that(
      all(parameters %in% uncertainties$parameter_som))

  }

  if (survey_form_type == "pfh") {

    if (is.null(parameters)) {
      parameters <- c("horizon_c_organic_total", "horizon_n_total")
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


    # Theoretically maximum possible value

    max_i <- read.csv2("./data/additional_data/ranges_qaqc.csv") %>%
      filter(!is.na(max_possible)) %>%
      filter(parameter_som == parameter_i |
               parameter_pfh == parameter_i) %>%
      distinct(max_possible) %>%
      pull(max_possible)

    assertthat::assert_that(length(max_i) == 1)


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
              pmin(.data[[parameter_max_i]] + unc_i$org_before_2000, max_i),
            layer_type %in% c("forest_floor", "peat") & survey_year >= 2000 ~
              pmin(.data[[parameter_max_i]] + unc_i$org_after_2000, max_i),
            layer_type == "mineral" & survey_year < 2000 ~
              pmin(.data[[parameter_max_i]] + unc_i$min_before_2000, max_i),
            layer_type == "mineral" & survey_year >= 2000 ~
              pmin(.data[[parameter_max_i]] + unc_i$min_after_2000, max_i)),
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
