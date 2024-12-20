
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
    # "P_extr", "Total_N", "org_C", "S_extr", "Fe_react", "Al_react", "Ca_exch"
    filter(code_parameter == "Total_N") %>%
    filter(!is.na(control_chart_std)) %>%
    arrange(control_chart_std) %>%
    pull(control_chart_std) %>%
    # There seem to be some extremely high values which do not seem
    # reliable.
    # Use the upper 80 % quantile (because higher ones may be unreliable,
    # e.g. due to calibration errors or mistakes in calculations)
    quantile(0.80)

  # Total N: 4.0 g kg-1
  # extrac_p: 4.9 mg kg-1
  # extrac_s: 5.0 mg kg-1
  # rea_fe: 6.0 mg kg-1
  # rea_al: 6.7 mg kg-1
  # exch_ca: 10 cmol(+) kg-1

  # FYI: 1 cmol(+) (from Ca2+) kg-1 = 200.39 mg Ca2+ kg-1


  uncertainties <- data.frame(
    parameter_som = c("organic_carbon_total", "n_total",
                      "extrac_p", "extrac_s", "rea_fe", "rea_al", "exch_ca"),
    parameter_pfh = c("horizon_c_organic_total", "horizon_n_total",
                      NA, NA, NA, NA, "horizon_exch_ca"),
    org_before_2000 = c(11.8, 4, 4.9, 5, 6, 6.7, 10),
    org_after_2000 = c(5.2, 4, 4.9, 5, 6, 6.7, 10),
    min_before_2000 = c(3.5, 4, 4.9, 5, 6, 6.7, 10),
    min_after_2000 = c(1.5, 4, 4.9, 5, 6, 6.7, 10),
    source = c("Confidence interval of central lab reanalyses by JRC (2011)",
               "LQA", "LQA", "LQA", "LQA", "LQA", "LQA"))


  # Define parameters

  if (is.null(parameters)) {
    parameters <- c("organic_carbon_total", "n_total", "extrac_p",
                    "extrac_s", "rea_fe", "rea_al", "exch_ca")
  }

  if (survey_form_type == "som") {

    assertthat::assert_that(
      all(parameters %in% names(df)))

    assertthat::assert_that(
      all(parameters %in% uncertainties$parameter_som))

  }

  if (survey_form_type == "pfh") {

    parameters <- parameters[which(parameters %in% names(df))]

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

    assertthat::assert_that(identical(max_i, integer(0)) ||
                              length(max_i) == 1)

    if (identical(max_i, integer(0))) {

      cation_data <- data.frame(
        par = c("free_h", "exch_k", "exch_ca",
                "exch_mg", "exch_na", "exch_al",
                "exch_fe", "exch_mn"),
        charge = c(1, 1, 2,
                   2, 1, 3,
                   2, 2),
        molar_mass = c(1.008, 39.098, 40.078,
                       24.305, 22.990, 26.982,
                       55.845, 54.938))

      assert_that(parameter_i %in% cation_data$par)

      charge_i <- cation_data$charge[which(cation_data$par == parameter_i)]
      mol_i <- cation_data$molar_mass[which(cation_data$par == parameter_i)]


      # These exchangeable data are reported in cmol(+) kg-1.
      # If they would be converted to mg kg-1, the maximum would be
      # 1E6 mg kg-1. Convert this to a molar maximum.

      max_i <- 1E6 * # mg kg-1
        1E-1 / # cg mg-1
        mol_i * # g per mol cations
        charge_i # mol(+) mol-1 cations

    }


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
