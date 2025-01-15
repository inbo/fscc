
#' Calculate stocks for a given concentration parameter
#'
#' @param survey_form Character string - Name of the survey form (lower case
#' and separated by '_') to be evaluated
#' @param data_frame Dataframe with the layer-specific profile data, e.g.
#' "som" or "pfh" survey forms. If NULL, the dataframe with the name specified
#' in "survey_form" is retrieved from the global environment.
#' @param parameter Name of concentration parameter to calculate stocks for.
#' Default is "organic_carbon_total".
#' @param constant_subsoil Logical indicating whether it is okay to assume that
#' the density (per cm) of the parameter is constant in the subsoil, i.e.
#' from 30 to 100 cm.
#' @param graph Logical indicating whether a graph of the spline fitting
#' should be made for the given depth profile
#' @param density_per_three_cm Logical indicating whether the density data
#' as returned from the spline function per 3 cm depth increment should be
#' included for graphing purposes. Default is FALSE.
#' @param save_to_env Logical indicating whether the output dataframes
#' can be saved to the environment and override any existing objects
#' with the same name. Default is TRUE.
#' @param save_to_gdrive Logical indicating whether the output dataframes
#' can be saved to the project Google Drive folder (date-specific subfolder
#' of "./output/stocks/). Default is TRUE.
#'
#' @return
#' @export
#'
#' @examples get_stocks(survey_form = "so_pfh",
#'                      data_frame = so_pfh,
#'                      density_per_three_cm = TRUE)

get_stocks <- function(survey_form,
                       data_frame = NULL,
                       parameter = "organic_carbon_total",
                       constant_subsoil = TRUE,
                       exclude_ol = TRUE,
                       graph = FALSE,
                       density_per_three_cm = FALSE,
                       add_stratifiers = TRUE,
                       add_plausible_fscc = TRUE,
                       save_to_env = TRUE,
                       save_to_gdrive = TRUE) {


  # Load packages and functions ----

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"),
            require("mpspline2"),
            require("patchwork"),
            require("ggtext"),
            require("grid"),
            require("broom"),
            require("readxl"))

  source("./src/stock_calculations/functions/soilspline.R")
  source("./src/stock_calculations/functions/spline2stock.R")

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")
  source("./src/functions/save_to_google_drive.R")
  source("./src/functions/as_character_summary.R")
  source("./src/functions/get_stratifiers.R")
  source("./src/functions/summarise_per_group.R")
  source("./src/functions/get_parameter_stats.R")
  source("./src/functions/get_date_local.R")
  source("./src/stock_calculations/functions/check_stock_plausibility.R")


  # Define data ----

  if (length(unlist(str_split(survey_form, "_"))) == 1 &&
      is.null(data_frame)) {

    survey_form_orig <- survey_form

    code_survey <- survey_form

    survey_forms <- paste0(code_survey,
                           c("_som", "_pfh"))

  }

  if (length(unlist(str_split(survey_form, "_"))) == 2) {

    survey_form_orig <- survey_form

    code_survey <- unlist(str_split(survey_form, "_"))[1]

    survey_forms <- survey_form
  }

  cat(paste0(" \nCalculate '", parameter, "' stocks for ",
             as_character_summary(survey_forms), ".\n \n"))



  if (add_stratifiers == TRUE) {

    if (code_survey == "s1") {

      if (exists("s1_strat")) {
        df_strat <- get_env("s1_strat")
      } else {
        df_strat <- get_stratifiers(level = "LI")
      }
    }

    if (code_survey == "so") {

      if (exists("so_strat")) {
        df_strat <- get_env("so_strat")
      } else {
        df_strat <- get_stratifiers(level = "LII")
      }
    }

    df_strat <- df_strat %>%
      select(plot_id, longitude_dec, latitude_dec, x_etrs89, y_etrs89,
             mat, map, altitude,
             slope, aspect, wrb_ref_soil_group, eftc, humus_form,
             parent_material, biogeographical_region,
             main_tree_species, bs_class)
  }


  # Specify parameter ----

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

  parameter_table <- data.frame(
    som_parameter = c(
      "extrac_pb", "extrac_zn", "part_size_clay", "part_size_silt", "exch_mn",
      "carbonates", "extrac_al", "exch_mg", "exch_fe", "extrac_na",
      "exch_ca", "ph_cacl2", "n_total", "extrac_k", "exch_al",
      "tot_mg", "exch_k", "extrac_s", "organic_carbon_total", "exch_na",
      "extrac_cr", "extrac_fe", "tot_k", "ph_h2o", "part_size_sand",
      "extrac_cu", "free_h", "extrac_cd", "extrac_hg", "extrac_ca",
      "extrac_mg", "extrac_ni", "extrac_p", "extrac_al", "extrac_mn",
      "extrac_fe", "tot_ca", "p_ox", "exch_acidiy", "tot_na",
      "tot_mn", "tot_fe", "tot_al",
      "moisture_content", "bulk_density", "rea_al", "rea_fe",
      "organic_layer_weight"),
    pfh_parameter = c(
      NA, NA, "horizon_clay", "horizon_silt", NA,
      "horizon_caco3_total", NA, "horizon_exch_mg", NA, NA,
      "horizon_exch_ca", NA, "horizon_n_total", NA, NA,
      NA, "horizon_exch_k", NA, "horizon_c_organic_total", "horizon_exch_na",
      NA, NA, NA, "horizon_ph", "horizon_sand",
      NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA,
      NA, NA, NA,
      NA, "bulk_density", NA, NA,
      "organic_layer_weight"),
    unit = c(
      "mg kg-1", "mg kg-1", "%wt", "%wt", "cmol+ kg-1",
      "g kg-1", "mg kg-1", "cmol+ kg-1", "cmol+ kg-1", "mg kg-1",
      "cmol+ kg-1", "-", "g kg-1", "mg kg-1", "cmol+ kg-1",
      "mg kg-1", "cmol+ kg-1", "mg kg-1", "g kg-1", "cmol+ kg-1",
      "mg kg-1", "mg kg-1", "mg kg-1", "-", "%wt",
      "mg kg-1", "cmol+ kg-1", "mg kg-1", "mg kg-1", "mg kg-1",
      "mg kg-1", "mg kg-1", "mg kg-1", "mg kg-1", "mg kg-1",
      "mg kg-1", "mg kg-1", "-", "cmol+ kg-1", "mg kg-1",
      "mg kg-1", "mg kg-1", "mg kg-1",
      "%wt", "kg m-3", "mg kg-1", "mg kg-1",
      "kg m-2"),
    # Only for variables for which stocks can be calculated
    # unit_density_per_cm = c(
    #   "kg ha-1 cm-1", "kg ha-1 cm-1", NA, NA, "1E4 mol+ ha-1 cm-1",
    #   "t ha-1 cm-1", "kg ha-1 cm-1", "1E4 mol+ ha-1 cm-1",
    #   "1E4 mol+ ha-1 cm-1",
    #   "kg ha-1 cm-1",
    #   "1E4 mol+ ha-1 cm-1", NA, "t ha-1 cm-1", "kg ha-1 cm-1",
    #   "1E4 mol+ ha-1 cm-1",
    #   "kg ha-1 cm-1", "1E4 mol+ ha-1 cm-1", "kg ha-1 cm-1", "t ha-1 cm-1",
    #   "1E4 mol+ ha-1 cm-1",
    #   "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1", NA, NA,
    #   "kg ha-1 cm-1", "1E4 mol+ ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1",
    #   "kg ha-1 cm-1",
    #   "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1",
    #   "kg ha-1 cm-1",
    #   "kg ha-1 cm-1", "kg ha-1 cm-1", NA, "1E4 mol+ ha-1 cm-1",
    #   "kg ha-1 cm-1",
    #   "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1",
    #   NA, NA, "kg ha-1 cm-1", "kg ha-1 cm-1",
    #   NA),
    # Only for variables for which stocks can be calculated
    shorter_name = c(
      "extrac_pb", "extrac_zn", NA, NA, "exch_mn",
      "caco3", "extrac_al", "exch_mg", "exch_fe", "extrac_na",
      "exch_ca", NA, "n", "extrac_k", "exch_al",
      "tot_mg", "exch_k", "extrac_s", "c", "exch_na",
      "extrac_cr", "extrac_fe", "tot_k", NA, NA,
      "extrac_cu", "free_h", "extrac_cd", "extrac_hg", "extrac_ca",
      "extrac_mg", "extrac_ni", "extrac_p", "extrac_al", "extrac_mn",
      "extrac_fe", "tot_ca", NA, "exch_acidiy", "tot_na",
      "tot_mn", "tot_fe", "tot_al",
      NA, NA, "rea_al", "rea_fe",
      NA
    )) %>%
    mutate(
      unit_density_per_cm = case_when(
        # Only calculate for rows for which stocks can be calculated (i.e.
        # where shorter_var_name is not NA)
        !is.na(shorter_var_name) & unit == "mg kg-1" ~ "kg ha-1 cm-1",
        # Important: this needs to be converted!!!!! from "1E4 mol+ ha-1 cm-1"
        !is.na(shorter_var_name) & unit == "cmol+ kg-1" ~ "kg ha-1 cm-1",
        !is.na(shorter_var_name) & unit == "g kg-1" ~ "t ha-1 cm-1",
        TRUE ~ NA_character_
      )) %>%
    left_join(
      cation_data,
      by = join_by("som_parameter" == "par")) %>%
    mutate(
      density_convert_factor = ifelse(
        unit == "cmol+ kg-1",
        # Equivalent in mg kg-1 for 1 cmol+ kg-1:
        1 / # 1 cmol+ kg-1
          charge * # mol(+) mol-1 cations
          molar_mass * # g mol-1 cations
          1E1, # mg cg-1
        1)) %>%
    select(-charge, -molar_mass) %>%
    mutate(
      unit_markdown = gsub("1E4", "10<sup>4</sup>",
                           gsub("-1", "<sup>-1</sup>",
                                gsub("-2", "<sup>-2</sup>",
                                     gsub("-3", "<sup>-3</sup>", unit)))),
      unit_density_per_cm_markdown =
        gsub("1E4", "10<sup>4</sup>",
             gsub("-1", "<sup>-1</sup>",
                  gsub("-2", "<sup>-2</sup>",
                       gsub("-3", "<sup>-3</sup>", unit_density_per_cm)))))


  assign_env("parameter_table", parameter_table)

  ind_som <- which(parameter == parameter_table$som_parameter)
  ind_pfh <- which(parameter == parameter_table$pfh_parameter)
  ind <- unique(c(ind_som, ind_pfh))

  assertthat::assert_that(!identical(ind, integer(0)))

  unit <- parameter_table$unit[ind]
  unit_markdown <- parameter_table$unit_markdown[ind]
  unit_density_per_cm <- parameter_table$unit_density_per_cm[ind]
  unit_density_per_cm_markdown <-
    parameter_table$unit_density_per_cm_markdown[ind]
  density_convert_factor <- parameter_table$density_convert_factor[ind]
  shorter_var_name <- parameter_table$shorter_name[ind]

  assertthat::assert_that(!is.na(shorter_var_name))

  parameter_orig <- parameter
  parameter <- parameter_table$som_parameter[ind]

  loq_org <- read.csv2("./data/additional_data/ranges_qaqc.csv") %>%
    filter(parameter_som == parameter) %>%
    pull(LOQ_org)

  loq_mineral <- read.csv2("./data/additional_data/ranges_qaqc.csv") %>%
    filter(parameter_som == parameter) %>%
    pull(LOQ_mineral)



  # Some parameters only appear in "som"

  if (length(survey_forms) == 2 &&
      is.na(parameter_table$pfh_parameter[ind])) {

    survey_forms <- paste0(code_survey,
                           c("_som"))
  }


  # Create dir to save data ----

  dir <- paste0("./output/stocks/",
                as.character(format(Sys.Date(), "%Y%m%d")), "_",
                as.character(format(as.Date(
                  get_date_local(path = "./data/raw_data/")),
                  format = "%Y%m%d")),
                "_", shorter_var_name, "_stocks/")

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  if (graph == TRUE) {

    for (survey_form_i in survey_forms) {

      if (!dir.exists(paste0(dir, survey_form_i,
                             "_splines_per_profile/"))) {
        dir.create(paste0(dir, survey_form_i,
                          "_splines_per_profile/"), recursive = TRUE)
      }
    }

    if (!dir.exists(paste0(dir, code_survey, "_splines_per_plot/"))) {
      dir.create(paste0(dir, code_survey, "_splines_per_plot/"),
                 recursive = TRUE)
    }
  }




  # Evaluate for each survey_form ----

  # 1. Preparations ----

  for (survey_form_i in survey_forms) {

  ## 1.1. Retrieve data ----

    if (is.null(data_frame)) {
      df <- get_env(survey_form_i)
    } else {
      df <- data_frame
    }

  ## 1.2. Assert that the input parameters are correct ----

  assertthat::assert_that(survey_form_i %in% c("s1_som",
                                             "s1_pfh",
                                             "so_som",
                                             "so_pfh"))

  # Assert that the required columns exist

  # "pfh"

  if (unlist(strsplit(survey_form_i, "_"))[2] == "pfh") {

    assertthat::assert_that(all(c(
      # Descriptive profile information
      "plot_id",
      "survey_year",
      "profile_pit_id",
      "partner_short",
      "code_country",
      "code_plot",
      # Layer information
      "horizon_master",
      "layer_number",
      "layer_type",
      "horizon_limit_up",
      "horizon_limit_low",
      "layer_thickness",
      # Data
      "organic_layer_weight",
      "coarse_fragment_vol",
      "bulk_density") %in% names(df)))

    df <- df %>%
      rename(code_layer = horizon_master) %>%
      rename(repetition = profile_pit_id) %>%
      rename(layer_limit_superior = horizon_limit_up) %>%
      rename(layer_limit_inferior = horizon_limit_low)

  }


  # "som"

  if (unlist(strsplit(survey_form_i, "_"))[2] == "som") {

    assertthat::assert_that(all(c(
      # Descriptive profile information
      "plot_id",
      "survey_year",
      "repetition",
      "partner_short",
      "code_country",
      "code_plot",
      # Layer information
      "code_layer",
      "layer_number",
      "layer_type",
      "layer_limit_superior",
      "layer_limit_inferior",
      "layer_thickness",
      # Data
      "organic_layer_weight",
      "coarse_fragment_vol",
      "bulk_density") %in% names(df)))

  }


  # Rename the variable to a harmonised name
  # and retrieve its unit

  assertthat::assert_that(
    parameter_table$som_parameter[ind] %in% names(df) ||
      parameter_table$pfh_parameter[ind] %in% names(df))

  if (!parameter_table$som_parameter[ind] %in% names(df)) {

    names(df)[which(names(df) == parameter_table$pfh_parameter[ind])] <-
      parameter_table$som_parameter[ind]

    par_min <- paste0(parameter_table$pfh_parameter[ind], "_min")
    par_max <- paste0(parameter_table$pfh_parameter[ind], "_max")

    if (par_min %in% names(df) &&
        par_max %in% names(df)) {

      names(df)[which(names(df) == par_min)] <-
        paste0(parameter_table$som_parameter[ind], "_min")

      names(df)[which(names(df) == par_max)] <-
        paste0(parameter_table$som_parameter[ind], "_max")
    }
  }

  names(df)[which(names(df) == parameter)] <-
    "parameter_for_stock"

  names(df)[which(names(df) == paste0(parameter, "_min"))] <-
    "parameter_for_stock_min"

  names(df)[which(names(df) == paste0(parameter, "_max"))] <-
    "parameter_for_stock_max"

  if (paste0(parameter, "_source") %in% names(df)) {

    names(df)[which(names(df) == paste0(parameter, "_source"))] <-
      "parameter_for_stock_source"

  } else {

    df$parameter_for_stock_source <- NA
  }

  if (paste0(parameter, "_loq") %in% names(df)) {

    names(df)[which(names(df) == paste0(parameter, "_loq"))] <-
      "parameter_for_stock_loq"

    if (any(is.na(df$parameter_for_stock_loq))) {

      df <- df %>%
        mutate(
          parameter_for_stock_loq = coalesce(
            parameter_for_stock_loq,
            ifelse(
              layer_type %in% c("forest_floor", "peat"),
              loq_org,
              loq_mineral)))
    }

  } else {

    df <- df %>%
      mutate(
        parameter_for_stock_loq = ifelse(
          layer_type %in% c("forest_floor", "peat"),
          loq_org,
          loq_mineral))
  }


  assertthat::assert_that(all(na.omit(df[[parameter]]) >= 0))


  ## 1.2. Add eff_soil_depth if needed ----

  if (!"eff_soil_depth" %in% names(df)) {

  cat(" \nAdd eff_soil_depth\n")

    if (code_survey == "s1") {

      if (exists("s1_strat")) {

        df_soil_depth <- get_env("s1_strat") %>%
          select(plot_id, eff_soil_depth, eff_soil_depth_source)

      } else {

        df_soil_depth <- get_stratifiers(level = "LI") %>%
          select(plot_id, eff_soil_depth, eff_soil_depth_source)
      }
    }

    if (code_survey == "so") {

      if (exists("so_strat")) {

        df_soil_depth <- get_env("so_strat") %>%
          select(plot_id, eff_soil_depth, eff_soil_depth_source)

      } else {

        df_soil_depth <- get_stratifiers(level = "LII") %>%
          select(plot_id, eff_soil_depth, eff_soil_depth_source)
      }
    }

    df <- df %>%
      # Add soil depth data
      left_join(df_soil_depth,
                by = "plot_id")
  }

  ## 1.3. Final data preparations ----

  df_working <- df %>%
    arrange(country,
            code_plot,
            survey_year,
            repetition,
            layer_number) %>%
    ungroup() %>%
    mutate(partner_short = as.character(partner_short),
           partner = as.character(partner),
           country = as.character(country)) %>%
    # Add survey_form
    mutate(survey_form = survey_form_i) %>%
    # If the effective soil depth is deeper than 100 cm or unknown (NA):
    # assume it is 100 cm (for stocks)
    mutate(depth_stock =
             ifelse(is.na(.data$eff_soil_depth) |
                      (.data$eff_soil_depth > 100),
                    100,
                    .data$eff_soil_depth)) %>%
    # If coarse fragments is unknown:
    # Assume it is 0
    # To do: the uncertainty associated with this assumption needs
    # to be assessed
    mutate(coarse_fragment_vol_frac =
             ifelse(is.na(.data$coarse_fragment_vol),
                    0,
                    .data$coarse_fragment_vol / 100)) %>%
    mutate(coarse_fragment_vol_min_frac =
             ifelse(is.na(.data$coarse_fragment_vol_min),
                    0,
                    .data$coarse_fragment_vol_min / 100)) %>%
    mutate(coarse_fragment_vol_max_frac =
             ifelse(is.na(.data$coarse_fragment_vol_max),
                    # Quick approximation as the stdev of coarse_fragment_vol
                    0.22,
                    .data$coarse_fragment_vol_max / 100)) %>%
    # profile_id is basically the same like unique_survey_repetition
    # (to do: harmonise names across project)
    mutate(profile_id = paste0(survey_year, "_",
                               plot_id, "_",
                               repetition)) %>%
    # layer_thickness already exists
    mutate(depth_top = layer_limit_superior,
           depth_bottom = layer_limit_inferior,
           depth_avg = ifelse(!is.na(depth_top) & !is.na(depth_bottom),
                              rowMeans(cbind(depth_top, depth_bottom)),
                              NA))


  # As requested by the partner, exclude data for 54_204 in 1997
  # (because of being unreliable)

  if (survey_form_i == "so_som" &&
      parameter == "organic_carbon_total") {

    df_working <- df_working %>%
      filter(!(plot_id == "54_204" & survey_year == 1997))

  }


  ## 1.4. Prepare an overview of potential for stock calculation ----
  # (only for "som" survey forms)

  if (unlist(strsplit(survey_form_i, "_"))[2] == "som") {

    plot_data_use_stocks <- get_env(paste0("data_availability_",
                                           code_survey)) %>%
      # Split survey_years by "_"
      separate_rows(survey_years, sep = "_") %>%
      # Keep distinct rows of plot_id and survey_years
      distinct(code_country, code_plot, plot_id, survey_years) %>%
      rename(survey_year = survey_years) %>%
      mutate(
        unique_survey = paste0(code_country, "_",
                               survey_year, "_",
                               code_plot)) %>%
      mutate(
        # Column with logical indicating whether a plausible stock could
        # be calculated for the given plot survey (plot_id x survey_year)
        plaus_stock_calculated = NA,
        # Column indicating the reason why no stock was calculated,
        # if plaus_stock_calculated is FALSE
        reason_excluded = NA_character_,
        # Column indicating the coverage of stocks that are assumed to be
        # plausible, i.e. the soil compartments with plausible stocks
        plaus_stock_coverage = NA_character_)

    assertthat::assert_that(
      all(unique(df_working$unique_survey) %in%
            plot_data_use_stocks$unique_survey))

    # Add reason for data which are not in "som"
    # (e.g. because they are in "pfh")

    plot_data_use_stocks <- plot_data_use_stocks %>%
      mutate(
        in_som = (unique_survey %in% unique(df_working$unique_survey)),
        reason_excluded = case_when(
          # One Slovakian plot excluded upon request of the soil expert
          unique_survey == "54_1997_204" &
            parameter == "organic_carbon_total" ~
            "Considered implausible by partner",
          in_som == FALSE ~ "No records in 'som'",
          TRUE ~ reason_excluded)) %>%
      select(-in_som)

    df_summ <- df_working %>%
      group_by(code_country, plot_id, unique_survey) %>%
      reframe(
        # Any below-ground layer
        any_bg = any(layer_type != "forest_floor" &
                       !is.na(layer_limit_inferior) &
                       layer_limit_inferior > 0),
        any_bg_non_redundant =
          any(layer_type != "forest_floor" &
                !is.na(layer_limit_inferior) &
                layer_limit_inferior > 0 &
                !is.na(layer_number)),
        # depth_stock_source:
        # - "Known to be >= 100 cm (depth of 'pfh')"
        # - "Depth of profile ('pfh')"
        # - "Manually harmonised"
        # - "Default based on WRB soil classification"
        # - "Max. of rooting, rock and obstacle depth"
        # - "Original eff_soil_depth"
        # - "Top of parent material horizons"
        # - "Top of stony zone"
        # - "Default (i.e. 100 cm)"
        depth_stock_source = case_when(
          any(!is.na(depth_stock) & depth_stock == 100 &
                is.na(eff_soil_depth)) ~
            "Default (i.e. 100 cm)",
          any(grepl("no info", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Default (i.e. 100 cm)",
          any(grepl("deep", eff_soil_depth_source, ignore.case = TRUE) &
            grepl("'pfh'", eff_soil_depth_source)) ~
            "Known to be >= 100 cm (depth of 'pfh')",
          any(eff_soil_depth_source == "Bottom of profile ('pfh')") ~
            "Depth of profile ('pfh')",
          any(grepl("\\bWRB\\b", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Default based on WRB soil classification",
          any(grepl("obstacle depth", eff_soil_depth_source,
              ignore.case = TRUE)) ~
            "Max. of rooting, rock and obstacle depth",
          any(grepl("Original", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Original eff_soil_depth",
          any(grepl("parent material", eff_soil_depth_source,
              ignore.case = TRUE)) ~
            "Top of parent material horizons",
          any(grepl("stony zone", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Top of stony zone",
          any(grepl("Harmonised", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Manually harmonised",
          TRUE ~ NA_character_),
        # bulk_density_source:
        # - "Old data source"
        # - "'pfh' (measured)"
        # - "'pfh' (estimated)"
        # - "'som'"
        # - "'sw_swc'"
        # - "PTF"
        bulk_density_source = case_when(
          any(grepl("som", bulk_density_source) &
                layer_type != "forest_floor") ~
            paste0(code_survey, "_som"),
          any(grepl("swc", bulk_density_source) &
                layer_type != "forest_floor") ~
            "sw_swc",
          any(grepl("pfh_measure", bulk_density_source)
              & layer_type != "forest_floor") ~
            paste0(code_survey, "_pfh (measured)"),
          any(grepl("pfh_est", bulk_density_source) &
                layer_type != "forest_floor") ~
            paste0(code_survey, "_pfh (estimated)"),
          any(grepl("fscdb", bulk_density_source, ignore.case = TRUE) &
            layer_type != "forest_floor") ~
            "Old data source",
          any(grepl("PTF", bulk_density_source, ignore.case = TRUE) &
            layer_type != "forest_floor") ~
            "PTF",
          TRUE ~ NA_character_),
        # organic_layer_weight_source:
        # - "'som'"
        # - "Old data source"
        # - "Bulk density and layer thickness"
        organic_layer_weight_source = case_when(
          any(grepl("^som", organic_layer_weight_source) &
                !grepl("bulk_density", organic_layer_weight_source) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            paste0(code_survey, "_som"),
          any(grepl("bulk_density", organic_layer_weight_source) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            "Bulk density & layer thickness",
          any(grepl("fscdb", organic_layer_weight_source, ignore.case = TRUE) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            "Old data source",
          TRUE ~ NA_character_),
        # coarse_fragment_vol_source:
        # - "Old data source"
        # - "'pfh' (code_horizon_coarse_vol)"
        # - "'pfh' (horizon_coarse_weight)"
        # - "'som'"
        # - "Default (i.e. 0 vol%)"
        coarse_fragment_vol_source = case_when(
          any(grepl("som", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0(code_survey, "_som"),
          any(grepl("pfh_weight", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0(code_survey, "_pfh (horizon_coarse_weight)"),
          any(grepl("pfh_code_vol", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0(code_survey, "_pfh (code_horizon_coarse_vol)"),
          any(grepl("fscdb", coarse_fragment_vol_source, ignore.case = TRUE) &
                layer_type == "mineral") ~
            "Old data source",
          any(!is.na(coarse_fragment_vol_frac) &
                coarse_fragment_vol_frac == 0 &
                is.na(coarse_fragment_vol)) ~
            "Default (i.e. 0 vol%)",
          TRUE ~ NA_character_),
        # parameter_for_stock_source:
        # - "Old data source"
        # - "'pfh' (same period)"
        # - "'som'"
        # - "PIR"
        parameter_for_stock_source = case_when(
          any(grepl("som", parameter_for_stock_source)) ~
            paste0(code_survey, "_som"),
          any(grepl("fscdb", parameter_for_stock_source,
                    ignore.case = TRUE)) ~
            "Old data source",
          any(grepl("pfh", parameter_for_stock_source)) ~
            paste0(code_survey, "_pfh (same period)"),
          any(parameter_for_stock_source == "PIR") ~ "PIR",
          any(grepl("manual correction", parameter_for_stock_source)) ~
            "Manual correction (FSCC)"))

    # Add summary of data sources per plot survey

    plot_data_use_stocks <- plot_data_use_stocks %>%
      left_join(df_summ %>%
                  select(-code_country, -plot_id),
                by = "unique_survey") %>%
      mutate(
        reason_excluded = case_when(
          any_bg == FALSE ~ "No below-ground layers reported in 'som'",
          any_bg == TRUE & any_bg_non_redundant == FALSE ~
            "All below-ground layers are considered redundant",
          TRUE ~ reason_excluded)) %>%
      select(-any_bg,
             -any_bg_non_redundant)

    }






  ## 1.5. Create function format_stocks ----

  format_stocks <- function(data_frame,
                            var_name = shorter_var_name) {

    df_output <- data_frame %>%
      # Remove any columns starting with the word "density"
      # (these columns, with densities after fitting mass-preserving splines,
      # are only for internal graphing purposes)
      select(-starts_with("density_")) %>%
      # Remove records with negative stocks
      filter(if_any(matches("stock_10"),  ~.x >= 0)) %>%
      filter(if_any(matches("stock_forest_floor"), ~(is.na(.x) | .x >= 0))) %>%
    # Add the variable name (e.g. "c_" for TOC) to the stock names
      rename_with(~ ifelse(startsWith(., "stock"),
                           paste(var_name, ., sep = "_"), .),
                  everything()) %>%
      rename_with(~ ifelse(startsWith(., "density"),
                           paste(var_name, ., sep = "_"), .),
                  everything()) %>%
      # Rename the column with name "parameter_for_stock" to the
      # actual parameter name (e.g. "organic_carbon_total")
      rename_with(
        ~ case_when(
          . == "parameter_for_stock" ~ parameter,
          TRUE ~ .)) %>%
      rename_with(
        ~ case_when(
          . == "parameter_for_stock_min" ~ paste0(parameter, "_min"),
          TRUE ~ .)) %>%
      rename_with(
        ~ case_when(
          . == "parameter_for_stock_max" ~ paste0(parameter, "_max"),
          TRUE ~ .))

    return(df_output)
  }






  # 2. Below-ground layers ----

  ## 2.1. Derive layer-based dataset for mineral and peat layers ----

  df_below_ground <- df_working %>%
    filter(.data$layer_type %in% c("mineral", "peat") &
             !is.na(depth_top) &
             (depth_top >= 0)) %>%
    # Filter out redundant layers
    # (i.e. layers not needed to compose the complete profile)
    filter(!is.na(.data$layer_number)) %>%
    select(survey_form,
           partner_short,
           plot_id,
           unique_survey,
           profile_id,
           partner_code,
           code_country,
           code_plot,
           survey_year,
           repetition,
           code_layer,
           layer_type,
           layer_number,
           depth_top,
           depth_bottom,
           depth_avg,
           layer_thickness,
           depth_stock,
           bulk_density,
           bulk_density_min,
           bulk_density_max,
           organic_layer_weight,
           organic_layer_weight_min,
           organic_layer_weight_max,
           coarse_fragment_vol,
           coarse_fragment_vol_frac,
           coarse_fragment_vol_min_frac,
           coarse_fragment_vol_max_frac,
           parameter_for_stock,
           parameter_for_stock_min,
           parameter_for_stock_max,
           parameter_for_stock_loq) %>%
    # Calculate carbon density per cm (t C ha-1 cm-1)
    # Units: (g C/kg fine earth) * (kg fine earth/m3 soil) = g C/m3 soil
    # 1 ha * 1 cm = 100 m * 100 m * 0.01 m = 100 m3
    # 1 ton C = 1E6 g C
    # Unit conversion factor: * 100 / 1000000
    mutate(density =
             (.data$parameter_for_stock * density_convert_factor *
                .data$bulk_density *
                (1 - .data$coarse_fragment_vol_frac)) / 10000) %>%
    mutate(density_min =
             (.data$parameter_for_stock_min * density_convert_factor *
                .data$bulk_density_min *
                (1 - .data$coarse_fragment_vol_max_frac)) / 10000) %>%
    mutate(density_max =
             (.data$parameter_for_stock_max * density_convert_factor *
                .data$bulk_density_max *
                (1 - .data$coarse_fragment_vol_min_frac)) / 10000) %>%
    mutate(density_loq =
             (.data$parameter_for_stock_loq * density_convert_factor *
                .data$bulk_density *
                (1 - .data$coarse_fragment_vol_frac)) / 10000) %>%
    # Organic layers (peat) with known organic_layer_weight
    mutate(density_org =
             ifelse(layer_type == "peat" &
                      !is.na(.data$organic_layer_weight),
                    (.data$parameter_for_stock * density_convert_factor *
                       .data$organic_layer_weight) /
                      (.data$layer_thickness * 100),
                    NA_integer_)) %>%
    mutate(density_min_org =
             ifelse(layer_type == "peat" &
                      !is.na(.data$organic_layer_weight),
                    (.data$parameter_for_stock_min * density_convert_factor *
                       .data$organic_layer_weight_min) /
                      (.data$layer_thickness * 100),
                    NA_integer_)) %>%
    mutate(density_max_org =
             ifelse(layer_type == "peat" &
                      !is.na(.data$organic_layer_weight),
                    (.data$parameter_for_stock_max * density_convert_factor *
                       .data$organic_layer_weight_max) /
                      (.data$layer_thickness * 100),
                    NA_integer_)) %>%
    mutate(density_loq_org =
             ifelse(layer_type == "peat" &
                      !is.na(.data$organic_layer_weight),
                    (.data$parameter_for_stock_loq * density_convert_factor *
                       .data$organic_layer_weight) /
                      (.data$layer_thickness * 100),
                    NA_integer_)) %>%
    mutate(density = coalesce(density_org, density),
           density_min = coalesce(density_min_org, density_min),
           density_max = coalesce(density_max_org, density_max),
           density_loq = coalesce(density_loq_org, density_loq)) %>%
    select(-coarse_fragment_vol,
           -density_org,
           -density_min_org,
           -density_max_org,
           -density_loq_org)

  # Update overview of plot potentials

  if (unlist(strsplit(survey_form_i, "_"))[2] == "som") {

    df_summ <- bind_rows(
      df_below_ground %>%
        filter(!is.na(.data$density)) %>%
        filter(!is.na(.data$depth_top) &
                 !is.na(.data$depth_bottom)) %>%
        group_by(profile_id, unique_survey) %>%
        reframe(
          max_depth = max(depth_bottom, na.rm = TRUE),
          min_depth = min(depth_top, na.rm = TRUE),
          nlay_below_ground = n(),
          depth_stock = unique(depth_stock)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(
          stock_possible =
            ((nlay_below_ground >= 2 ||
                # If the soil depth is very shallow, one layer can be reliable,
                # e.g. so_som 1996_7_12_1
                (nlay_below_ground == 1 &&
                   max_depth >= 0.7 * depth_stock)) &&
               (min_depth == 0) &&
               (depth_stock > 0))) %>%
        ungroup() %>%
        group_by(unique_survey) %>%
        reframe(
          nlay_below_ground = max(nlay_below_ground),
          max_depth = max(max_depth),
          min_depth = min(min_depth),
          depth_stock = unique(depth_stock),
          stock_possible = any(stock_possible)) %>%
        ungroup() %>%
        mutate(
          reason_excl = ifelse(
            stock_possible == FALSE,
            case_when(
              depth_stock == 0 ~ "Continuous bedrock at 0 cm",
              min_depth > 0 ~ paste0("Insufficient data to calculate density ",
                                     "in upper below-ground layer"),
              # If not any of these cases, there should simply be insufficient
              # layers with data
              TRUE ~ paste0("Insufficient below-ground layers (as compared to ",
                            "soil depth) for which density can be calculated")),
            NA_character_)) %>%
        select(unique_survey, reason_excl),

      df_below_ground %>%
        group_by(profile_id, unique_survey) %>%
        reframe(
          no_toc = all(is.na(parameter_for_stock))) %>%
        ungroup() %>%
        group_by(unique_survey) %>%
        reframe(
          no_toc = all(no_toc == TRUE)) %>%
        ungroup() %>%
        filter(no_toc == TRUE) %>%
        mutate(reason_excl = paste0("No relevant '", parameter, "' data")) %>%
        select(unique_survey, reason_excl))


    plot_data_use_stocks <- plot_data_use_stocks %>%
      left_join(
        df_summ,
        by = "unique_survey") %>%
      mutate(
        reason_excluded = coalesce(reason_excluded,
                                   reason_excl)) %>%
      select(-reason_excl)

  }



  ## 2.2. Gap-fill subsoil density data ----


  ### 2.2.1. Assumption: constant subsoil ----

  # If it is okay to assume that the subsoil carbon density remains the same
  # gap-fill subsoil carbon density data

  if (constant_subsoil == TRUE) {

    cat(paste0(" \nGap-fill below-40-cm subsoil with the assumption that ",
               "the subsoil ", shorter_var_name,
               " density remains constant.\n"))

    source("./src/functions/harmonise_layer_to_depths.R")

    extra_rows <- NULL

    # Assume that the following depth range can be considered constant
    # (in line with the sampling requirements in ICP Forests)

    target_depth_range <- seq(40, 100, by = 1)

    # Set progress bar
    progress_bar <-
      txtProgressBar(min = 0,
                     max = length(unique(df_below_ground$profile_id)),
                     style = 3)

    for (i in seq_along(unique(df_below_ground$profile_id))) {

      prof_id_i <- as.character(unique(df_below_ground$profile_id)[i])

      df_profile_i <- df_below_ground %>%
        filter(profile_id == prof_id_i) %>%
        filter(!is.na(density)) %>%
        filter(!is.na(depth_top) &
                 !is.na(depth_bottom))

      if (nrow(df_profile_i) >= 1 &&
          any(df_profile_i$depth_top == 0)) {

      plot_id_i <- unique(df_profile_i$plot_id)

      depth_range <- df_profile_i %>%
        mutate(depth_sequence =
                 purrr::pmap(list(round(depth_top),
                                  round(depth_bottom)), seq, by = 1)) %>%
        pull(depth_sequence) %>%
        unlist %>%
        unique

      # Remove "bordering values"
      depth_range <- depth_range[-(c(which(diff(depth_range) != 1),
                                     which(diff(depth_range) != 1) + 1,
                                     length(depth_range)))]

      # Update target depth range
      target_depth_range <- seq(40, 100, by = 1)
      depth_stock_i <- round(unique(df_profile_i$depth_stock))

      if (depth_stock_i != max(target_depth_range) &&
          depth_stock_i > min(target_depth_range)) {

        target_depth_range <- seq(40, depth_stock_i, by = 1)
      }

      if (depth_stock_i <= min(target_depth_range)) {
        next
      }

      # Specify the depth range with missing density data

      depth_range_missing <-
        target_depth_range[!target_depth_range %in% depth_range]

      if (!identical(depth_range_missing, numeric(0))) {

        if (length(depth_range_missing) == 1) {
          depth_range_missing <- numeric(0)
        } else {

          if (diff(depth_range_missing)[1] > 1) {
            depth_range_missing <- depth_range_missing[-1]
          }

          if (!identical(depth_range_missing, numeric(0)) &&
              length(depth_range_missing) == 1) {
            depth_range_missing <- numeric(0)
          }

          if ((!identical(depth_range_missing, numeric(0))) &&
              (depth_range_missing[length(depth_range_missing)] == 100 &&
               depth_range_missing[length(depth_range_missing) - 1] < 99)) {
            depth_range_missing <-
              depth_range_missing[-length(depth_range_missing)]
          }
        }
      }

      if (!identical(depth_range_missing, numeric(0))) {

      # List the layers of other profiles of this plot that do contain
      # deeper data

      other_profile_ids_i <- df_below_ground %>%
        filter(plot_id == plot_id_i) %>%
        filter(profile_id != prof_id_i) %>%
        filter(!is.na(density)) %>%
        rowwise() %>%
        filter(any(depth_range_missing > .data$depth_top &
                     depth_range_missing < .data$depth_bottom)) %>%
        distinct(profile_id) %>%
        pull(profile_id)

      # If there are any other profiles to get subsoil data from

      if (!identical(other_profile_ids_i, character(0))) {

      df_sub_selected <- df_below_ground %>%
        filter(!is.na(density)) %>%
        filter(profile_id %in% other_profile_ids_i) %>%
        rowwise() %>%
        filter(any(depth_range_missing > .data$depth_top &
                     depth_range_missing < .data$depth_bottom)) %>%
        rename(layer_limit_superior = depth_top) %>%
        rename(layer_limit_inferior = depth_bottom) %>%
        select(code_layer,
               layer_limit_superior,
               layer_limit_inferior,
               bulk_density,
               coarse_fragment_vol_frac,
               layer_thickness,
               density,
               density_min,
               density_max)

      limit_sup <- max(min(df_sub_selected$layer_limit_superior),
                       min(depth_range_missing))

      limit_inf <- min(max(df_sub_selected$layer_limit_inferior),
                       max(depth_range_missing))

      density_i <-
        harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = "density")

      density_min_i <-
        harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = "density_min")

      density_max_i <-
        harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = "density_max")

      bulk_density_i <-
        harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = "bulk_density")

      coarse_fragment_vol_frac_i <-
        harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = "coarse_fragment_vol_frac")

      df_profile_i <- df_profile_i %>%
        mutate(diff_with_limit_sup = limit_sup - round(.data$depth_bottom))

      min_diff_with_limit_sup <- df_profile_i %>%
        filter(!is.na(diff_with_limit_sup) &
                 diff_with_limit_sup >= 0) %>%
        pull(diff_with_limit_sup) %>%
        min()

      layer_number_above <- df_profile_i$layer_number[
        which(df_profile_i$diff_with_limit_sup == min_diff_with_limit_sup)]

      if (layer_number_above != max(df_profile_i$layer_number)) {

        df_below_ground <- df_below_ground %>%
          mutate(layer_number =
                   ifelse(.data$profile_id == prof_id_i &
                            .data$layer_number > layer_number_above,
                          .data$layer_number + 1,
                          .data$layer_number))
      }

      extra_row_i <- df_profile_i %>%
        select(-diff_with_limit_sup) %>%
        filter(layer_number == layer_number_above) %>%
        mutate(code_layer = "X",
               layer_number = layer_number_above + 1,
               depth_top = limit_sup,
               depth_bottom = limit_inf,
               depth_avg = mean(c(limit_sup, limit_inf)),
               layer_thickness = diff(c(limit_sup, limit_inf)),
               bulk_density = bulk_density_i,
               coarse_fragment_vol_frac = coarse_fragment_vol_frac_i,
               density = density_i,
               density_min = density_min_i,
               density_max = density_max_i) %>%
        mutate(across(c("organic_layer_weight", "organic_layer_weight_min",
                        "organic_layer_weight_max", #"coarse_fragment_vol_frac",
                        "coarse_fragment_vol_min_frac",
                        "coarse_fragment_vol_max_frac",
                        "bulk_density_min",
                        "bulk_density_max",
                        "parameter_for_stock", "parameter_for_stock_min",
                        "parameter_for_stock_max"),
                      ~ NA_real_))

      extra_rows <- rbind(extra_rows,
                          extra_row_i)

      }
      }
      }

      # Update progress bar
      setTxtProgressBar(progress_bar, i)

    } # End of for loop along profiles

    close(progress_bar)

    df_below_ground <-
      bind_rows(df_below_ground %>%
                  mutate(gapfilled_post_layer1 = NA),
                extra_rows %>%
                  mutate(gapfilled_post_layer1 = "rule: constant below 40 cm"))

  } # End of "if constant subsoil is true"





    ### 2.2.2. Fixed density ----

  # Calculated based on available carbon densities and supplementary
  # partner data for this depth range
  # See "./src/stock_calculations/subsoil_c_densities.R"

  subsoil_densities_table <- data.frame(
    parameter = c("organic_carbon_total",
                  "n_total",
                  "extrac_p",
                  "extrac_s",
                  "rea_fe",
                  "rea_al",
                  "exch_ca"),
    density = c(0.30, # t C ha-1 cm-1
                0.0178, # t N ha-1 cm-1
                11.80, # kg P ha-1 cm-1 # Only based on five plots!
                17.91, # kg S ha-1 cm-1
                151.38, # kg Fe ha-1 cm-1
                75.27, # kg Al ha-1 cm-1
                0.107 # cmol(+) kg-1
                ),
    density_min = c(0.06, # 5 % quantile
                    0.0049,
                    2.11,
                    5.49,
                    2.87,
                    10.22,
                    0.002),
    density_max = c(2.58, # 95 % quantile
                    0.1198,
                    23.26,
                    24.09,
                    595.56,
                    193.34,
                    2.136))

  source("./src/functions/bulk_density_ptf.R")

  subsoil_toc <- 3.52 # g C kg-1 (based on LI)
  subsoil_bulk_density <- bd_ptf(subsoil_toc) # 1359 kg m-3
  subsoil_coarse_fragment_vol_frac  <- 0.1357 # mean based on LI; median: 0.0125

    # Currently only for a few parameters





    if (!parameter %in% subsoil_densities_table$parameter) {

      cat(paste0("Unknown subsoil density below 80 cm for '",
                 parameter,
                 "'. ",
                 "Densities below 80 cm will therefore not be gap-filled.\n"))

    } else

    if (parameter %in% subsoil_densities_table$parameter) {

      density_i <- subsoil_densities_table$density[which(
        subsoil_densities_table$parameter == parameter)] *
        density_convert_factor # t C ha-1 cm-1

      density_min_i <- subsoil_densities_table$density_min[which(
        subsoil_densities_table$parameter == parameter)] *
        density_convert_factor # 5 % quantile

      density_max_i <- subsoil_densities_table$density_max[which(
        subsoil_densities_table$parameter == parameter)] *
        density_convert_factor # 95 % quantile


      cat(paste0(" \nGap-fill below-80-cm subsoil with a fixed ",
                 shorter_var_name,
                 " density value (", density_i, " ", unit_density_per_cm,
                 ").\n"))

      source("./src/functions/harmonise_layer_to_depths.R")

      extra_rows <- NULL

      # Assume the following depth range that can be considered constant:
      target_depth_range <- seq(80, 100, by = 1)

      # Set progress bar
      progress_bar <-
        txtProgressBar(min = 0,
                       max = length(unique(df_below_ground$profile_id)),
                       style = 3)

      for (i in seq_along(unique(df_below_ground$profile_id))) {

        prof_id_i <- as.character(unique(df_below_ground$profile_id)[i])

        df_profile_i <- df_below_ground %>%
          filter(profile_id == prof_id_i) %>%
          filter(!is.na(density)) %>%
          filter(!is.na(depth_top) &
                   !is.na(depth_bottom))

        if ("gapfilled_post_layer1" %in% names(df_below_ground)) {
          df_profile_i <- df_profile_i %>%
            select(-gapfilled_post_layer1)
        }

        if (nrow(df_profile_i) >= 1 &&
            any(df_profile_i$depth_top == 0)) {

          plot_id_i <- unique(df_profile_i$plot_id)

          depth_range <- df_profile_i %>%
            mutate(depth_sequence =
                     purrr::pmap(list(round(depth_top),
                                      round(depth_bottom)), seq, by = 1)) %>%
            pull(depth_sequence) %>%
            unlist %>%
            unique

          # Remove "bordering values"
          depth_range <- depth_range[-(c(which(diff(depth_range) != 1),
                                         which(diff(depth_range) != 1) + 1,
                                         length(depth_range)))]

          # Update target depth range
          target_depth_range <- seq(80, 100, by = 1)
          depth_stock_i <- round(unique(df_profile_i$depth_stock))

          if (depth_stock_i != max(target_depth_range) &&
              depth_stock_i > min(target_depth_range)) {

            target_depth_range <- seq(80, depth_stock_i, by = 1)
          }

          if (depth_stock_i <= min(target_depth_range)) {
            next
          }

          # Specify the depth range with missing density data

          depth_range_missing <-
            target_depth_range[!target_depth_range %in% depth_range]

          if (!identical(depth_range_missing, numeric(0))) {

            if (length(depth_range_missing) == 1) {
              depth_range_missing <- numeric(0)
            } else {

              if (diff(depth_range_missing)[1] > 1) {
                depth_range_missing <- depth_range_missing[-1]
              }

              if (!identical(depth_range_missing, numeric(0)) &&
                  length(depth_range_missing) == 1) {
                depth_range_missing <- numeric(0)
              }

              if ((!identical(depth_range_missing, numeric(0))) &&
                  (depth_range_missing[length(depth_range_missing)] == 100 &&
                  depth_range_missing[length(depth_range_missing) - 1] < 99)) {
                depth_range_missing <-
                  depth_range_missing[-length(depth_range_missing)]
              }
            }
          }

          if (!identical(depth_range_missing, numeric(0))) {

            # List the layers of other profiles of this plot that do contain
            # deeper data

              limit_sup <- min(depth_range_missing)

              limit_inf <- max(depth_range_missing)


              df_profile_i <- df_profile_i %>%
                mutate(diff_with_limit_sup =
                         limit_sup - round(.data$depth_bottom))

              min_diff_with_limit_sup <- df_profile_i %>%
                filter(!is.na(diff_with_limit_sup) &
                         diff_with_limit_sup >= 0) %>%
                pull(diff_with_limit_sup) %>%
                min()

              layer_number_above <- df_profile_i$layer_number[
                which(df_profile_i$diff_with_limit_sup ==
                        min_diff_with_limit_sup)]

              if (layer_number_above != max(df_profile_i$layer_number)) {

                df_below_ground <- df_below_ground %>%
                  mutate(layer_number =
                           ifelse(.data$profile_id == prof_id_i &
                                    .data$layer_number > layer_number_above,
                                  .data$layer_number + 1,
                                  .data$layer_number))
              }

              # Only if the layer on top is mineral

              if (df_profile_i$layer_type[which(
                df_profile_i$layer_number == layer_number_above)] ==
                "mineral") {

              extra_row_i <- df_profile_i %>%
                select(-diff_with_limit_sup) %>%
                filter(layer_number == layer_number_above) %>%
                mutate(code_layer = "X",
                       layer_number = layer_number_above + 1,
                       depth_top = limit_sup,
                       depth_bottom = limit_inf,
                       depth_avg = mean(c(limit_sup, limit_inf)),
                       layer_thickness = diff(c(limit_sup, limit_inf)),
                       bulk_density = subsoil_bulk_density,
                       coarse_fragment_vol_frac =
                         subsoil_coarse_fragment_vol_frac,
                       density = density_i,
                       density_min = density_min_i,
                       density_max = density_max_i) %>%
                mutate(across(c("organic_layer_weight",
                                "organic_layer_weight_min",
                                "organic_layer_weight_max",
                                # "coarse_fragment_vol_frac",
                                "coarse_fragment_vol_min_frac",
                                "coarse_fragment_vol_max_frac",
                                "bulk_density_min",
                                "bulk_density_max",
                                "parameter_for_stock",
                                "parameter_for_stock_min",
                                "parameter_for_stock_max"),
                              ~ NA_real_))

              extra_rows <- rbind(extra_rows,
                                  extra_row_i)
              }

          }
        }

        # Update progress bar
        setTxtProgressBar(progress_bar, i)

      } # End of for loop along profiles

      close(progress_bar)

      if (!"gapfilled_post_layer1" %in% names(df_below_ground)) {
        df_below_ground$gapfilled_post_layer1 <- NA
      }

      df_below_ground <-
        bind_rows(df_below_ground,
                  extra_rows %>%
                    mutate(gapfilled_post_layer1 =
                             "rule: fixed value below 80 cm")) %>%
        arrange(partner_short,
                code_plot,
                survey_year,
                repetition,
                layer_number)


    } # End of "if parameter in list of parameters with known subsoil density"



  if ("gapfilled_post_layer1" %in% names(df_below_ground)) {

    cat(paste0(" \nSubsoils gap-filled for ",
               df_below_ground %>%
                 filter(!is.na(gapfilled_post_layer1)) %>%
                 distinct(profile_id) %>%
                 nrow,
               " profiles in '",
               survey_form_i, "'.\n",
               "--------------------------------------------------------\n"))

}






  # Save the df_below_ground dataset (all data, also missing data)

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_i, "_below_ground"),
               df_below_ground)
  }






  ## 2.3. Calculate below-ground stocks ----

  # Remark:
  # It is rare for carbon densities to be higher than 10 t C ha-1 cm-1.
  # However, issues that lead to extremes are supposed to be solved in layer 1.
  # Any remaining issues will be flagged as "implausible" in the final
  # plot survey-specific carbon stock dataset using the function
  # check_stock_plausibility.R.


  profile_stocks_below_ground <- NULL
  profile_stocks_below_ground_min <- NULL
  profile_stocks_below_ground_max <- NULL

  profile_list <- unique(df_below_ground$profile_id)

  cat(paste0(" \nCalculate below-ground stocks after fitting ",
             "mass-preserving splines.\n"))

  # Set progress bar
  progress_bar <- txtProgressBar(min = 0,
                                 max = length(profile_list), style = 3)

  # Evaluate for each of the profiles

  for (i in seq_along(profile_list)) {

    # Select profile

    df_profile_i <- df_below_ground %>%
      filter(profile_id == profile_list[i])

    # Define variables for calculation output

    prof_id_i <- as.character(unique(df_profile_i$profile_id))
    plot_id_i <- as.character(unique(df_profile_i$plot_id))

    assertthat::assert_that(length(unique(df_profile_i$depth_stock)) == 1)
    depth_stock_i <- unique(df_profile_i$depth_stock)

    # Prepare input for calculate_stocks function

    prof <- df_profile_i %>%
      select(plot_id,
             profile_id,
             code_layer,
             depth_top,
             depth_bottom,
             density,
             density_min,
             density_max,
             density_loq,
             bulk_density,
             coarse_fragment_vol_frac,
             depth_stock,
             gapfilled_post_layer1) %>%
      # Only calculate carbon stocks based on layers
      # for which the carbon density is known
      filter(!is.na(.data$density)) %>%
      # and for which their layer limits are known
      filter(!is.na(.data$depth_top) &
               !is.na(.data$depth_bottom))

    if (nrow(prof) == 0) {
      next
    }

    # Assert that all the depth layers are below-ground

      assertthat::assert_that(
        all(prof$depth_top >= 0) &&
          all(prof$depth_bottom >= 0),
        msg = paste0("Not all layer limits are below-ground ",
                     "(i.e. not below 0) for the profile '",
                     prof_id_i, "'."))

    # Issue:
    # Not possible to calculate splines in case of overlapping layers in
    # the profile
    # This is corrected in the get_layer_inconsistencies script,
    # but there may be new issues due to gap-filling of the subsoil,
    # i.e. typically 0.5 cm overlap because of rounding.
    # Solution: take average of the adjacent overlapping depth limits

    prof <- prof %>%
      mutate(prev_depth_bottom = lag(depth_bottom),
             next_depth_top = lead(depth_top)) %>%
      rowwise() %>%
      mutate(depth_bottom = ifelse(!is.na(.data$depth_bottom) &
                                     !is.na(.data$next_depth_top) &
                                     (.data$next_depth_top <
                                        .data$depth_bottom),
                                   mean(c(.data$depth_bottom,
                                          .data$next_depth_top)),
                                   .data$depth_bottom),
             depth_top = ifelse(!is.na(.data$depth_top) &
                                  !is.na(.data$prev_depth_bottom) &
                                  (.data$prev_depth_bottom > .data$depth_top),
                                mean(c(.data$depth_top,
                                       .data$prev_depth_bottom)),
                                .data$depth_top)) %>%
      ungroup() %>%
      select(-prev_depth_bottom,
             -next_depth_top)


    # Apply calculate_stocks and soilspline functions on profile

    # Only fit splines to profiles with:
    # - known carbon densities and layer limits for at least two layers OR
    #   for one layer, if this is the upper below-ground layer and if its
    #   lower layer limit is >= 0.7 * depth_stock
    # - and one of them should be the upper below-ground layer

    # If there are less than two layers or
    # if the highest layer is not the upper below-ground layer,
    # any stock calculations beyond the boundaries of the known layers
    # are simply too unreliable

      if ("gapfilled_post_layer1" %in% names(prof)) {

        # ! Uncertainty: is it justifiable to include the gap-filled
        # layer below 40 cm as one of the layers? Maybe better not...

        nlay_below_ground <- prof %>%
          # filter(is.na(gapfilled_post_layer1) |
          #          gapfilled_post_layer1 == "rule: constant below 40 cm") %>%
          filter(is.na(gapfilled_post_layer1)) %>%
          nrow

        max_depth_i <- prof %>%
          # filter(is.na(gapfilled_post_layer1) |
          #          gapfilled_post_layer1 == "rule: constant below 40 cm") %>%
          filter(is.na(gapfilled_post_layer1)) %>%
          pull(depth_bottom) %>%
          max

      } else {

        nlay_below_ground <- nrow(prof)

        max_depth_i <- max(prof$depth_bottom)
      }


    if ((nlay_below_ground >= 2 ||
         # If the soil depth is very shallow, one layer can be reliable,
         # e.g. so_som 1996_7_12_1
         (nlay_below_ground == 1 &&
          max_depth_i >= 0.7 * unique(prof$depth_stock))) &&
        (min(prof$depth_top) == 0) &&
        (unique(prof$depth_stock) > 0)) {

      # Mean stock

      profile_stock_output_i <-
        spline2stock(prof = prof,
                     variab_name = "density",
                     parameter_name = parameter,
                     survey_form = survey_form_i,
                     use_splines = TRUE,
                     density_per_three_cm = density_per_three_cm,
                     graph = graph)

      max_obs_depth <- prof %>%
        filter(is.na(.data$gapfilled_post_layer1)) %>%
        pull(depth_bottom) %>%
        max()




      # Calculate the mass_fine_earth (until 100 cm or eff_soil_depth
      # if shallower)
      # and the mass_fine_earth_topsoil (until 30 cm or eff_soil_depth
      # if shallower)

      # To do so, we won't use splines but LOCF or NOCB.
      # First, we need to add layers for missing depth ranges

      # Define the depth range to cover
      target_depth_range <- seq(0, 100)

      depth_range <- prof %>%
        mutate(depth_sequence =
                 purrr::pmap(list(round(depth_top),
                                  round(depth_bottom)), seq, by = 1)) %>%
        pull(depth_sequence) %>%
        unlist %>%
        unique

      # Remove "bordering values"
      depth_range <- depth_range[-(c(which(diff(depth_range) != 1),
                                     which(diff(depth_range) != 1) + 1,
                                     length(depth_range)))]

      # Specify the depth range with missing density data

      depth_range_missing <-
        target_depth_range[!target_depth_range %in% depth_range]

      if (!identical(depth_range_missing, numeric(0)) &&
          !identical(depth_range_missing, integer(0))) {

        if (length(depth_range_missing) == 1) {
          depth_range_missing <- numeric(0)
        } else {

          if (diff(depth_range_missing)[1] > 1) {
            depth_range_missing <- depth_range_missing[-1]
          }

          if (!identical(depth_range_missing, numeric(0)) &&
              !identical(depth_range_missing, integer(0)) &&
              length(depth_range_missing) == 1) {
            depth_range_missing <- numeric(0)
          }

          if ((!identical(depth_range_missing, numeric(0))) &&
              (depth_range_missing[length(depth_range_missing)] == 100 &&
               depth_range_missing[length(depth_range_missing) - 1] < 99)) {
            depth_range_missing <-
              depth_range_missing[-length(depth_range_missing)]
          }
        }
      }

      # Add layers if needed

      if (!identical(depth_range_missing, numeric(0)) &&
          !identical(depth_range_missing, integer(0))) {

        ind_gap_i <- which(diff(depth_range_missing) > 1)

        # if one layer is missing

        if (identical(ind_gap_i, integer(0))) {

          prof <- bind_rows(
            prof,
            prof[which(prof$depth_bottom == depth_range_missing[1]), ] %>%
              mutate(
                code_layer = "X",
                depth_top = depth_range_missing[1],
                depth_bottom =
                  depth_range_missing[length(depth_range_missing)]))

        } else {

          # if multiple layers are missing

          for (j in seq_len(length(ind_gap_i) + 1)) {

            if (j == 1) {
              depth_top_j <- depth_range_missing[1]
            } else {
              depth_top_j <- depth_range_missing[ind_gap_i[j - 1] + 1]
            }

            if (j == (length(ind_gap_i) + 1)) {
              depth_bottom_j <- depth_range_missing[length(depth_range_missing)]
            } else {
              depth_bottom_j <- depth_range_missing[ind_gap_i[j]]
            }

            prof <- bind_rows(
              prof,
              # There should always be a layer above it since the presence of
              # the upper mineral layer is a criterium for stocks
              prof[which(prof$depth_bottom == depth_top_j), ] %>%
                mutate(
                  code_layer = "X",
                  depth_top = depth_top_j,
                  depth_bottom = depth_bottom_j))

          } # End of for loop over extra layers
        }

        prof <- prof %>%
          arrange(depth_top)

      } # End of "if extra layers are needed"

      prof <- prof %>%
        filter(depth_top < depth_stock_i) %>%
        mutate(depth_bottom = ifelse(
          depth_bottom > depth_stock_i,
          depth_stock_i,
          depth_bottom))

      assertthat::assert_that(all(!is.na(prof$bulk_density)))
      assertthat::assert_that(all(!is.na(prof$coarse_fragment_vol_frac)))

      # mass_fine_earth (tot depth_stock) in kg fine earth m-2

      prof <- prof %>%
        mutate(
          layer_thickness = (depth_bottom - depth_top),
          bulk_density_total_soil =
            bulk_density * (1 - coarse_fragment_vol_frac),
          mass_fine_earth =
            layer_thickness * (1E-2) * bulk_density_total_soil)

      mass_fine_earth_i <- round(sum(prof$mass_fine_earth), 2)

      # mass_fine_earth_topsoil (tot 30 cm) in kg fine earth m-2

      prof <- prof %>%
        filter(depth_top < min(c(30, depth_stock_i))) %>%
        mutate(depth_bottom = ifelse(
          depth_bottom > min(c(30, depth_stock_i)),
          min(c(30, depth_stock_i)),
          depth_bottom)) %>%
        mutate(
          layer_thickness = (depth_bottom - depth_top),
          bulk_density_total_soil =
            bulk_density * (1 - coarse_fragment_vol_frac),
          mass_fine_earth =
            layer_thickness * (1E-2) * bulk_density_total_soil)

      mass_fine_earth_topsoil_i <- round(sum(prof$mass_fine_earth), 2)



      # use_stock_topsoil_i <- ifelse(max_obs_depth < 30 &
      #                                 max_obs_depth < 0.8 * depth_stock_i,
      #                               TRUE,
      #                               FALSE)

      profile_stocks_i <-
        data.frame(survey_form = unique(df_profile_i$survey_form),
                   partner_short = unique(df_profile_i$partner_short),
                   plot_id = plot_id_i,
                   profile_id = prof_id_i,
                   survey_year = unique(df_profile_i$survey_year),
                   partner_code = unique(df_profile_i$partner_code),
                   code_country = unique(df_profile_i$code_country),
                   code_plot = unique(df_profile_i$code_plot),
                   repetition = unique(df_profile_i$repetition),
                   contains_peat = any(df_profile_i$layer_type == "peat"),
                   obs_depth = max_obs_depth,
                   # use_stock_topsoil = use_stock_topsoil_i,
                   depth_stock = depth_stock_i,
                   mass_fine_earth = mass_fine_earth_i, # kg m-2
                   mass_fine_earth_topsoil = mass_fine_earth_topsoil_i, # kg m-2
                   profile_stock_output_i)

      profile_stocks_below_ground <-
        rbind(profile_stocks_below_ground,
              profile_stocks_i)

    }

    # Update progress bar
    setTxtProgressBar(progress_bar, i)

  } # End of for loop along profiles

  close(progress_bar)


  # Add minima and maxima stocks

  profile_stocks_below_ground <- profile_stocks_below_ground %>%
    relocate(stock_below_ground_min,
             .after = stock_below_ground) %>%
    relocate(stock_below_ground_max,
             .after = stock_below_ground_min) %>%
    relocate(stock_below_ground_topsoil_min,
             .after = stock_below_ground_topsoil) %>%
    relocate(stock_below_ground_topsoil_max,
             .after = stock_below_ground_topsoil_min)


  assertthat::assert_that(
    all(profile_stocks_below_ground$stock_below_ground >= 0 |
          is.na(profile_stocks_below_ground$stock_below_ground)))


  # Save output

  if (save_to_env == TRUE) {

    assign_env(paste0(survey_form_i,
                      "_profile_",
                      shorter_var_name,
                      "_stocks_below_ground"),
               profile_stocks_below_ground)

  }





  ## 2.4. Aggregate below-ground per plot ----

  parameters <- c("stock_below_ground",
                  "stock_below_ground_topsoil",
                  "stock_below_ground_1cm")

  grouping_cols <- c("survey_form", "partner_short", "partner_code",
                     "code_country", "code_plot",
                     "plot_id", "survey_year")

  numeric_grouping_cols <- profile_stocks_below_ground %>%
    summarise(across(all_of(grouping_cols), is.numeric)) %>%
    unlist()

  numeric_grouping_cols <-
    names(numeric_grouping_cols)[numeric_grouping_cols]


  plot_stocks_below_ground <- profile_stocks_below_ground %>%
    mutate(across(all_of(numeric_grouping_cols), as.character)) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise_per_group(variables_to_summarise = parameters,
                        mode = "mean",
                        digits = 2) %>%
    rowwise() %>%
    mutate(key = paste(c_across(all_of(grouping_cols)),
                       collapse = "_")) %>%
    left_join(
      profile_stocks_below_ground %>%
        filter(stock_below_ground > 0) %>%
        mutate(across(all_of(numeric_grouping_cols), as.character)) %>%
        group_by(across(all_of(grouping_cols))) %>%
        reframe(
          nlay_below_ground_min =
            min(nlay_below_ground, na.rm = TRUE),
          nlay_below_ground_max =
            max(nlay_below_ground, na.rm = TRUE),
          obs_depth_avg =
            round(mean(obs_depth, na.rm = TRUE), 1),
          depth_stock_avg =
            round(mean(depth_stock, na.rm = TRUE), 2),
          mass_fine_earth =
            round(mean(mass_fine_earth, na.rm = TRUE), 2),
          mass_fine_earth_topsoil =
            round(mean(mass_fine_earth_topsoil, na.rm = TRUE), 2),
          contains_peat =
            any(contains_peat == TRUE),
          # use_stock_topsoil =
          #   any(use_stock_topsoil == TRUE),
          rmse_mpspline_max =
            ifelse(any(!is.na(rmse_mpspline)),
                   max(rmse_mpspline, na.rm = TRUE),
                   NA_real_)) %>%
        rowwise() %>%
        mutate(key = paste(c_across(all_of(grouping_cols)),
                           collapse = "_")) %>%
        select(-all_of(grouping_cols)),
      by = "key") %>%
    select(-key) %>%
    arrange(partner_short,
            code_plot,
            survey_year) %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))



  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_i,
                      "_plot_",
                      shorter_var_name,
                      "_stocks_below_ground"),
               plot_stocks_below_ground)
  }




  cat(" \n--------------------------------------------------------\n")





  # 3. Forest floor layers ----
  ## 3.1. Derive layer-based dataset for forest floors ----

  if (parameter == "organic_carbon_total") {

  # Get gap-filling data for TOCs of forest floors

  # OF
  toc_of <- get_parameter_stats(parameter = "organic_carbon_total",
                                mode = "stat",
                                layer_type = "of")
  toc_of <- data.frame(
    mean = as.numeric(toc_of[which(names(toc_of) == "Mean")]),
    lower = as.numeric(toc_of[which(names(toc_of) == "5%")]),
    upper = as.numeric(toc_of[which(names(toc_of) == "95%")]))

  # OH
  toc_oh <- get_parameter_stats(parameter = "organic_carbon_total",
                                mode = "stat",
                                layer_type = "oh")
  toc_oh <- data.frame(
    mean = as.numeric(toc_oh[which(names(toc_oh) == "Mean")]),
    lower = as.numeric(toc_oh[which(names(toc_oh) == "5%")]),
    upper = as.numeric(toc_oh[which(names(toc_oh) == "95%")]))

  # Forest floor excluding OL
  toc_ff <- get_parameter_stats(parameter = "organic_carbon_total",
                                mode = "stat",
                                layer_type = "forest_floor_excl_ol")
  toc_ff <- data.frame(
    mean = as.numeric(toc_ff[which(names(toc_ff) == "Mean")]),
    lower = as.numeric(toc_ff[which(names(toc_ff) == "5%")]),
    upper = as.numeric(toc_ff[which(names(toc_ff) == "95%")]))

  }

  # TOC data below LOQ for the forest floor are highly implausible





  cat(paste0(" \nCalculate forest floor stocks by depth-integrating ",
             "layer stocks.\n"))

  df_forest_floor <- df_working %>%
    filter(layer_type == "forest_floor" |
             (is.na(depth_top)) |
             (depth_top < 0)) %>%
    # Filter out redundant layers
    # (i.e. layers not needed to compose the complete profile,
    # such as overlapping layers)
    filter(!is.na(.data$layer_number)) %>%
    # Remove all layers containing "L"
    # (since C in OL is not considered part of the soil organic carbon)
    filter(!grepl("L", code_layer)) %>%
    select(survey_form,
           partner_short,
           plot_id,
           unique_survey,
           profile_id,
           partner_code,
           code_country,
           code_plot,
           survey_year,
           repetition,
           code_layer,
           layer_type,
           layer_number,
           depth_top,
           depth_bottom,
           depth_avg,
           layer_thickness,
           depth_stock,
           bulk_density,
           bulk_density_min,
           bulk_density_max,
           organic_layer_weight,
           organic_layer_weight_min,
           organic_layer_weight_max,
           coarse_fragment_vol,
           coarse_fragment_vol_frac,
           coarse_fragment_vol_min_frac,
           coarse_fragment_vol_max_frac,
           parameter_for_stock,
           parameter_for_stock_min,
           parameter_for_stock_max) %>%
    # Remove values below LOQ (since highly implausible for forest floor),
    # e.g. for the UK in LI
    mutate(
      parameter_for_stock_min = ifelse(
        !is.na(parameter_for_stock) &
          parameter_for_stock < loq_org,
        NA_real_,
        parameter_for_stock_min),
      parameter_for_stock_max = ifelse(
        !is.na(parameter_for_stock) &
          parameter_for_stock < loq_org,
        NA_real_,
        parameter_for_stock_max),
      parameter_for_stock = ifelse(
        !is.na(parameter_for_stock) &
          parameter_for_stock < loq_org,
        NA_real_,
        parameter_for_stock))


  if (parameter == "organic_carbon_total") {

    df_forest_floor <- df_forest_floor %>%
      # Gap-fill TOCs
      mutate(
        parameter_for_stock_min = ifelse(
          !is.na(parameter_for_stock),
          parameter_for_stock_min,
          ifelse(
            # Do not gap-fill if organic_layer_weight is still unknown
            !is.na(organic_layer_weight),
            case_when(
              code_layer == "OF" ~ toc_of$lower,
              code_layer == "OH" ~ toc_oh$lower,
              .default = toc_ff$lower),
            NA_real_)),
        parameter_for_stock_max = ifelse(
          !is.na(parameter_for_stock),
          parameter_for_stock_max,
          ifelse(
            !is.na(organic_layer_weight),
            case_when(
              code_layer == "OF" ~ toc_of$upper,
              code_layer == "OH" ~ toc_oh$upper,
              .default = toc_ff$upper),
            NA_real_)),
        parameter_for_stock = ifelse(
          !is.na(parameter_for_stock),
          parameter_for_stock,
          ifelse(
            !is.na(organic_layer_weight),
            case_when(
              code_layer == "OF" ~ toc_of$mean,
              code_layer == "OH" ~ toc_oh$mean,
              .default = toc_ff$mean),
            NA_real_)))

  }



  df_forest_floor <- df_forest_floor %>%
    # Carbon stock per layer (t C ha-1 for each layer)
    # Units: g C/kg forest floor * kg forest floor/m2
    mutate(stock_layer =
             (.data$parameter_for_stock * density_convert_factor *
                .data$organic_layer_weight) /
             100) %>%
    mutate(stock_layer_min =
             (.data$parameter_for_stock_min * density_convert_factor *
                .data$organic_layer_weight_min) /
             100) %>%
    mutate(stock_layer_max =
             (.data$parameter_for_stock_max * density_convert_factor *
                .data$organic_layer_weight_max) /
             100) %>%
    # Carbon density (t C ha-1 cm-1)
    mutate(density =
             .data$stock_layer / .data$layer_thickness) %>%
    mutate(density_min =
             .data$stock_layer_min / .data$layer_thickness) %>%
    mutate(density_max =
             .data$stock_layer_max / .data$layer_thickness) %>%
    select(-coarse_fragment_vol)

  # Save the df_forest_floor dataset (all data, also missing data)

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_i, "_forest_floor"),
               df_forest_floor)
  }







  ## 3.2. Calculate forest floor carbon stocks ----

  profile_stocks_forest_floor <- NULL
  profile_list <- unique(df_forest_floor$profile_id)

  # Set progress bar
  progress_bar <- txtProgressBar(min = 0,
                                 max = length(profile_list), style = 3)

  for (i in seq_along(profile_list)) {

    # Select profile

    df_profile_i <- df_forest_floor %>%
      filter(profile_id == profile_list[i])

    if (nrow(df_profile_i) > 0) {

      if (all(!is.na(df_profile_i$stock_layer))) {

      # Combine stocks in forest floor per profile

      profile_stocks_i <-
        data.frame(survey_form = unique(df_profile_i$survey_form),
                   partner_short = unique(df_profile_i$partner_short),
                   plot_id = as.character(unique(df_profile_i$plot_id)),
                   profile_id = as.character(unique(df_profile_i$profile_id)),
                   survey_year = unique(df_profile_i$survey_year),
                   partner_code = unique(df_profile_i$partner_code),
                   code_country = unique(df_profile_i$code_country),
                   code_plot = unique(df_profile_i$code_plot),
                   repetition = unique(df_profile_i$repetition),
                   forest_floor_thickness =
                     ifelse(any(is.na(df_profile_i$layer_thickness)),
                            NA,
                            sum(df_profile_i$layer_thickness)),
                   nlay_forest_floor = length(df_profile_i$layer_number),
                   forest_floor_layers = paste(c(df_profile_i$code_layer),
                                               collapse = "_"),
                   # Cumulative carbon stock in forest floor
                   stock_forest_floor =
                     round(sum(df_profile_i$stock_layer), 2),
                   stock_forest_floor_min =
                     round(sum(df_profile_i$stock_layer_min), 2),
                   stock_forest_floor_max =
                     round(sum(df_profile_i$stock_layer_max), 2),
                   mass_forest_floor =
                     round(sum(df_profile_i$organic_layer_weight), 2))

      } else {

        # If not all "stock_layer" are known (due to unknown layer limits and
        # unknown organic_layer_weight):
        # forest floor stocks are NA

        profile_stocks_i <-
          data.frame(survey_form = unique(df_profile_i$survey_form),
                     partner_short = unique(df_profile_i$partner_short),
                     plot_id = as.character(unique(df_profile_i$plot_id)),
                     profile_id = as.character(unique(df_profile_i$profile_id)),
                     survey_year = unique(df_profile_i$survey_year),
                     partner_code = unique(df_profile_i$partner_code),
                     code_country = unique(df_profile_i$code_country),
                     code_plot = unique(df_profile_i$code_plot),
                     repetition = unique(df_profile_i$repetition),
                     forest_floor_thickness =
                       ifelse(any(is.na(df_profile_i$layer_thickness)),
                              NA,
                              sum(df_profile_i$layer_thickness)),
                     nlay_forest_floor = length(df_profile_i$layer_number),
                     forest_floor_layers = paste(c(df_profile_i$code_layer),
                                                 collapse = "_"),
                     # Cumulative carbon stock in forest floor: NA
                     stock_forest_floor = NA_real_,
                     # We know that it is minimally what is reported
                     stock_forest_floor_min = NA_real_,
                       # round(sum(df_profile_i$stock_layer_min, na.rm = TRUE),
                       #       2),
                     stock_forest_floor_max = NA_real_,
                     mass_forest_floor = NA_real_)


      }

      profile_stocks_forest_floor <-
        rbind(profile_stocks_forest_floor,
              profile_stocks_i)

    }

    # Update progress bar
    setTxtProgressBar(progress_bar, i)
  }

  close(progress_bar)

  # profile_stocks_forest_floor <- profile_stocks_forest_floor %>%
  #   mutate(unknown_forest_floor = ifelse(
  #     is.na(stock_forest_floor), TRUE, FALSE))

  assertthat::assert_that(
    all(profile_stocks_forest_floor$stock_forest_floor >= 0 |
          is.na(profile_stocks_forest_floor$stock_forest_floor)))

  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_i,
                      "_profile_",
                      shorter_var_name,
                      "_stocks_forest_floor"),
               profile_stocks_forest_floor)
  }





  ## 3.3. Aggregate forest floor per plot ----

  plot_stocks_forest_floor <-
    profile_stocks_forest_floor %>%
    group_by(survey_form, partner_short, partner_code,
             code_country, code_plot,
             plot_id, survey_year) %>%
    reframe(
      stock_forest_floor_min =
        ifelse(any(!is.na(stock_forest_floor)),
               round(min(stock_forest_floor_min, na.rm = TRUE), 2),
               NA_real_),
      stock_forest_floor_max =
        ifelse(any(!is.na(stock_forest_floor)),
               round(max(stock_forest_floor_max, na.rm = TRUE), 2),
               NA_real_),
      stock_forest_floor =
        ifelse(any(!is.na(stock_forest_floor)),
               round(mean(stock_forest_floor, na.rm = TRUE), 2),
               NA_real_),
      mass_forest_floor =
        ifelse(any(!is.na(mass_forest_floor)),
               round(mean(mass_forest_floor, na.rm = TRUE), 2),
               NA_real_),
      nlay_forest_floor_min =
        min(nlay_forest_floor, na.rm = TRUE),
      nlay_forest_floor_max =
        max(nlay_forest_floor, na.rm = TRUE),
      forest_floor_thickness_avg =
        round(mean(forest_floor_thickness, na.rm = TRUE), 2),
      forest_floor_layers_unique =
        ifelse(length(unique(forest_floor_layers)) == 1,
               unique(forest_floor_layers),
               NA_character_)) %>%
    relocate(stock_forest_floor, .before = stock_forest_floor_min) %>%
    # mutate(known_forest_floor = ifelse(
    #     !is.na(stock_forest_floor), TRUE, FALSE)) %>%
    arrange(partner_short,
            code_plot,
            survey_year) %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))

  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_i,
                      "_plot_",
                      shorter_var_name,
                      "_stocks_forest_floor"),
               plot_stocks_forest_floor)
  }


  cat(" \n--------------------------------------------------------\n")






  # 4. Combine below-ground and forest floor ----
  ## 4.1. Combine below-ground and forest floor layers ----


  df_layer <-
    bind_rows(df_below_ground %>%
                mutate(repetition = as.character(repetition),
                       stock_layer = NA_real_,
                       stock_layer_min = NA_real_,
                       stock_layer_max = NA_real_) %>%
                relocate(gapfilled_post_layer1, .after = stock_layer_max),
              df_forest_floor %>%
                relocate(density, .before = stock_layer) %>%
                relocate(density_min, .after = density) %>%
                relocate(density_max, .after = density_min) %>%
                mutate(repetition = as.character(repetition),
                       gapfilled_post_layer1 = NA_character_,
                       parameter_for_stock_loq = NA_real_,
                       density_loq = NA_real_) %>%
                relocate(parameter_for_stock_loq,
                         .after = parameter_for_stock_max) %>%
                relocate(density_loq, .after = density_max)) %>%
    mutate(profile_id_form = paste0(survey_form_i, "_",
                                    profile_id)) %>%
    relocate(profile_id_form, .after = profile_id) %>%
    arrange(partner_short,
            code_plot,
            survey_year,
            survey_form,
            profile_id,
            layer_number)


  # Save the df_below_ground dataset (all data, also missing data)

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_i, "_layers"),
               df_layer)
  }



  ## 4.2. Join below-ground and forest floor profile stocks ----

  cat(paste0(" \nCombine below-ground and forest floor stocks.\n"))

  profile_stocks <-
    profile_stocks_below_ground %>%
    left_join(profile_stocks_forest_floor,
              join_by(survey_form, partner_short, plot_id, profile_id,
                      survey_year, partner_code, code_country, code_plot,
                      repetition)) %>%
    mutate(stock =
             rowSums(select(., stock_below_ground, stock_forest_floor),
                     na.rm = TRUE),
           stock_min =
             rowSums(select(., stock_below_ground_min, stock_forest_floor_min),
                     na.rm = TRUE),
           stock_max =
             rowSums(select(., stock_below_ground_max, stock_forest_floor_max),
                     na.rm = TRUE),
           stock_topsoil =
             rowSums(select(., stock_below_ground_topsoil, stock_forest_floor),
                     na.rm = TRUE),
           stock_topsoil_min =
             rowSums(select(., stock_below_ground_topsoil_min,
                            stock_forest_floor_min),
                     na.rm = TRUE),
           stock_topsoil_max =
             rowSums(select(., stock_below_ground_topsoil_max,
                            stock_forest_floor_max),
                     na.rm = TRUE),
           stock_forest_floor_proxy =
             rowSums(select(., stock_below_ground_1cm, stock_forest_floor),
                     na.rm = TRUE),
           stock_forest_floor_proxy_min =
             rowSums(select(., stock_below_ground_1cm_min,
                            stock_forest_floor_min),
                     na.rm = TRUE),
           stock_forest_floor_proxy_max =
             rowSums(select(., stock_below_ground_1cm_max,
                            stock_forest_floor_max),
                     na.rm = TRUE),
           nlay = rowSums(select(., nlay_below_ground, nlay_forest_floor),
                          na.rm = TRUE)) %>%
    select(-contains("stock_below_ground_1cm")) %>%
    relocate(obs_depth, depth_stock, forest_floor_thickness,
             mass_fine_earth, mass_fine_earth_topsoil, mass_forest_floor,
             nlay, nlay_below_ground, nlay_forest_floor, forest_floor_layers,
             rmse_mpspline,
             .after = stock_forest_floor_proxy_max) %>%
    relocate(starts_with("stock_") & matches("\\d+$"), .after = last_col()) %>%
    relocate(starts_with("density_"), .after = last_col())


  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_i,
                      "_profile_",
                      shorter_var_name,
                      "_stocks"),
               profile_stocks)
  }





  ## 4.3. Aggregate per plot ----


  # Combine plot average for forest floor with plot average for below-ground
  # rather than taking the average of the different profile stocks

  plot_stocks <- plot_stocks_below_ground %>%
    mutate(unique_survey = paste0(code_country, "_",
                                  survey_year, "_",
                                  code_plot),
           partner_code = as.numeric(partner_code),
           code_country = as.numeric(code_country),
           code_plot = as.numeric(code_plot),
           survey_year = as.numeric(survey_year)) %>%
    left_join(
      plot_stocks_forest_floor %>%
        mutate(unique_survey = paste0(code_country, "_",
                                      survey_year, "_",
                                      code_plot)),
      by = join_by(survey_form, partner_short, partner_code,
                   code_country, code_plot, plot_id, survey_year,
                   unique_survey)) %>%
    rowwise() %>%
    mutate(stock =
             sum(c(stock_below_ground, stock_forest_floor),
                 na.rm = TRUE),
           stock_min =
             sum(c(stock_below_ground_min,
                   stock_forest_floor_min),
                 na.rm = TRUE),
           stock_max =
             sum(c(stock_below_ground_max,
                   stock_forest_floor_max),
                 na.rm = TRUE),
           stock_topsoil =
             sum(c(stock_below_ground_topsoil,
                   stock_forest_floor),
                 na.rm = TRUE),
           stock_topsoil_min =
             sum(c(stock_below_ground_topsoil_min,
                   stock_forest_floor_min),
                 na.rm = TRUE),
           stock_topsoil_max =
             sum(c(stock_below_ground_topsoil_max,
                   stock_forest_floor_max),
                 na.rm = TRUE),
           stock_forest_floor_proxy =
             sum(c(stock_below_ground_1cm, stock_forest_floor),
                 na.rm = TRUE),
           stock_forest_floor_proxy_min =
             sum(c(stock_below_ground_1cm_min,
                   stock_forest_floor_min),
                 na.rm = TRUE),
           stock_forest_floor_proxy_max =
             sum(c(stock_below_ground_1cm_max,
                   stock_forest_floor_max),
                 na.rm = TRUE),
           nlay_min =
             sum(c(nlay_below_ground_min,
                   nlay_forest_floor_min),
                 na.rm = TRUE),
           nlay_max =
             sum(c(nlay_below_ground_max,
                   nlay_forest_floor_max),
                 na.rm = TRUE)) %>%
    select(-contains("stock_below_ground_1cm")) %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%
    arrange(partner_short,
            code_plot,
            survey_year) %>%
    select(-unique_survey) %>%
    relocate(contains("stock"), .after = survey_year) %>%
    relocate(contains("stock_topsoil"), .after = stock_max) %>%
    relocate(contains("stock_below_ground"), .after = stock_topsoil_max) %>%
    relocate(contains("stock_forest_floor"),
             .after = stock_below_ground_topsoil_max) %>%
    relocate(contains("stock_forest_floor_proxy"),
             .after = stock_forest_floor_max) %>%
    # relocate(use_stock_topsoil, .after = survey_year) %>%
    relocate(contains_peat, .after = survey_year) %>%
    # relocate(known_forest_floor, .after = use_stock_topsoil) %>%
    # relocate(contains_peat, .after = known_forest_floor) %>%
    relocate(contains("nlay"), .after = stock_forest_floor_proxy_max) %>%
    relocate(contains("nlay_below_ground"), .after = nlay_max) %>%
    relocate(contains("nlay_forest_floor"), .after = nlay_below_ground_max) %>%
    relocate(depth_stock_avg, .after = stock_forest_floor_proxy_max) %>%
    relocate(obs_depth_avg, .after = depth_stock_avg) %>%
    relocate(rmse_mpspline_max, .after = forest_floor_layers_unique)



  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_i,
                      "_plot_",
                      shorter_var_name,
                      "_stocks"),
               plot_stocks)
  }



  cat(" \n--------------------------------------------------------\n")

  # Number of plots with stocks

  cat(paste0(" \nNumber of plots in '",
             survey_form_i,
             "' with below-ground '",
             parameter,
             "' stocks:\n"))

  print(plot_stocks %>%
          distinct(plot_id) %>%
          nrow)

  # Number of plots with stocks for at least two surveys

  cat(paste0(" \n",
             "Number of plots in '",
             survey_form_i,
             "' with below-ground '",
             parameter,
             "' stocks for at least two survey years:\n"))

  print(plot_stocks %>%
          group_by(plot_id) %>%
          summarise(n = n()) %>%
          filter(n > 1) %>%
          nrow)



} # End of loop over survey_forms ----






  # Layers: compile and save data ----

if (length(survey_forms) == 2) {

  df_layer <-
    bind_rows(get_env(paste0(code_survey, "_som_layers")),
              get_env(paste0(code_survey, "_pfh_layers"))) %>%
    arrange(partner_short,
            code_plot,
            survey_year,
            survey_form,
            profile_id,
            layer_number)

}

  # Save the df_layer dataset

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_orig, "_layers"),
               df_layer)
  }

  if (save_to_gdrive == TRUE) {
    save_to_google_drive(objects_to_save =
                           paste0(survey_form_orig, "_layers"),
                         df_object_to_save = format_stocks(df_layer),
                         path_name = "./output/stocks/")
  }




  write.table(df_layer,
              file = paste0(dir,
                            survey_form,
                            "_layers.csv"),
              row.names = FALSE,
              na = "",
              sep = ";",
              dec = ".")






  # Profile stocks: compile and save data ----

  if (length(survey_forms) == 2) {

    profile_stocks <-
      bind_rows(get_env(paste0(code_survey, "_som_profile_",
                                  shorter_var_name, "_stocks")) %>%
                  mutate(repetition = as.character(repetition)),
                get_env(paste0(code_survey, "_pfh_profile_",
                                  shorter_var_name, "_stocks")) %>%
                  mutate(repetition = as.character(repetition))) %>%
      arrange(partner_short,
              code_plot,
              survey_year,
              survey_form,
              profile_id)
  }

  profile_stocks <- profile_stocks %>%
    as_tibble() %>%
    mutate(profile_id_form = paste0(survey_form, "_",
                                    profile_id)) %>%
    relocate(profile_id_form, .after = profile_id)


  # Save the profile_stocks dataset

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_orig, "_profile_",
                      shorter_var_name, "_stocks"),
               profile_stocks)
  }

  if (save_to_gdrive == TRUE) {
    save_to_google_drive(objects_to_save =
                           paste0(survey_form_orig, "_profile_",
                                  shorter_var_name, "_stocks"),
                         df_object_to_save = format_stocks(profile_stocks),
                         path_name = "./output/stocks/")
  }

  write.table(format_stocks(profile_stocks),
              file = paste0(dir,
                            survey_form,
                            "_profile_",
                            shorter_var_name, "_stocks.csv"),
              row.names = FALSE,
              na = "",
              sep = ";",
              dec = ".")








  # Plot stocks: compile and save data ----

  if (length(survey_forms) == 2) {

    plot_stocks <-
      bind_rows(get_env(paste0(code_survey, "_som_plot_",
                                  shorter_var_name, "_stocks")),
                get_env(paste0(code_survey, "_pfh_plot_",
                                  shorter_var_name, "_stocks"))) %>%
      arrange(partner_short,
              code_plot,
              survey_year,
              survey_form)
  }

  plot_stocks <- plot_stocks %>%
    as_tibble() %>%
    mutate(plot_id_form = paste0(survey_form, "_",
                                    plot_id)) %>%
    relocate(plot_id_form, .after = plot_id)


  if (add_stratifiers == TRUE) {

    plot_stocks <- plot_stocks %>%
      left_join(df_strat %>%
                  select(-contains("eff_soil_depth")),
                by = "plot_id")

  }

  if (add_plausible_fscc == TRUE) {

    plot_stocks <- check_stock_plausibility(data_frame = plot_stocks,
                                            parameter = parameter,
                                            shorter_var_name = shorter_var_name,
                                            survey_form_orig = survey_form_orig)

  }


  # Save the plot_stocks dataset

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form_orig, "_plot_",
                      shorter_var_name, "_stocks"),
               plot_stocks)
  }

  if (save_to_gdrive == TRUE) {
    save_to_google_drive(objects_to_save =
                           paste0(survey_form_orig, "_plot_",
                                  shorter_var_name, "_stocks"),
                         df_object_to_save = format_stocks(plot_stocks),
                         path_name = "./output/stocks/")
  }

  write.table(format_stocks(plot_stocks),
              file = paste0(dir,
                            survey_form_orig,
                            "_plot_",
                            shorter_var_name, "_stocks.csv"),
              row.names = FALSE,
              na = "",
              sep = ";",
              dec = ".")


  source("./src/functions/create_attribute_catalogue.R")

  create_attribute_catalogue(data_frame =
                               format_stocks(plot_stocks),
                             path_to_save = paste0(dir,
                                                   "plot_",
                                                   shorter_var_name,
                                                   "_stocks_"))



  # Plot overview

  if (any(grepl("som", survey_forms))) {

    if (add_plausible_fscc == TRUE) {

    unique_surveys_stocks <- plot_stocks %>%
      filter(grepl("som", survey_form)) %>%
        filter(stock_plaus == TRUE) %>%
      mutate(unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot)) %>%
      distinct(unique_survey) %>%
      pull(unique_survey)

    unique_surveys_stocks_implaus <- plot_stocks %>%
      filter(grepl("som", survey_form)) %>%
      mutate(unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot)) %>%
      filter(stock_plaus == FALSE) %>%
      mutate(reason = paste0("Implausible stock: ",
                             implaus_fscc_reason)) %>%
      as_tibble %>%
      select(unique_survey, reason)

    unique_surveys_plaus_cov <- plot_stocks %>%
      filter(grepl("som", survey_form)) %>%
      mutate(unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot)) %>%
      mutate(cov = case_when(
        stock_plaus == TRUE ~
          "Complete (plausible for forest floor + topsoil + subsoil)",
        stock_topsoil_plaus == TRUE ~
          "Incomplete (plausible for forest floor + topsoil, not for subsoil)",
        stock_below_ground_plaus == TRUE ~
          "Incomplete (plausible for topsoil + subsoil, not for forest floor)",
        stock_below_ground_topsoil_plaus == TRUE ~
          paste0("Incomplete (plausible for topsoil, ",
                 "not for forest floor and subsoil)"),
        .default = NA_character_)) %>%
      as_tibble %>%
      select(unique_survey, cov)

    unique_surveys_simultaneous_pfh <- plot_stocks %>%
      filter(grepl("pfh", survey_form)) %>%
      mutate(unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot)) %>%
      filter(stock_plaus == TRUE) %>%
      pull(unique_survey)

    plot_data_use_stocks <- plot_data_use_stocks %>%
      left_join(unique_surveys_stocks_implaus,
                by = "unique_survey") %>%
      mutate(
        plaus_stock_calculated =
               (unique_survey %in% unique_surveys_stocks),
        reason_excluded = coalesce(reason_excluded,
                                   reason)) %>%
      select(-reason) %>%
      left_join(unique_surveys_plaus_cov,
                by = "unique_survey") %>%
      mutate(plaus_stock_coverage = coalesce(plaus_stock_coverage,
                                             cov)) %>%
      select(-cov) %>%
      mutate(
        plaus_stock_pfh_simultaneous =
          (unique_survey %in% unique_surveys_simultaneous_pfh))
    }

    plot_data_use_stocks <- plot_data_use_stocks %>%
      rename(
        !!paste0(parameter, "_source") := parameter_for_stock_source)

    write.table(plot_data_use_stocks,
                file = paste0(dir,
                              survey_form_orig, "_",
                              shorter_var_name, "_stock_potential_summary",
                              ".csv"),
                row.names = FALSE,
                na = "",
                sep = ";",
                dec = ".")

    source("./src/functions/create_attribute_catalogue.R")

    create_attribute_catalogue(data_frame =
                                 plot_data_use_stocks,
                               path_to_save = paste0(dir,
                                                     survey_form_orig, "_",
                                                     shorter_var_name,
                                                     "_stock_potential_",
                                                     "summary_"))

  }













  # Create graphs per plot ----

  # Calculate the estimated thickness of forest floor layers (and the
  # resulting variable density) based on pedotransfer bulk density
  # estimates for forest floor layers.
  # This will be used to visualise the (estimated) variable densities
  # including the forest floor

  # Retrieve the representative bulk density for forest floor layers

  bd_ff <- get_parameter_stats(parameter = "bulk_density",
                               mode = "stat",
                               layer_type = "forest_floor_excl_ol")

  bd_ff <- as.numeric(bd_ff[which(names(bd_ff) == "Mean")])












  if (graph == TRUE) {

  # Estimate layer thicknesses of forest floor layers with unknown layer limits

  df_layer_gapfilled <- df_layer %>%
    mutate(
      # Gap-filled layer_thickness
      layer_thickness_gf = coalesce(
        layer_thickness,
        # Thickness of the layer
        organic_layer_weight / (bd_ff * 1E-2)),
      # Gap-filled density
      density_gf = round(coalesce(
        density,
        ifelse(
          layer_type == "forest_floor" &
            is.na(layer_thickness) &
            !is.na(stock_layer),
          .data$stock_layer / .data$layer_thickness_gf,
        NA_real_)), 2),
      density_min_gf = round(coalesce(
        density_min,
        ifelse(
          layer_type == "forest_floor" &
            is.na(layer_thickness) &
            !is.na(stock_layer_min),
        .data$stock_layer_min / .data$layer_thickness_gf,
        NA_real_)), 2),
      density_max_gf = round(coalesce(
        density_max,
        ifelse(
          layer_type == "forest_floor" &
            is.na(layer_thickness) &
            !is.na(stock_layer_max),
        .data$stock_layer_max / .data$layer_thickness_gf,
        NA_real_)), 2),
      layer_thickness_gf = round(layer_thickness_gf, 1)) %>%
    filter(!is.na(density_gf))


  # Gap-fill layer limits

  vec <- which(is.na(df_layer_gapfilled$depth_top))

  for (j in sort(vec, decreasing = TRUE)) {

    if (df_layer_gapfilled$profile_id_form[j + 1] !=
        df_layer_gapfilled$profile_id_form[j]) {
      # For example the case when there are only forest floor layers in
      # a certain profile (e.g. "so_som_1993_1_40_1")

      next
    }

    if (is.na(df_layer_gapfilled$depth_top[j + 1])) {
      # For example the case when there are more than one layers and
      # all layers are forest floor layers without layer limits
      # (e.g. "s1_som_1988_55_1156_1")

      next
    }

    assertthat::assert_that(
      !is.na(df_layer_gapfilled$layer_thickness_gf[j]))

    if (is.na(df_layer_gapfilled$depth_bottom[j])) {

      df_layer_gapfilled$depth_bottom[j] <-
        round(df_layer_gapfilled$depth_top[j + 1], 1)

    }

    if (is.na(df_layer_gapfilled$depth_top[j])) {

      df_layer_gapfilled$depth_top[j] <-
        round(df_layer_gapfilled$depth_bottom[j] -
        df_layer_gapfilled$layer_thickness_gf[j], 1)
    }
  }

  df_layer_gapfilled <- df_layer_gapfilled %>%
    filter(!is.na(depth_top))


  ## Reorganise profile stocks ----

  profile_stocks_long <- pivot_longer(
    profile_stocks,
    cols = starts_with("density_"),
    names_to = "depth",
    names_prefix = "density_",
    values_to = "density") %>%
    mutate(depth = as.numeric(depth),
           repetition = as.character(repetition)) %>%
    select(survey_form, code_country, code_plot, plot_id, survey_year,
           repetition, profile_id, profile_id_form, depth_stock,
           stock, stock_below_ground, stock_below_ground_topsoil,
           depth, density) %>%
    filter(!is.na(density)) %>%
    mutate(density_gf = density) %>%
    # Forest floor
    bind_rows(
      # Upper layer limit forest floor
      df_layer_gapfilled %>%
        filter(layer_type == "forest_floor") %>%
        filter(profile_id_form %in% profile_stocks$profile_id_form) %>%
        left_join(profile_stocks %>%
                    select(profile_id_form, stock, stock_below_ground,
                           stock_below_ground_topsoil),
                  by = "profile_id_form") %>%
        mutate(depth = depth_top) %>%
        select(survey_form, code_country, code_plot, plot_id, survey_year,
               repetition, profile_id, profile_id_form, depth_stock,
               stock, stock_below_ground, stock_below_ground_topsoil,
               depth, density, density_gf)) %>%
    bind_rows(
      # Upper layer limit forest floor
      df_layer_gapfilled %>%
        filter(layer_type == "forest_floor") %>%
        filter(profile_id_form %in% profile_stocks$profile_id_form) %>%
        left_join(profile_stocks %>%
                    select(profile_id_form, stock, stock_below_ground,
                           stock_below_ground_topsoil),
                  by = "profile_id_form") %>%
        mutate(depth = ifelse(!is.na(depth_top),
                              depth_bottom - 0.1,
                              NA_real_)) %>%
        select(survey_form, code_country, code_plot, plot_id, survey_year,
               repetition, profile_id, profile_id_form, depth_stock,
               stock, stock_below_ground, stock_below_ground_topsoil,
               depth, density, density_gf))

  profile_stocks_long <- profile_stocks_long %>%
    bind_rows(
      # Add data for 0-cm depth
      profile_stocks_long %>%
        filter(depth == 1) %>%
        mutate(depth = 0)) %>%
    arrange(code_country,
            code_plot,
            survey_year,
            survey_form,
            profile_id,
            depth) %>%
    mutate(unique_survey = paste0(code_country, "_",
                                  survey_year, "_",
                                  code_plot))


  assertthat::assert_that(
    all(!is.na(profile_stocks_long$density_gf)))



  # Set progress bar
  progress_bar <- txtProgressBar(min = 0,
                                 max = length(unique(profile_stocks$plot_id)),
                                 style = 3)

  ## Evaluate over plot_ids ----

 for (i in seq_along(unique(profile_stocks$plot_id))) {

    plot_id_i <- unique(profile_stocks$plot_id)[i]

    survey_years_i <- profile_stocks %>%
      filter(plot_id == plot_id_i) %>%
      distinct(survey_year) %>%
      arrange(survey_year) %>%
      pull(survey_year)

    df_strat_i <- df_strat %>%
      filter(plot_id == plot_id_i) %>%
      as.data.frame

    depth_stock_i <- plot_stocks %>%
      filter(plot_id == plot_id_i) %>%
      distinct(depth_stock_avg) %>%
      pull(depth_stock_avg)


    ## Plausible stocks ----

    plaus_stocks_i <- profile_stocks %>%
      left_join(df_strat,
                by = "plot_id")

    if (!is.na(df_strat_i$wrb_ref_soil_group)) {

      plaus_stocks_i <- plaus_stocks_i %>%
        filter(wrb_ref_soil_group == df_strat_i$wrb_ref_soil_group)
    }

    if (!is.na(df_strat_i$eftc)) {

      plaus_stocks_i <- plaus_stocks_i %>%
        filter(eftc == df_strat_i$eftc)
    }

    if (nrow(plaus_stocks_i) > 0) {

      if (nrow(plaus_stocks_i) >= 5) {

        quantile_i <- quantile(plaus_stocks_i$stock,
                               c(0.025, 0.975))

        plaus_stocks_i <- plaus_stocks_i %>%
          filter(stock >= quantile_i[1] &
                   stock <= quantile_i[2]) %>%
          pull(profile_id_form)

      } else {

        plaus_stocks_i <- plaus_stocks_i %>%
          pull(profile_id_form)
      }

      plaus_stocks_min_i <- profile_stocks_long %>%
        filter(profile_id_form %in% plaus_stocks_i) %>%
        filter(stock == min(.data$stock))

      plaus_stocks_max_i <- profile_stocks_long %>%
        filter(profile_id_form %in% plaus_stocks_i) %>%
        filter(stock == max(.data$stock))

      plaus_stocks_i <-
        data.frame(depth = sort(unique(c(plaus_stocks_min_i$depth,
                                         plaus_stocks_max_i$depth)))) %>%
        left_join(plaus_stocks_min_i %>%
                    mutate(density_min = density_gf) %>%
                    select(depth, density_min),
                  by = "depth") %>%
        left_join(plaus_stocks_max_i %>%
                    rename(density_max = density_gf) %>%
                    select(depth, density_max),
                  by = "depth")

      if (max(plaus_stocks_i$depth) > depth_stock_i) {

        plaus_stocks_i <- plaus_stocks_i %>%
          filter(depth <= depth_stock_i)

        assertthat::assert_that(
          abs(diff(c(max(plaus_stocks_i$depth), depth_stock_i))) <= 3)

        plaus_stocks_i <- plaus_stocks_i %>%
          mutate(
            depth = ifelse(
              depth == max(depth, na.rm = TRUE),
              depth_stock_i,
              depth))
      }

    } else {
      plaus_stocks_i <- NULL
    }

    depth_range_j <- c(0,
                       profile_stocks_long %>%
                         filter(plot_id == plot_id_i) %>%
                         distinct(depth) %>%
                         arrange(depth) %>%
                         pull(depth),
                       depth_stock_i)

    depth_range_j <- (1) * sort(unique(c(
      0,
      seq(0, min(depth_range_j), by = -20),
      round(min(depth_range_j)),
      seq(0, max(depth_range_j), by = 20),
      round(max(depth_range_j)))))


    mylist <- list()


    ## Evaluate over survey years ----

    for (j in seq_along(survey_years_i)) {

      profiles_j <- profile_stocks_long %>%
        filter(plot_id == plot_id_i) %>%
        filter(survey_year == survey_years_i[j]) %>%
        distinct(profile_id_form, .keep_all = TRUE) %>%
        select(profile_id_form, profile_id, survey_form) %>%
        mutate(col = case_when(
          grepl("som", survey_form) ~ "#CC3300",
          grepl("pfh", survey_form) ~ "#570000"))

      profiles_other_j <- profile_stocks_long %>%
        filter(plot_id == plot_id_i) %>%
        filter(survey_year != survey_years_i[j]) %>%
        distinct(profile_id_form, .keep_all = TRUE) %>%
        select(profile_id_form, profile_id, survey_form)

      stock_j <- plot_stocks %>%
        filter(plot_id == plot_id_i) %>%
        filter(survey_year == survey_years_i[j])

      if (nrow(stock_j) > 1) {

        assertthat::assert_that(nrow(stock_j) == 2)

        stock_j <- stock_j %>%
          filter(grepl("som", survey_form))
      }


      stock_j <- stock_j %>%
        mutate(
          stock_final = case_when(
            stock_plaus == TRUE ~ stock,
            stock_topsoil_plaus == TRUE ~ stock_topsoil,
            stock_below_ground_plaus == TRUE ~ stock_below_ground,
            stock_below_ground_topsoil_plaus == TRUE ~
              stock_below_ground_topsoil,
            # If implausible
            TRUE ~ stock),
          label = case_when(
            stock_plaus == TRUE ~
              paste0("(*", survey_form, "*)"),
            stock_topsoil_plaus == TRUE ~
              paste0("(*", survey_form, "* · excl. subsoil)"),
            stock_below_ground_plaus == TRUE ~
              paste0("(*", survey_form, "* · excl. forest floor)"),
            stock_below_ground_topsoil_plaus ~
              paste0("(*", survey_form, "* · excl. forest floor and subsoil)"),
            # If implausible
            TRUE ~ paste0("(*", survey_form,
                          "* · stock considered implaus.)")))

      ## Graph ----

      p_j <- ggplot()

      if (!is.null(plaus_stocks_i)) {

        p_j <- p_j +
          geom_ribbon(data = plaus_stocks_i,
                      aes(ymin = density_min,
                          ymax = density_max,
                          x = (1) * depth),
                      color = NA,
                      fill = "#D8E0E0",
                      alpha = 1)
      }

      # If subsoil is implausible: draw line at 30 cm

      if (stock_j$stock_below_ground_topsoil_plaus == TRUE &&
          stock_j$stock_below_ground_plaus == FALSE) {

        p_j <- p_j +
          geom_vline(xintercept = 30, linetype = "dashed",
                     color = "black", linewidth = 1)

      }


      if (nrow(profiles_other_j) > 0) {

        for (k in seq_len(nrow(profiles_other_j))) {

          p_j <- p_j +
            geom_line(data = profile_stocks_long %>%
                        filter(profile_id_form ==
                                 profiles_other_j$profile_id_form[k]),
                      aes(y = density_gf,
                          x = (1) * depth),
                      color = "grey",
                      linetype = "21",
                      linewidth = 1) +
            geom_line(data = profile_stocks_long %>%
                        filter(profile_id_form ==
                                 profiles_other_j$profile_id_form[k]) %>%
                        filter(!is.na(density)),
                      aes(y = density,
                          x = (1) * depth),
                      color = "grey",
                      linewidth = 1)
        }
      }

      for (k in seq_len(nrow(profiles_j))) {

        col_k <- profiles_j$col[k]

        p_j <- p_j +
          geom_line(data = profile_stocks_long %>%
                      filter(profile_id_form == profiles_j$profile_id_form[k]),
                    aes(y = density_gf,
                        x = (1) * depth),
                    color = col_k,
                    linetype = "21",
                    linewidth = 1) +
          geom_line(data = profile_stocks_long %>%
                      filter(profile_id_form ==
                               profiles_j$profile_id_form[k]) %>%
                      filter(!is.na(density)),
                    aes(y = density,
                        x = (1) * depth),
                    color = col_k,
                    linewidth = 1)
      }




      # p_j <- p_j +
      #   scale_color_identity(breaks = c("#CC3300", "#570000", "grey"),
      #                        labels = c("Fixed-depth layers",
      #                                   "Pedogenic horizons",
      #                                   "Other survey years"),
      #                        guide = "legend",
      #                        name = NULL)

      # Bottom left
      if (j %% 2 > 0 & j > (length(survey_years_i) - 2)) {

        p_j <- p_j +
          labs(y = paste0("**", shorter_var_name, " density**<br>(",
                          unit_density_per_cm_markdown, ")"),
              #y = "**C density**<br>(t ha<sup>-1</sup> cm<sup>-1</sup>)",
               x = "**Depth**<br>(cm)")

      # Left but not bottom
      } else if (j %% 2 > 0 & j <= (length(survey_years_i) - 2)) {

        p_j <- p_j +
          labs(y = NULL,
               x = "**Depth**<br>(cm)")

      # Right bottom
      } else if (j %% 2 == 0 & j > (length(survey_years_i) - 2)) {

        p_j <- p_j +
          labs(y = paste0("**", shorter_var_name, " density**<br>(",
                          unit_density_per_cm_markdown, ")"),
               #y = "**C density**<br>(t ha<sup>-1</sup> cm<sup>-1</sup>)",
               x = NULL)

      # Else
      } else if (j %% 2 == 0 & j <= (length(survey_years_i) - 2)) {

        p_j <- p_j +
          labs(y = NULL,
               x = NULL)
      }

      p_j <- p_j +
        labs(fill = NULL,
             title = survey_years_i[j],
             subtitle = paste0("stock",
                               ": *",
                               round(stock_j$stock_final),
                               "* ", unit_density_per_cm_markdown, "<br>",
                               stock_j$label)) +
        coord_flip() +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_reverse(breaks = depth_range_j,
                           expand = c(0, 0)) +
        theme(text = element_text(color = "black",
                                  size = 10),
              plot.title = element_text(color = "black",
                                        size = 10,
                                        face = "bold",
                                        margin = margin(b = 6)),
              plot.subtitle = element_markdown(color = "black",
                                               size = 10,
                                               lineheight = 1.4,
                                               margin = margin(b = 10)),
              axis.text = element_text(size = 10, colour = "black"),
              axis.text.y = element_text(hjust = 1,
                                         vjust = 0.5,
                                         margin = margin(0,
                                                         5,
                                                         0,
                                                         0)),
              axis.text.x = element_text(margin = margin(8,
                                                         0,
                                                         0,
                                                         0)),
              axis.title.x = element_markdown(hjust = 1.01,
                                              lineheight = 1.4,
                                              colour = "black",
                                              margin = margin(t = 8,
                                                              r = 5)),
              axis.title.y = element_markdown(angle = 0,
                                              hjust = 0,
                                              vjust = 1.02,
                                              lineheight = 1.4,
                                              colour = "black",
                                              margin = margin(r = 5)),
              legend.text = element_text(lineheight = 1.2),
              axis.ticks.length = unit(-0.2, "cm"),
              axis.ticks.x = element_line(linewidth = 0.7, colour = "black"),
              axis.ticks.y = element_line(linewidth = 0.7, colour = "black"),
              axis.line.y = element_line(linewidth = 0.7, colour = "black"),
              axis.line.x = element_line(linewidth = 0.7, colour = "black"),
              panel.grid.major.x = element_line(linewidth = 1,
                                                colour = "#E6EBEB"),
              panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              plot.margin = margin(t = 0.5,  # Top margin
                                   r = 0.5,  # Right margin
                                   b = 0.5,  # Bottom margin
                                   l = 0.5,  # Left margin
                                   unit = "cm"),
              aspect.ratio = 1)

      mylist[[j]] <- p_j


    } # End of "for" loop along survey_years


    # Legend

    # Define the legend manually
    legend_info <- list(
      "Fixed-depth layers" = "#CC3300",
      "Pedogenic horizons" = "#570000",
      "Other survey years" = "grey"
    )

    # Draw the legend
    legend_grob <- grid::legendGrob(
      labels = names(legend_info),
      pch = 20,
      gp = gpar(col = unlist(unname(legend_info)), cex = 0.8),
      ncol = 1,
    )

    # Combine the plot and legend using patchwork
    mylist[[j + 1]] <- legend_grob


    p <- patchwork::wrap_plots(mylist,
                               ncol = 2,
                               guides = "collect",
                               axis_titles = "collect") +
      plot_annotation(
        title = paste0("**", plot_id_i, "**<br>",
                       "WRB reference soil group: *",
                       df_strat_i$wrb_ref_soil_group,
                       "*<br>",
                       gsub("\n", "*<br>*",
                       str_wrap(paste0("European forest type category: *",
                       df_strat_i$eftc, "*"))), "<br>",
                       "Humus form: *", df_strat_i$humus_form, "*")) &
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            legend.justification = c("left", "top"),
            title = element_markdown(lineheight = 1.4,
                                     size = 10),
            plot.margin = margin(t = 0.5,  # Top margin
                                 r = 0.5,  # Right margin
                                 b = 0.5,  # Bottom margin
                                 l = 0.5,  # Left margin
                                 unit = "cm"))


    height_i <- case_when(
      length(survey_years_i) == 1 ~ 5.5,
      length(survey_years_i) %in% c(2, 3) ~ 9,
      length(survey_years_i) %in% c(4, 5) ~ 13,
      length(survey_years_i) > 5 ~ 15)


    ggsave(filename = paste0(plot_id_i, ".png"),
           path = paste0(dir, code_survey, "_splines_per_plot/"),
           plot = p,
           dpi = 300,
           height = height_i,
           width = 6.81)

    # Update progress bar
    setTxtProgressBar(progress_bar, i)


  } # End of "for" loop along plot_ids

  close(progress_bar)
}


}
