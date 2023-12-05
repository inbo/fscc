
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
                        graph = FALSE,
                        density_per_three_cm = FALSE,
                        save_to_env = TRUE,
                        save_to_gdrive = TRUE) {

  # 1. Preparations ----

  ## 1.1. Load packages and functions ----

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"),
            require("mpspline2"))

  source("./src/stock_calculations/functions/soilspline.R")
  source("./src/stock_calculations/functions/spline2stock.R")

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")
  source("./src/functions/save_to_google_drive.R")

  ## 1.2. Retrieve data ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }

  code_survey <- unlist(str_split(survey_form, "_"))[1]


  ## 1.3. Assert that the input parameters are correct ----

  assertthat::assert_that(survey_form %in% c("s1_som",
                                             "s1_pfh",
                                             "so_som",
                                             "so_pfh"))

  # Assert that the required columns exist

  # "pfh"

  if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

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

  if (unlist(strsplit(survey_form, "_"))[2] == "som") {

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
    unit_density_per_cm = c(
      "kg ha-1 cm-1", "kg ha-1 cm-1", NA, NA, "1E4 mol+ ha-1 cm-1",
      "t ha-1 cm-1", "kg ha-1 cm-1", "1E4 mol+ ha-1 cm-1", "1E4 mol+ ha-1 cm-1", "kg ha-1 cm-1",
      "1E4 mol+ ha-1 cm-1", NA, "t ha-1 cm-1", "kg ha-1 cm-1", "1E4 mol+ ha-1 cm-1",
      "kg ha-1 cm-1", "1E4 mol+ ha-1 cm-1", "kg ha-1 cm-1", "t ha-1 cm-1", "1E4 mol+ ha-1 cm-1",
      "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1", NA, NA,
      "kg ha-1 cm-1", "1E4 mol+ ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1",
      "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1",
      "kg ha-1 cm-1", "kg ha-1 cm-1", NA, "1E4 mol+ ha-1 cm-1", "kg ha-1 cm-1",
      "kg ha-1 cm-1", "kg ha-1 cm-1", "kg ha-1 cm-1",
      NA, NA, "kg ha-1 cm-1", "kg ha-1 cm-1",
      NA),
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
    ))

  ind_som <- which(parameter == parameter_table$som_parameter)
  ind_pfh <- which(parameter == parameter_table$pfh_parameter)
  ind <- unique(c(ind_som, ind_pfh))

  assertthat::assert_that(!identical(ind, integer(0)))

  unit <- parameter_table$unit[ind]
  unit_density_per_cm <- parameter_table$unit_density_per_cm[ind]
  shorter_var_name <- parameter_table$shorter_name[ind]

  assertthat::assert_that(!is.na(shorter_var_name))

  assertthat::assert_that(
    parameter_table$som_parameter[ind] %in% names(df) ||
      parameter_table$pfh_parameter[ind] %in% names(df))

  parameter_orig <- parameter
  parameter <- parameter_table$som_parameter[ind]

  if (!parameter_table$som_parameter[ind] %in% names(df)) {
    names(df)[which(names(df) == parameter_table$pfh_parameter[ind])] <-
      parameter_table$som_parameter[ind]
  }

  names(df)[which(names(df) == parameter)] <-
    "parameter_for_stock"

  assertthat::assert_that(all(na.omit(df[[parameter]]) >= 0))


  ## 1.4. Add eff_soil_depth if needed ----

  if (!"eff_soil_depth" %in% names(df)) {

    prf <- get_env(paste0(code_survey, "_prf"))

    # Aggregate per plot_id

    prf_agg <- prf %>%
      # Aggregate per plot_id
      group_by(plot_id) %>%
      summarise(eff_soil_depth = mean(eff_soil_depth, na.rm = TRUE))

    df <- df %>%
      # Add soil depth data
      left_join(prf_agg,
                by = "plot_id")
  }

  ## 1.5. Final data preparations ----

  df_working <- df %>%
    ungroup() %>%
    # If the effective soil depth is deeper than 100 cm or unknown (NA):
    # assume it is 100 cm (for stocks)
    mutate(soil_depth =
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

  ## 1.6. Create function format_stocks ----

  format_stocks <- function(data_frame,
                            var_name = shorter_var_name) {

    df_output <- data_frame %>%
      # Remove any columns starting with the word "density"
      # (these columns, with densities after fitting mass-preserving splines,
      # are only for internal graphing purposes)
      select(-starts_with("density_")) %>%
      # Remove records with negative stocks
      filter(if_any(matches("stock_10"),  ~.x >= 0)) %>%
      filter(if_any(matches("stock_forest_floor"),  ~.x >= 0)) %>%
      # Add the variable name (e.g. "c_" for TOC) to the stock names
      rename_with(~ ifelse(startsWith(., "stock"),
                           paste(var_name, ., sep = "_"), .),
                  everything()) %>%
      rename_with(~ ifelse(startsWith(., "density"),
                           paste(var_name, ., sep = "_"), .),
                  everything()) %>%
      # Rename the column with name "parameter_for_stock" to the
      # actual parameter name (e.g. "organic_carbon_total")
      # rename_with(~ ifelse(grepl("parameter_for_stock", .),
      #                      parameter,
      #                      .),
      #             starts_with("parameter_for_stock")) %>%
      rename_with(
        ~ case_when(
          . == "parameter_for_stock" ~ parameter,
          TRUE ~ .))


    return(df_output)
  }


  # 2. Below-ground layers ----

  ## 2.1. Derive layer-based dataset for mineral and peat layers ----

  df_below_ground <- df_working %>%
    filter(.data$layer_type %in% c("mineral", "peat")) %>%
    # Filter out redundant layers
    # (i.e. layers not needed to compose the complete profile)
    filter(!is.na(.data$layer_number)) %>%
    select(partner_short,
           plot_id,
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
           soil_depth,
           bulk_density,
           organic_layer_weight,
           coarse_fragment_vol,
           coarse_fragment_vol_frac,
           parameter_for_stock) %>%
    # Calculate carbon density per cm (t C ha-1 cm-1)
    # Units: (g C/kg fine earth) * (kg fine earth/m3 soil) = g C/m3 soil
    # 1 ha * 1 cm = 100 m * 100 m * 0.01 m = 100 m3
    # 1 ton C = 1E6 g C
    # Unit conversion factor: * 100 / 1000000
    mutate(density =
             (.data$parameter_for_stock * .data$bulk_density *
                (1 - .data$coarse_fragment_vol_frac)) / 10000) %>%
    # Carbon stock per layer (instead of per cm)
    # Units: t C ha-1 (per layer)
    mutate(stock_layer =
             .data$density * .data$layer_thickness) %>%
    # In peat layers, organic layer weight may have been reported
    # instead of bulk density etc
    # In that case, use the formula for forest floor
    mutate(stock_layer =
             ifelse(is.na(.data$stock_layer) &
                      !is.na(.data$organic_layer_weight),
                    (.data$parameter_for_stock * .data$organic_layer_weight) /
                      100,
                    .data$stock_layer)) %>%
    mutate(density =
             ifelse(is.na(.data$density) &
                      !is.na(.data$stock_layer),
                    .data$stock_layer / .data$layer_thickness,
                    .data$density)) %>%
    # Add data availability index
    mutate(avail_thick = ifelse(is.na(.data$layer_thickness), 0, 1),
           avail_toc = ifelse(is.na(.data$parameter_for_stock), 0, 1),
           avail_bd = ifelse(is.na(.data$bulk_density), 0, 1),
           avail_cf = ifelse(is.na(.data$coarse_fragment_vol), 0, 1)) %>%
    select(-coarse_fragment_vol)


  # Save the df_below_ground dataset (all data, also missing data)

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form, "_below_ground"),
               df_below_ground)
  }

  if (save_to_gdrive == TRUE) {

    save_to_google_drive(objects_to_save =
                           paste0(survey_form, "_below_ground"),
                         df_object_to_save = format_stocks(df_below_ground),
                         path_name = "./output/stocks/")
  }




  ## 2.2. Gap-fill with assumption: constant subsoil ----

  # If it is okay to assume that the subsoil carbon density remains the same
  # gap-fill subsoil carbon density data

  if (constant_subsoil == TRUE) {

    cat(paste0(" \nGap-fill subsoil\n"))

    source("./src/functions/harmonise_layer_to_depths.R")

    extra_rows <- NULL

    # Assume the following depth range that can be considered constant:
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

          if (depth_range_missing[length(depth_range_missing)] == 100 &&
              depth_range_missing[length(depth_range_missing) - 1] < 99) {
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
               density)

      limit_sup <- max(min(df_sub_selected$layer_limit_superior),
                       min(depth_range_missing))

      limit_inf <- min(max(df_sub_selected$layer_limit_inferior),
                       max(depth_range_missing))

      density_i <-
        harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = "density")

      bulk_density_i <-
        harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = "bulk_density")

      df_sub_selected <-
      extra_row_i <- df_profile_i %>%
        filter(layer_number == max(df_profile_i$layer_number)) %>%
        mutate(code_layer = "X",
               layer_type = "mineral",
               layer_number = 100,
               depth_top = limit_sup,
               depth_bottom = limit_inf,
               depth_avg = mean(c(limit_sup, limit_inf)),
               layer_thickness = diff(c(limit_sup, limit_inf)),
               bulk_density = bulk_density_i,
               density = density_i) %>%
        mutate(across(c("organic_layer_weight", "coarse_fragment_vol_frac",
                      "parameter_for_stock", "stock_layer",
                      "avail_thick", "avail_toc", "avail_bd", "avail_cf"),
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
      bind_rows(df_below_ground,
                extra_rows) %>%
      arrange(partner_short,
              code_plot,
              survey_year,
              repetition,
              layer_number)

    cat(paste0(" \nSubsoils gap-filled for ",
               length(unique(extra_rows$profile_id)),
               " profiles in '",
               survey_form, "'.\n",
               "--------------------------------------------------------\n"))
  }






  ## 2.3. Calculate below-ground carbon stocks ----

  profile_stocks_below_ground <- NULL
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

    assertthat::assert_that(length(unique(df_profile_i$soil_depth)) == 1)
    soil_depth_i <- unique(df_profile_i$soil_depth)

    # Prepare input for calculate_stocks function

    prof <- df_profile_i %>%
      select(plot_id,
             profile_id,
             code_layer,
             depth_top,
             depth_bottom,
             density,
             soil_depth) %>%
      # Only calculate carbon stocks based on layers
      # for which the carbon density is known
      filter(!is.na(.data$density)) %>%
      # and for which their layer limits are known
      filter(!is.na(.data$depth_top) &
               !is.na(.data$depth_bottom))

    # Assert that all the depth layers are below-ground

    # if (!prof_id_i %in% c("1995_7_139_1",
    #                       "1995_7_208_1",
    #                       "2008_13_164_1",
    #                       "2008_13_1161_1")) {

      assertthat::assert_that(
        all(prof$depth_top >= 0) &&
          all(prof$depth_bottom >= 0),
        msg = paste0("Not all layer limits are below-ground ",
                     "(i.e. not below 0) for the profile '",
                     prof_id_i, "'."))
    #}

    # Issue:
    # Not possible to calculate splines in case of overlapping layers in
    # the profile
    # Solution: take average of the adjacent overlapping depth limits

    # prof <- prof %>%
    #   mutate(prev_depth_bottom = lag(depth_bottom),
    #          next_depth_top = lead(depth_top)) %>%
    #   rowwise() %>%
    #   mutate(depth_bottom = ifelse(!is.na(.data$depth_bottom) &
    #                                  !is.na(.data$next_depth_top) &
    #                                  (.data$next_depth_top < .data$depth_bottom),
    #                                mean(c(.data$depth_bottom,
    #                                       .data$next_depth_top)),
    #                                .data$depth_bottom),
    #          depth_top = ifelse(!is.na(.data$depth_top) &
    #                               !is.na(.data$prev_depth_bottom) &
    #                               (.data$prev_depth_bottom > .data$depth_top),
    #                             mean(c(.data$depth_top,
    #                                    .data$prev_depth_bottom)),
    #                             .data$depth_top)) %>%
    #   select(-prev_depth_bottom,
    #          -next_depth_top)


    # Apply calculate_stocks and soilspline functions on profile

    # Only fit splines to profiles with:
    # - known carbon densities and layer limits for at least two layers
    # - and one of them should be the upper below-ground layer

    # If there are less than two layers or
    # if the highest layer is not the upper below-ground layer,
    # any stock calculations beyond the boundaries of the known layers
    # are simply too unreliable

    if ((nrow(prof) >= 2) &&
        (min(prof$depth_top) == 0)) { # &&
        # The issue with this profile needs to be solved
        # (!prof_id_i %in% c("1995_7_139_1",
        #                    "1995_7_208_1",
        #                    "2008_13_164_1",
        #                    "2008_13_1161_1"))) {

      profile_stock_output_i <-
        spline2stock(prof = prof,
                     variab_name = "density",
                     survey_form = survey_form,
                     density_per_three_cm = density_per_three_cm,
                     graph = graph)

      profile_stocks_i <-
        data.frame(partner_short = unique(df_profile_i$partner_short),
                   plot_id = plot_id_i,
                   profile_id = prof_id_i,
                   survey_year = unique(df_profile_i$survey_year),
                   partner_code = unique(df_profile_i$partner_code),
                   code_country = unique(df_profile_i$code_country),
                   code_plot = unique(df_profile_i$code_plot),
                   repetition = unique(df_profile_i$repetition),
                   contains_peat = any(df_profile_i$layer_type == "peat"),
                   obs_depth = max(prof$depth_bottom),
                   soil_depth = soil_depth_i,
                   profile_stock_output_i)

      profile_stocks_below_ground <-
        rbind(profile_stocks_below_ground,
              profile_stocks_i)

    }

    # Update progress bar
    setTxtProgressBar(progress_bar, i)

  } # End of for loop along profiles

  close(progress_bar)

  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form,
                      "_profile_",
                      shorter_var_name,
                      "_stocks_below_ground"),
               profile_stocks_below_ground)
  }

  if (save_to_gdrive == TRUE) {

    save_to_google_drive(objects_to_save =
                           paste0(survey_form,
                                  "_profile_",
                                  shorter_var_name,
                                  "_stocks_below_ground"),
                         df_object_to_save =
                           format_stocks(profile_stocks_below_ground),
                         path_name = "./output/stocks/")
  }



  ## 2.4. Aggregate below-ground per plot ----

  plot_stocks_below_ground <- profile_stocks_below_ground %>%
    filter(stock_10 > 0) %>%
    group_by(partner_short, partner_code, code_country, code_plot,
             plot_id, survey_year) %>%
    summarise(stock_below_ground_avg =
                round(mean(stock_below_ground, na.rm = TRUE), 2),
              # Standard deviation across spatial repetitions
              # (this also accounts for some sample preprocessing and
              # lab analytical uncertainty)
              stock_below_ground_stdev =
                ifelse(length(stock_below_ground) > 1,
                       round(sd(stock_below_ground, na.rm = TRUE), 2),
                       NA),
              stock_below_ground_topsoil =
                round(mean(stock_below_ground_topsoil, na.rm = TRUE), 2),
              stock_below_ground_stdev =
                ifelse(length(stock_below_ground_topsoil) > 1,
                       round(sd(stock_below_ground_topsoil, na.rm = TRUE), 2),
                       NA),
              nlay_below_ground_min =
                min(nlay_below_ground, na.rm = TRUE),
              nlay_below_ground_max =
                max(nlay_below_ground, na.rm = TRUE),
              obs_depth_avg =
                round(mean(obs_depth, na.rm = TRUE), 2),
              soil_depth_avg =
                round(mean(soil_depth, na.rm = TRUE), 2),
              contains_peat =
                any(contains_peat == TRUE),
              rmse_mpspline_max =
                max(rmse_mpspline, na.rm = TRUE),
              .groups = "drop") %>%
    rename(stock_below_ground = stock_below_ground_avg) %>%
    arrange(partner_short,
            code_plot,
            survey_year) %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))


  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form,
                      "_plot_",
                      shorter_var_name,
                      "_stocks_below_ground"),
               plot_stocks_below_ground)
  }

  if (save_to_gdrive == TRUE) {

    save_to_google_drive(objects_to_save =
                           paste0(survey_form,
                                  "_plot_",
                                  shorter_var_name,
                                  "_stocks_below_ground"),
                         df_object_to_save =
                           format_stocks(plot_stocks_below_ground),
                         path_name = "./output/stocks/")
  }





  cat(" \n--------------------------------------------------------\n")





  # 3. Forest floor layers ----
  ## 3.1. Derive layer-based dataset for forest floors ----

  cat(paste0(" \nCalculate forest floor stocks by depth-integrating ",
             "layer stocks.\n"))

  df_forest_floor <- df_working %>%
    filter(layer_type == "forest_floor") %>%
    # Filter out redundant layers
    # (i.e. layers not needed to compose the complete profile,
    # such as overlapping layers)
    filter(!is.na(.data$layer_number)) %>%
    # One Slovakian profile has a layer named "OL1"
    # (renamed as such by FSCC because it contains two "OL" layers
    # from which one redundant)
    # Rename this
    mutate(code_layer = ifelse(.data$code_layer == "OL1",
                               "OL",
                               .data$code_layer)) %>%
    select(partner_short,
           plot_id,
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
           soil_depth,
           bulk_density,
           organic_layer_weight,
           coarse_fragment_vol,
           coarse_fragment_vol_frac,
           parameter_for_stock) %>%
    # Carbon stock per layer (t C ha-1 for each layer)
    # Units: g C/kg forest floor * kg forest floor/m2
    mutate(stock_layer =
             (.data$parameter_for_stock * .data$organic_layer_weight) /
             100) %>%
    # Carbon density (t C ha-1 cm-1)
    mutate(density =
             .data$stock_layer / .data$layer_thickness) %>%
    # Add data availability index
    mutate(avail_thick = ifelse(is.na(.data$layer_thickness), 0, 1),
           avail_toc = ifelse(is.na(.data$parameter_for_stock), 0, 1),
           avail_bd = ifelse(is.na(.data$bulk_density), 0, 1),
           avail_org_layer_weight =
             ifelse(is.na(.data$organic_layer_weight), 0, 1)) %>%
    select(-coarse_fragment_vol)

  # Save the df_forest_floor dataset (all data, also missing data)

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form, "_forest_floor"),
               df_forest_floor)
  }

  if (save_to_gdrive == TRUE) {
    save_to_google_drive(objects_to_save =
                           paste0(survey_form, "_forest_floor"),
                         df_object_to_save = format_stocks(df_forest_floor),
                         path_name = "./output/stocks/")
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
      filter(profile_id == profile_list[i]) %>%
      filter(!is.na(stock_layer))

    if (nrow(df_profile_i) > 0) {

      # Combine stocks in OL, OFH and O per profile

      profile_stocks_i <-
        data.frame(partner_short = unique(df_profile_i$partner_short),
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
                   stock_ol =
                     ifelse("OL" %in% df_profile_i$code_layer,
                            df_profile_i %>%
                              filter(code_layer == "OL") %>%
                              pull(stock_layer) %>%
                              round(2),
                            NA),
                   stock_ofh =
                     ifelse(("OF" %in% df_profile_i$code_layer &&
                               "OH" %in% df_profile_i$code_layer) ||
                              ("OFH" %in% df_profile_i$code_layer),
                            df_profile_i %>%
                              filter(code_layer %in% c("OF", "OH", "OFH")) %>%
                              pull(stock_layer) %>%
                              sum %>%
                              round(2),
                            NA),
                   # Cumulative carbon stock in forest floor
                   # Note that this is not necessarily the sum of
                   # - carbon_stock_ol
                   # - carbon_stock_ofh
                   # Since layer-specific carbon stocks in the forest floor can
                   # sometimes not be assigned to any of both stocks
                   # (e.g. "OLF", only "OF", only "OH", "O1", ...)
                   stock_forest_floor =
                     round(sum(df_profile_i$stock_layer), 2))

      profile_stocks_forest_floor <-
        rbind(profile_stocks_forest_floor,
              profile_stocks_i)

    }

    # Update progress bar
    setTxtProgressBar(progress_bar, i)
  }

  close(progress_bar)

  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form,
                      "_profile_",
                      shorter_var_name,
                      "_stocks_forest_floor"),
               profile_stocks_forest_floor)
  }

  if (save_to_gdrive == TRUE) {
    save_to_google_drive(objects_to_save =
                           paste0(survey_form,
                                  "_profile_",
                                  shorter_var_name,
                                  "_stocks_forest_floor"),
                         df_object_to_save =
                           format_stocks(profile_stocks_forest_floor),
                         path_name = "./output/stocks/")
  }




  ## 3.3. Aggregate forest floor per plot ----

  plot_stocks_forest_floor <-
    profile_stocks_forest_floor %>%
    filter(stock_forest_floor >= 0) %>%
    group_by(partner_short, partner_code, code_country, code_plot,
             plot_id, survey_year) %>%
    summarise(stock_forest_floor_avg =
                round(mean(stock_forest_floor, na.rm = TRUE), 2),
              # Standard deviation across spatial repetitions
              # (this also accounts for some sample preprocessing and
              # lab analytical uncertainty)
              stock_forest_floor_stdev =
                ifelse(length(stock_forest_floor) > 1,
                       round(sd(stock_forest_floor, na.rm = TRUE), 2),
                       NA),
              nlay_forest_floor_min =
                min(nlay_forest_floor, na.rm = TRUE),
              nlay_forest_floor_max =
                max(nlay_forest_floor, na.rm = TRUE),
              forest_floor_thickness_avg =
                round(mean(forest_floor_thickness, na.rm = TRUE), 2),
              forest_floor_layers_unique =
                ifelse(length(unique(forest_floor_layers)) == 1,
                       unique(forest_floor_layers),
                       NA),
              .groups = "drop") %>%
    rename(stock_forest_floor = stock_forest_floor_avg) %>%
    arrange(partner_short,
            code_plot,
            survey_year) %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))

  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form,
                      "_plot_",
                      shorter_var_name,
                      "_stocks_forest_floor"),
               plot_stocks_forest_floor)
  }

  if (save_to_gdrive == TRUE) {

    save_to_google_drive(objects_to_save =
                           paste0(survey_form,
                                  "_plot_",
                                  shorter_var_name,
                                  "_stocks_forest_floor"),
                         df_object_to_save =
                           format_stocks(plot_stocks_forest_floor),
                         path_name = "./output/stocks/")
  }

  cat(" \n--------------------------------------------------------\n")






  # 4. Combine below-ground and forest floor ----
  ## 4.1. Join below-ground and forest floor ----

  cat(paste0(" \nCombine below-ground and forest floor stocks.\n"))

  profile_stocks <-
    profile_stocks_below_ground %>%
    left_join(profile_stocks_forest_floor %>%
                select(-partner_short,
                       -plot_id,
                       -survey_year,
                       -partner_code,
                       -code_country,
                       -code_plot,
                       -repetition),
              by = "profile_id") %>%
    mutate(stock =
             rowSums(select(., stock_below_ground, stock_forest_floor),
                     na.rm = TRUE),
           stock_topsoil =
             rowSums(select(., stock_below_ground_topsoil, stock_forest_floor),
                     na.rm = TRUE),
           nlay = rowSums(select(., nlay_below_ground, nlay_forest_floor),
                          na.rm = TRUE))

  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form,
                      "_profile_",
                      shorter_var_name,
                      "_stocks"),
               profile_stocks)
  }

  if (save_to_gdrive == TRUE) {

    save_to_google_drive(objects_to_save =
                           paste0(survey_form,
                                  "_profile_",
                                  shorter_var_name,
                                  "_stocks"),
                         df_object_to_save =
                           format_stocks(profile_stocks),
                         path_name = "./output/stocks/")
  }






  ## 4.2. Aggregate per plot ----

  plot_stocks <- profile_stocks %>%
    filter(stock_10 > 0) %>%
    group_by(partner_short, partner_code, code_country, code_plot,
             plot_id, survey_year) %>%
    summarise(stock_avg =
                round(mean(stock, na.rm = TRUE), 2),
              stock_stdev =
                ifelse(length(stock) > 1,
                       round(sd(stock, na.rm = TRUE), 2),
                       NA),
              stock_topsoil_avg =
                round(mean(stock_topsoil, na.rm = TRUE), 2),
              stock_topsoil_stdev =
                ifelse(length(stock_topsoil) > 1,
                       round(sd(stock_topsoil, na.rm = TRUE), 2),
                       NA),
              stock_below_ground_avg =
                round(mean(stock_below_ground, na.rm = TRUE), 2),
              stock_below_ground_stdev =
                ifelse(length(stock_below_ground) > 1,
                       round(sd(stock_below_ground, na.rm = TRUE), 2),
                       NA),
              stock_below_ground_topsoil_avg =
                round(mean(stock_below_ground_topsoil, na.rm = TRUE), 2),
              stock_below_ground_topsoil_stdev =
                ifelse(length(stock_below_ground_topsoil) > 1,
                       round(sd(stock_below_ground_topsoil, na.rm = TRUE), 2),
                       NA),
              stock_forest_floor_avg =
                round(mean(stock_forest_floor, na.rm = TRUE), 2),
              stock_forest_floor_stdev =
                ifelse(length(stock_forest_floor) > 1,
                       round(sd(stock_forest_floor, na.rm = TRUE), 2),
                       NA),
              nlay_min = min(nlay,
                             na.rm = TRUE),
              nlay_max = max(nlay,
                             na.rm = TRUE),
              nlay_below_ground_min = min(nlay_below_ground,
                                          na.rm = TRUE),
              nlay_below_ground_max = max(nlay_below_ground,
                                          na.rm = TRUE),
              obs_depth_avg =
                round(mean(obs_depth, na.rm = TRUE), 2),
              soil_depth_avg =
                round(mean(soil_depth, na.rm = TRUE), 2),
              forest_floor_thickness_avg =
                round(mean(forest_floor_thickness, na.rm = TRUE), 2),
              forest_floor_layers_unique =
                ifelse(length(unique(forest_floor_layers)) == 1,
                       unique(forest_floor_layers),
                       NA),
              contains_peat =
                any(contains_peat == TRUE),
              rmse_mpspline_max =
                max(rmse_mpspline, na.rm = TRUE),
              .groups = "drop") %>%
    rename(stock = stock_avg) %>%
    rename(stock_topsoil = stock_topsoil_avg) %>%
    rename(stock_forest_floor = stock_forest_floor_avg) %>%
    rename(stock_below_ground = stock_below_ground_avg) %>%
    rename(stock_below_ground_topsoil = stock_below_ground_topsoil_avg) %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%
    arrange(partner_short,
            code_plot,
            survey_year)

  # Save output

  if (save_to_env == TRUE) {
    assign_env(paste0(survey_form,
                      "_plot_",
                      shorter_var_name,
                      "_stocks"),
               plot_stocks)
  }

  if (save_to_gdrive == TRUE) {

    save_to_google_drive(objects_to_save =
                           paste0(survey_form,
                                  "_plot_",
                                  shorter_var_name,
                                  "_stocks"),
                         df_object_to_save =
                           format_stocks(plot_stocks),
                         path_name = "./output/stocks/")
  }
  cat(" \n--------------------------------------------------------\n")

  # Number of plots with stocks

  cat(paste0(" \nNumber of plots in '",
             survey_form,
             "' with below-ground '",
             parameter,
             "' stocks:\n"))

  print(plot_stocks %>%
          distinct(plot_id) %>%
          nrow)

  # Number of plots with stocks for at least two surveys

  cat(paste0(" \n",
             "Number of plots in '",
             survey_form,
             "' with below-ground '",
             parameter,
             "' stocks for at least two survey years:\n"))

  print(plot_stocks %>%
          group_by(plot_id) %>%
          summarise(n = n()) %>%
          filter(n > 1) %>%
          nrow)

}
