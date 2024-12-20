

gapfill_internally <- function(survey_form,
                               data_frame = NULL,
                               save_to_env = FALSE,
                               gapfill_ff_concentrations = TRUE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  source("./src/functions/harmonise_per_plot_layer.R")
  source("./src/functions/depth_join.R")

  cat(paste0(" \nGap-fill '", survey_form, "' internally\n"))


  code_survey <- unlist(str_split(survey_form, "_"))[1]
  survey_form_type <- unlist(str_split(survey_form, "_"))[2]



  # Parameter ranges ----

  source("./src/functions/bulk_density_ptf.R")




  # Import survey forms ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }


  if (survey_form_type == "pfh") {

    # Harmonise the variable names with those of "som"

    df <- df %>%
      rename(layer_limit_superior = horizon_limit_up) %>%
      rename(layer_limit_inferior = horizon_limit_low) %>%
      rename(code_layer = horizon_master) %>%
      rename(organic_carbon_total = horizon_c_organic_total) %>%
      rename(organic_carbon_total_source = horizon_c_organic_total_source) %>%
      rename(n_total = horizon_n_total) %>%
      rename(n_total_source = horizon_n_total_source) %>%
      rename(unique_survey_repetition = unique_survey_profile)

  }





  # 1. Get data of physical parameters at one profile per plot ----

  ## 1.1. Source 1: "sw_swc" ----

  if (code_survey == "so") {

    # Level II

    swc <- get_env("sw_swc")

    # Above-ground as well as below-ground need to be gap-filled
    # based on depth

    name <- "sw_swc_plot_layer"

    if (!exists(name) ||
        (exists(name) &&
         any(!"data.frame" %in% class(get_env(name))))) {

      cat(paste0(" \nHarmonise data of '", "sw_swc", "' per plot layer\n"))

      swc_plot <-
        harmonise_per_plot_layer(survey_form_input = "sw_swc",
                                 data_frame_input = swc)

      assign_env(name,
                 swc_plot)

      write.table(swc_plot,
                  file = paste0("./output/gap_filling_details/",
                                name,
                                ".csv"),
                  row.names = FALSE,
                  na = "",
                  sep = ";",
                  dec = ".")

    } else {

      swc_plot <- get_env(name)

    }
  }




  ## 1.2. Source 2: "pfh" ----

  # This takes some time, so check whether this form already exists
  # in the global environment

  name <- paste0(code_survey, "_pfh_plot_layer")

  if (!exists(name) ||
       (exists(name) &&
        any(!"data.frame" %in% class(get_env(name))))) {

    # Redundant layers do need to be removed so this column is needed

    assertthat::assert_that(
      "layer_number" %in% names(
        get_env(paste0(code_survey, "_pfh"))))

    cat(paste0(" \nHarmonise data of '", code_survey, "_pfh",
               "' per plot layer\n"))

    pfh_plot <-
      harmonise_per_plot_layer(
        survey_form_input =
          paste0(code_survey, "_pfh"),
        data_frame_input =
          get_env(paste0(code_survey, "_pfh")))

    assign_env(name,
               pfh_plot)

    write.table(pfh_plot,
                file = paste0("./output/gap_filling_details/",
                              name,
                              ".csv"),
                row.names = FALSE,
                na = "",
                sep = ";",
                dec = ".")

  } else {

    pfh_plot <- get_env(name)

  }




  ## 1.3. Source 3: "som" ----

  # This takes some time, so check whether this form already exists
  # in the global environment

  name <- paste0(code_survey, "_som_plot_layer")

  if (!exists(name) ||
      (exists(name) &&
       any(!"data.frame" %in% class(get_env(name))))) {

    # Redundant layers do need to be removed so this column is needed

    assertthat::assert_that(
      "layer_number" %in% names(
        get_env(paste0(code_survey, "_som"))))

    cat(paste0(" \nHarmonise data of '", code_survey, "_som",
               "' per plot layer\n"))

    som_plot <-
      harmonise_per_plot_layer(
        survey_form_input =
          paste0(code_survey, "_som"),
        data_frame_input =
          get_env(paste0(code_survey, "_som")))

    assign_env(name,
               som_plot)

    write.table(som_plot,
                file = paste0("./output/gap_filling_details/",
                              name,
                              ".csv"),
                row.names = FALSE,
                na = "",
                sep = ";",
                dec = ".")

  } else {

    som_plot <- get_env(name)

  }







# 2. Add data to df ----

  # Remark: with this approach, based on joining one harmonised profile per
  # plot_id (non-redundant layers), physical variables that are gap-filled
  # in this script are removed if a certain layer is redundant (e.g. R-horizon)

  source("./src/functions/depth_join.R")

  # Add data of "som"

  cat(paste0(" \nAdd 'som' data to '", survey_form, "'.\n"))

  df <- depth_join(df1 = df,
                   df2 = som_plot,
                   parameters = NULL,
                   prefix_parameters_in_df1 = "som_")

  # Add data of "pfh"

  cat(paste0(" \nAdd 'pfh' data to '", survey_form, "'.\n"))

  df <- depth_join(df1 = df,
                   df2 = pfh_plot,
                   parameters = NULL,
                   prefix_parameters_in_df1 = "pfh_")

  # Add data of "swc"

  if (code_survey == "so") {

  cat(paste0(" \nAdd 'swc' data to '", survey_form, "'.\n"))

  df <- depth_join(df1 = df,
                   df2 = swc_plot,
                   prefix_parameters_in_df1 = "swc_")
  }



  # Save

  write.table(df,
              file = paste0("./output/gap_filling_details/",
                            survey_form, "_suppl",
                            ".csv"),
              row.names = FALSE,
              na = "",
              sep = ";",
              dec = ".")







# 3. Combine columns ----

  ## 3.1. "som" ----

  if (survey_form_type == "som") {

    ### 3.1.1. Bulk density ----

    cat("\n\nCombine bulk density columns\n")

    bulk_density_columns <-
      df %>%
        select(contains("bulk_dens"),
               -contains("source"), -contains("survey_year"),
               -contains("orig"), -contains("fscdb")) %>%
        names

    quant_bd <- c(6, 1991) # Based on minimum and maximum in
    # bd_upper and bd_lower functions

    df <- df %>%
      # Make a copy of the current bulk density column,
      # since the bulk density column will be changed,
      # while the current values are relevant for the uncertainty range
      mutate(bulk_density_pre_gapfill = bulk_density) %>%
      mutate(
        bd_up = case_when(
          layer_type == "mineral" ~ bd_upper(.data$organic_carbon_total),
          layer_type == "peat" ~ bd_peat_upper_95,
          layer_type == "forest_floor" & !grepl("L", code_layer) ~
            bd_ff_upper_95,
          TRUE ~ quant_bd[2]),
        bd_low = case_when(
          layer_type == "mineral" ~ bd_lower(.data$organic_carbon_total),
          layer_type == "peat" ~ bd_peat_lower_5,
          layer_type == "forest_floor" & !grepl("L", code_layer) ~
            bd_ff_lower_5,
          TRUE ~ quant_bd[1])) %>%
      # Remove values outside of the bulk density range
      # This needs to be repeated for bulk density (next to the
      # harmonise_per_plot_layer function). The latter is less strict,
      # especially for sw_swc, for which no TOC is reported.
      mutate_at((bulk_density_columns),
                ~ ifelse(
                  (!is.na(.)) &
                    (. > bd_low & . < bd_up),
                  .,
                  NA)) %>%
      mutate(bulk_density_source = ifelse(
          (!is.na(bulk_density_source) &
             !is.na(bulk_density)),
          bulk_density_source,
          NA)) %>%
      select(-bd_low, -bd_up) %>%
      # Use bulk_density_layer_weight to gap-fill layers from the same year
      # (only for organic layers)
      mutate(
        bulk_density_source = ifelse(
          (is.na(bulk_density) &
             !is.na(bulk_density_layer_weight) &
             (layer_type %in% c("forest_floor", "peat"))),
          paste0("som (organic_layer_weight ", .data$survey_year, ")"),
          bulk_density_source),
        bulk_density = ifelse(
          (is.na(bulk_density) &
             !is.na(bulk_density_layer_weight) &
             (layer_type %in% c("forest_floor", "peat"))),
          bulk_density_layer_weight,
          bulk_density))

    if (survey_form == "so_som") {

    df <- df %>%
      # Gap-fill
      mutate(
        bulk_density_source = ifelse(
          # If bulk density is reported and the layer is either forest floor,
          # or mineral/peat with in the latter case the survey year of the
          # harmonised bulk density data being the same as the survey
          # year of the given layer
          # (i.e. in mineral records:
          #  if the harmonised bulk density data for the given survey form
          #  are for example from 2007
          #  because the harmonise_per_plot_layer script detected this as
          #  the best year for bulk density information for the given plot_id,
          #  while the survey year for your record is different,
          #  then you need to replace the reported bulk density value by the
          #  harmonised value.
          #  This was decided because of the assumption that bulk densities
          #  remain the same for mineral and peat layers)
          !is.na(bulk_density) &
            ((layer_type %in% c("forest_floor", "peat")) |
               (layer_type %in% c("mineral") &
                  !is.na(som_bulk_density_survey_year) &
                  (survey_year == som_bulk_density_survey_year))),
          # Original data are kept
          bulk_density_source,
          # Sources for gap-filling
          case_when(
            !is.na(som_bulk_density) ~
              paste0("som_", .data$som_bulk_density_survey_year),
            !is.na(.$swc_bulk_density) ~
              paste0("swc_", swc_bulk_density_survey_year),
            !is.na(pfh_horizon_bulk_dens_measure) ~
              paste0("pfh_measure_",
                     .data$pfh_horizon_bulk_dens_measure_survey_year),
            !is.na(pfh_horizon_bulk_dens_est) ~
              paste0("pfh_est_", .data$pfh_horizon_bulk_dens_est_survey_year))),
        bulk_density = ifelse(
          !is.na(bulk_density) &
            ((layer_type %in% c("forest_floor", "peat")) |
            (layer_type %in% c("mineral") &
               !is.na(som_bulk_density_survey_year) &
               (survey_year == som_bulk_density_survey_year))),
          # Keep the original bulk density data
          bulk_density,
          # Else, gap-fill
          coalesce(
            som_bulk_density,
            swc_bulk_density,
            pfh_horizon_bulk_dens_measure,
            pfh_horizon_bulk_dens_est)))

    }

    if (survey_form == "s1_som") {

      df <- df %>%
        # Gap-fill
        mutate(
          bulk_density_source = ifelse(
            !is.na(bulk_density) &
              ((layer_type %in% c("forest_floor", "peat")) |
                 (layer_type %in% c("mineral") &
                    !is.na(som_bulk_density_survey_year) &
                    (survey_year == som_bulk_density_survey_year))),
            # Original data are kept
            bulk_density_source,
            # Sources for gap-filling
            case_when(
              !is.na(som_bulk_density) ~
                paste0("som_", .data$som_bulk_density_survey_year),
              !is.na(pfh_horizon_bulk_dens_measure) ~
                paste0("pfh_measure_",
                       .data$pfh_horizon_bulk_dens_measure_survey_year),
              !is.na(pfh_horizon_bulk_dens_est) ~
                paste0("pfh_est_",
                       .data$pfh_horizon_bulk_dens_est_survey_year))),
          bulk_density = ifelse(
            !is.na(bulk_density) &
              ((layer_type %in% c("forest_floor", "peat")) |
                 (layer_type %in% c("mineral") &
                    !is.na(som_bulk_density_survey_year) &
                    (survey_year == som_bulk_density_survey_year))),
            # Keep the original bulk density data
            bulk_density,
            # Else, gap-fill
            coalesce(
              som_bulk_density,
              pfh_horizon_bulk_dens_measure,
              pfh_horizon_bulk_dens_est)))

    }

    df <- df %>%
      # Min-max intervals
      # Rather than confidence intervals or standard deviations,
      # we will just report the minimum and maximum of all of the possible
      # values for the given layer, because:
      # - we select the value for which we think it is most likely
      #   (based on survey year, survey form, range, etc), so we do not take
      #   an average value from all the possible values
      # - the harmonise_per_plot_layer anyway also reports the minimum and
      #   maximum values because of the same reasoning
      rowwise() %>%
      mutate(
        bulk_density_min = ifelse(
          !is.na(bulk_density),
          round(min(c_across(all_of(c(bulk_density_columns,
                                      "bulk_density_pre_gapfill"))),
                    na.rm = TRUE)),
          NA),
        bulk_density_max = ifelse(
          !is.na(bulk_density),
          round(max(c_across(all_of(c(bulk_density_columns,
                                      "bulk_density_pre_gapfill"))),
                    na.rm = TRUE)),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(bulk_density = round(bulk_density)) %>%
      relocate(any_of(c("bulk_density_min", "bulk_density_max",
                        "bulk_density_source")),
               .after = "bulk_density") # %>%
      # select(-any_of(c(
      #   "bulk_density_afscdb",
      #   "som_bulk_density", "som_bulk_density_survey_year",
      #   "som_bulk_density_min", "som_bulk_density_max",
      #   "pfh_horizon_bulk_dens_measure",
      #   "pfh_horizon_bulk_dens_measure_survey_year",
      #   "pfh_horizon_bulk_dens_measure_min",
      #   "pfh_horizon_bulk_dens_measure_max",
      #   "pfh_horizon_bulk_dens_est", "pfh_horizon_bulk_dens_est_survey_year",
      #   "pfh_horizon_bulk_dens_est_min", "pfh_horizon_bulk_dens_est_max",
      #   "swc_bulk_density", "swc_bulk_density_survey_year",
      #   "swc_bulk_density_min", "swc_bulk_density_max",
      #   "bulk_density_pre_gapfill")))


    cat("\nData sources 'bulk_density' after internal gap-filling:\n")
    print(table(
      df %>%
        mutate(bulk_density_source_harm = case_when(
          grepl("fscdb", bulk_density_source, ignore.case = TRUE) ~
            "old database versions",
          grepl("pir", bulk_density_source, ignore.case = TRUE) ~ "PIRs",
          grepl("som", bulk_density_source) ~ "som",
          grepl("swc", bulk_density_source) ~ "swc",
          grepl("pfh.*measure|measure.*pfh", bulk_density_source) ~
            "pfh (measured)",
          grepl("pfh.*est|est.*pfh", bulk_density_source) ~ "pfh (estimated)",
          .default = bulk_density_source)) %>%
        pull(bulk_density_source_harm)))




    ### 3.1.2. Coarse fragments ----

    cat("\n\nCombine coarse fragments columns\n")

    coarse_fragment_vol_columns <- c(
      df %>%
      select(contains("coarse_frag"),
             -contains("source"), -contains("survey_year"),
             -contains("orig"), -contains("fscdb")) %>%
      names,
      "coarse_fragment_vol_pre_gapfill")

    df <- df %>%
      # Make a copy of the current column
      mutate(coarse_fragment_vol_pre_gapfill = coarse_fragment_vol) %>%
      # Gap-fill
      mutate(
        coarse_fragment_vol_source = ifelse(
          !is.na(coarse_fragment_vol) &
            (!is.na(som_coarse_fragment_vol_survey_year) &
               (survey_year == som_coarse_fragment_vol_survey_year)),
          # Original data are kept
          coarse_fragment_vol_source,
          # Sources for gap-filling
          case_when(
            !is.na(som_coarse_fragment_vol) ~
              paste0("som_", .data$som_coarse_fragment_vol_survey_year),
            !is.na(pfh_coarse_fragment_vol_converted) ~
              paste0("pfh_weight_",
                     .data$pfh_coarse_fragment_vol_converted_survey_year),
            !is.na(pfh_coarse_fragment_vol_avg) ~
              paste0("pfh_code_vol_",
                     .data$pfh_coarse_fragment_vol_avg_survey_year))),
        coarse_fragment_vol = ifelse(
          !is.na(coarse_fragment_vol) &
            (!is.na(som_coarse_fragment_vol_survey_year) &
                  (survey_year == som_coarse_fragment_vol_survey_year)),
          # Keep the original cf data
          coarse_fragment_vol,
          # Else, gap-fill
          coalesce(
            som_coarse_fragment_vol,
            pfh_coarse_fragment_vol_converted,
            pfh_coarse_fragment_vol_avg))) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        coarse_fragment_vol_min = ifelse(
          !is.na(coarse_fragment_vol),
          round(min(c_across(all_of(coarse_fragment_vol_columns)),
                    na.rm = TRUE), 1),
          NA),
        coarse_fragment_vol_max = ifelse(
          !is.na(coarse_fragment_vol),
          round(max(c_across(all_of(coarse_fragment_vol_columns)),
                    na.rm = TRUE), 1),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(coarse_fragment_vol = round(coarse_fragment_vol, 1)) %>%
      relocate(any_of(c("coarse_fragment_vol_min",
                        "coarse_fragment_vol_max",
                        "coarse_fragment_vol_source")),
               .after = "coarse_fragment_vol") # %>%
      # select(-c(
      #   "coarse_fragment_vol_afscdb",
      #   "som_coarse_fragment_vol", "som_coarse_fragment_vol_survey_year",
      #   "som_coarse_fragment_vol_min", "som_coarse_fragment_vol_max",
      #   "pfh_coarse_fragment_vol_converted",
      #   "pfh_coarse_fragment_vol_converted_survey_year",
      #   "pfh_coarse_fragment_vol_converted_min",
      #   "pfh_coarse_fragment_vol_converted_max",
      #   "pfh_coarse_fragment_vol_avg",
      #   "pfh_coarse_fragment_vol_avg_survey_year",
      #   "pfh_coarse_fragment_vol_avg_min", "pfh_coarse_fragment_vol_avg_max",
      #   "coarse_fragment_vol_pre_gapfill"))




    ### 3.1.3. Clay ----

    cat("\n\nCombine clay columns\n")

    part_size_clay_columns <- c(
      df %>%
        select(contains("clay"),
               -contains("source"), -contains("survey_year"),
               -contains("_rt"), -contains("_loq"),
               -contains("orig"), -contains("fscdb")) %>%
        names,
      "part_size_clay_pre_gapfill")

    df <- df %>%
      # Make a copy of the current column
      mutate(part_size_clay_pre_gapfill = coarse_fragment_vol) %>%
      # Gap-fill
      mutate(
        part_size_clay_source = ifelse(
          !is.na(part_size_clay) &
            (!is.na(som_texture_survey_year) &
               (survey_year == som_texture_survey_year)),
          # Original data are kept
          part_size_clay_source,
          # Sources for gap-filling
          case_when(
            !is.na(som_part_size_clay) ~
              paste0("som_", .data$som_texture_survey_year),
            !is.na(pfh_horizon_clay) ~
              paste0("pfh_",
                     .data$pfh_texture_survey_year))),
        part_size_clay = ifelse(
          !is.na(part_size_clay) &
            (!is.na(som_texture_survey_year) &
               (survey_year == som_texture_survey_year)),
          # Keep the original data
          part_size_clay,
          # Else, gap-fill
          coalesce(
            som_part_size_clay,
            pfh_horizon_clay))) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        part_size_clay_min = ifelse(
          !is.na(part_size_clay),
          round(min(c_across(all_of(part_size_clay_columns)),
                    na.rm = TRUE), 1),
          NA),
        part_size_clay_max = ifelse(
          !is.na(part_size_clay),
          round(max(c_across(all_of(part_size_clay_columns)),
                    na.rm = TRUE), 1),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(part_size_clay = round(part_size_clay, 1)) %>%
      relocate(any_of(c("part_size_clay_min",
                        "part_size_clay_max",
                        "part_size_clay_source")),
               .after = "part_size_clay") # %>%
      # select(-c(
      #   "part_size_clay_afscdb",
      #   "som_part_size_clay",
      #   "som_part_size_clay_min", "som_part_size_clay_max",
      #   "pfh_horizon_clay",
      #   "pfh_horizon_clay_min",
      #   "pfh_horizon_clay_max",
      #   "part_size_clay_pre_gapfill"))



    ### 3.1.4. Silt ----

    cat("\n\nCombine silt columns\n")

    part_size_silt_columns <- c(
      df %>%
        select(contains("silt"),
               -contains("source"), -contains("survey_year"),
               -contains("_rt"), -contains("_loq"),
               -contains("orig"), -contains("fscdb")) %>%
        names,
      "part_size_silt_pre_gapfill")

    df <- df %>%
      # Make a copy of the current column
      mutate(part_size_silt_pre_gapfill = coarse_fragment_vol) %>%
      # Gap-fill
      mutate(
        part_size_silt_source = ifelse(
          !is.na(part_size_silt) &
            (!is.na(som_texture_survey_year) &
               (survey_year == som_texture_survey_year)),
          # Original data are kept
          part_size_silt_source,
          # Sources for gap-filling
          case_when(
            !is.na(som_part_size_silt) ~
              paste0("som_", .data$som_texture_survey_year),
            !is.na(pfh_horizon_silt) ~
              paste0("pfh_",
                     .data$pfh_texture_survey_year))),
        part_size_silt = ifelse(
          !is.na(part_size_silt) &
            (!is.na(som_texture_survey_year) &
               (survey_year == som_texture_survey_year)),
          # Keep the original data
          part_size_silt,
          # Else, gap-fill
          coalesce(
            som_part_size_silt,
            pfh_horizon_silt))) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        part_size_silt_min = ifelse(
          !is.na(part_size_silt),
          round(min(c_across(all_of(part_size_silt_columns)),
                    na.rm = TRUE), 1),
          NA),
        part_size_silt_max = ifelse(
          !is.na(part_size_silt),
          round(max(c_across(all_of(part_size_silt_columns)),
                    na.rm = TRUE), 1),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(part_size_silt = round(part_size_silt, 1)) %>%
      relocate(any_of(c("part_size_silt_min",
                        "part_size_silt_max",
                        "part_size_silt_source")),
               .after = "part_size_silt") # %>%
    # select(-c(
    #   "part_size_silt_afscdb",
    #   "som_part_size_silt",
    #   "som_part_size_silt_min", "som_part_size_silt_max",
    #   "pfh_horizon_silt",
    #   "pfh_horizon_silt_min",
    #   "pfh_horizon_silt_max",
    #   "part_size_silt_pre_gapfill"))



    ### 3.1.5. Sand ----

    cat("\n\nCombine sand columns\n")

    part_size_sand_columns <- c(
      df %>%
        select(contains("sand"),
               -contains("source"), -contains("survey_year"),
               -contains("_rt"), -contains("_loq"),
               -contains("orig"), -contains("fscdb")) %>%
        names,
      "part_size_sand_pre_gapfill")

    df <- df %>%
      # Make a copy of the current column
      mutate(part_size_sand_pre_gapfill = coarse_fragment_vol) %>%
      # Gap-fill
      mutate(
        part_size_sand_source = ifelse(
          !is.na(part_size_sand) &
            (!is.na(som_texture_survey_year) &
               (survey_year == som_texture_survey_year)),
          # Original data are kept
          part_size_sand_source,
          # Sources for gap-filling
          case_when(
            !is.na(som_part_size_sand) ~
              paste0("som_", .data$som_texture_survey_year),
            !is.na(pfh_horizon_sand) ~
              paste0("pfh_",
                     .data$pfh_texture_survey_year))),
        part_size_sand = ifelse(
          !is.na(part_size_sand) &
            (!is.na(som_texture_survey_year) &
               (survey_year == som_texture_survey_year)),
          # Keep the original data
          part_size_sand,
          # Else, gap-fill
          coalesce(
            som_part_size_sand,
            pfh_horizon_sand))) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        part_size_sand_min = ifelse(
          !is.na(part_size_sand),
          round(min(c_across(all_of(part_size_sand_columns)),
                    na.rm = TRUE), 1),
          NA),
        part_size_sand_max = ifelse(
          !is.na(part_size_sand),
          round(max(c_across(all_of(part_size_sand_columns)),
                    na.rm = TRUE), 1),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(part_size_sand = round(part_size_sand, 1)) %>%
      relocate(any_of(c("part_size_sand_min",
                        "part_size_sand_max",
                        "part_size_sand_source")),
               .after = "part_size_sand") # %>%
    # select(-c(
    #   "part_size_sand_afscdb",
    #   "som_part_size_sand", "som_texture_survey_year",
    #   "som_part_size_sand_min", "som_part_size_sand_max",
    #   "pfh_horizon_sand", "pfh_texture_survey_year",
    #   "pfh_horizon_sand_min",
    #   "pfh_horizon_sand_max",
    #   "part_size_sand_pre_gapfill"))






    ### 3.1.6. Organic layer weight ----

    assertthat::assert_that(
      "bulk_density_layer_weight" %in% names(df),
      msg = paste0("Please do not remove 'bulk_density_layer_weight' ",
                   "before the end of this script (e.g. step 3.1.1)."))

    cat("\n\nCombine organic layer weight columns\n")

    organic_layer_weight_columns <- c(
      "organic_layer_weight",
      "organic_layer_weight_bd",
      "organic_layer_weight_bd_min",
      "organic_layer_weight_bd_max")

    df <- df %>%
      # Gap-fill
      mutate(
        # Derive organic layer weight from bulk density
        organic_layer_weight_bd = ifelse(
            !is.na(bulk_density) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density * (.data$layer_thickness * 1e-2), 2),
          NA_real_),
        organic_layer_weight_bd_min = ifelse(
          !is.na(bulk_density_min) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density_min * (.data$layer_thickness * 1e-2), 2),
          NA_real_),
        organic_layer_weight_bd_max = ifelse(
          !is.na(bulk_density_max) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density_min * (.data$layer_thickness * 1e-2), 2),
          NA_real_),
        # Remove implausible organic_layer_weight data
        # Note that implausible data should be removed in the column
        # bulk_density_layer_weight in step 3.1.1. (only for records for
        # which the layer thickness is known)
        organic_layer_weight_source = ifelse(
          layer_type %in% c("peat", "forest_floor"),
          ifelse((is.na(.data$layer_thickness) &
                    !is.na(.data$organic_layer_weight)) |
                   (!is.na(.data$layer_thickness) &
                      !is.na(.data$organic_layer_weight) &
                      !is.na(.data$bulk_density_layer_weight)),
                 organic_layer_weight_source,
                 NA_character_),
          NA_character_),
        organic_layer_weight = ifelse(
          layer_type %in% c("peat", "forest_floor"),
          ifelse((is.na(.data$layer_thickness) &
                    !is.na(.data$organic_layer_weight)) |
                   (!is.na(.data$layer_thickness) &
                      !is.na(.data$organic_layer_weight) &
                      # So if the parameter below is not NA, the reported
                      # organic_layer_weight is plausible
                      !is.na(.data$bulk_density_layer_weight)),
                 organic_layer_weight,
                 NA_real_),
          NA_real_),
        # Gap-fill
        organic_layer_weight_source = ifelse(
          layer_type %in% c("peat", "forest_floor"),
          # Sources for gap-filling
          case_when(
            !is.na(organic_layer_weight) ~
              .data$organic_layer_weight_source,
            !is.na(organic_layer_weight_bd) ~
              paste0("bulk_density (", .data$bulk_density_source, ")")),
          NA_character_),
        organic_layer_weight = ifelse(
          layer_type %in% c("peat", "forest_floor"),
          coalesce(organic_layer_weight,
                   organic_layer_weight_bd),
          NA_real_)) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        organic_layer_weight_min = ifelse(
          !is.na(organic_layer_weight),
          round(min(c_across(all_of(organic_layer_weight_columns)),
                    na.rm = TRUE), 2),
          NA),
        organic_layer_weight_max = ifelse(
          !is.na(organic_layer_weight),
          round(max(c_across(all_of(organic_layer_weight_columns)),
                    na.rm = TRUE), 2),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(organic_layer_weight = round(organic_layer_weight, 2)) %>%
      relocate(any_of(c("organic_layer_weight_min",
                        "organic_layer_weight_max",
                        "organic_layer_weight_source")),
               .after = "organic_layer_weight") # %>%
      # select(-c(
      #   "organic_layer_weight_afscdb", "organic_layer_weight_bd",
      #   "organic_layer_weight_bd_min", "organic_layer_weight_bd_max",
      #   "organic_layer_weight_bd_median",
      #   "bulk_density_layer_weight"))


  }  # End of "if som"





  ## 3.2. "pfh" ----

  if (survey_form_type == "pfh") {

    ### 3.2.1. Bulk density ----

    cat("\n\nCombine bulk density columns\n")

    df <- df %>%
      # Rename "horizon_bulk_dens_measure" as "bulk_density"
      rename(bulk_density = horizon_bulk_dens_measure) %>%
      rename(bulk_density_source = horizon_bulk_dens_measure_source) %>%
      # Update the column bulk_density_source so that it is clear
      # that "pfh" refers to the original column "horizon_bulk_dens_measure"
      mutate(bulk_density_source = case_when(
        bulk_density_source == "pfh" ~ "pfh_measure",
        bulk_density_source == "pfh (corrected units)" ~
          "pfh_measure (corrected units)",
        TRUE ~ bulk_density_source))

    bulk_density_columns <-
      df %>%
      select(contains("bulk_dens"),
             -contains("source"), -contains("survey_year"),
             -contains("orig"), -contains("fscdb")) %>%
      names

    quant_bd <- c(6, 1991) # Based on minimum and maximum in
    # bd_upper and bd_lower functions

    df <- df %>%
      # Make a copy of the current bulk density column,
      # since the bulk density column will be changed,
      # while the current values are relevant for the uncertainty range
      mutate(bulk_density_pre_gapfill = bulk_density) %>%
      mutate(
        bd_up = case_when(
          layer_type == "mineral" ~ bd_upper(.data$organic_carbon_total),
          layer_type == "peat" ~ bd_peat_upper_95,
          layer_type == "forest_floor" & !grepl("L", code_layer) ~
            bd_ff_upper_95,
          TRUE ~ quant_bd[2]),
        bd_low = case_when(
          layer_type == "mineral" ~ bd_lower(.data$organic_carbon_total),
          layer_type == "peat" ~ bd_peat_lower_5,
          layer_type == "forest_floor" & !grepl("L", code_layer) ~
            bd_ff_lower_5,
          TRUE ~ quant_bd[1])) %>%
      # Remove values outside of the bulk density range
      # This needs to be repeated for bulk density (next to the
      # harmonise_per_plot_layer function). The latter is less strict,
      # especially for sw_swc, for which no TOC is reported.
      mutate_at((bulk_density_columns),
                ~ ifelse(
                  (!is.na(.)) &
                    (. > bd_low & . < bd_up),
                  .,
                  NA)) %>%
      mutate(bulk_density_source = ifelse(
        (!is.na(bulk_density_source) &
           !is.na(bulk_density)),
        bulk_density_source,
        NA)) %>%
      select(-bd_up, -bd_low)
      # bulk_density_layer_weight is not reported since by default
      # no organic layer weight is reported in pfh


    if (survey_form == "so_pfh") {

    df <- df %>%
      # Gap-fill
      mutate(
        bulk_density_source = ifelse(
          !is.na(bulk_density) &
            ((layer_type %in% c("forest_floor", "peat")) |
               (layer_type %in% c("mineral") &
                  !is.na(pfh_horizon_bulk_dens_measure_survey_year) &
                  (survey_year == pfh_horizon_bulk_dens_measure_survey_year))),
          # Original data are kept
          bulk_density_source,
          # Sources for gap-filling
          case_when(
            !is.na(pfh_horizon_bulk_dens_measure) ~
              paste0("pfh_measure_",
                     .data$pfh_horizon_bulk_dens_measure_survey_year),
            !is.na(swc_bulk_density) ~
              paste0("swc_", .data$swc_bulk_density_survey_year),
            !is.na(som_bulk_density) ~
              paste0("som_",
                     .data$som_bulk_density_survey_year),
            !is.na(pfh_horizon_bulk_dens_est) ~
              paste0("pfh_est_", .data$pfh_horizon_bulk_dens_est_survey_year))),
        bulk_density = ifelse(
          !is.na(bulk_density) &
            ((layer_type %in% c("forest_floor", "peat")) |
               (layer_type %in% c("mineral") &
                  !is.na(pfh_horizon_bulk_dens_measure_survey_year) &
                  (survey_year == pfh_horizon_bulk_dens_measure_survey_year))),
          # Keep the original bulk density data
          bulk_density,
          # Else, gap-fill
          coalesce(
            pfh_horizon_bulk_dens_measure,
            swc_bulk_density,
            som_bulk_density,
            pfh_horizon_bulk_dens_est)))

    }

    if (survey_form == "s1_pfh") {

      df <- df %>%
        # Gap-fill
        mutate(
          bulk_density_source = ifelse(
            !is.na(bulk_density) &
              ((layer_type %in% c("forest_floor", "peat")) |
                 (layer_type %in% c("mineral") &
                    !is.na(pfh_horizon_bulk_dens_measure_survey_year) &
                    (survey_year ==
                       pfh_horizon_bulk_dens_measure_survey_year))),
            # Original data are kept
            bulk_density_source,
            # Sources for gap-filling
            case_when(
              !is.na(pfh_horizon_bulk_dens_measure) ~
                paste0("pfh_measure_",
                       .data$pfh_horizon_bulk_dens_measure_survey_year),
              !is.na(som_bulk_density) ~
                paste0("som_",
                       .data$som_bulk_density_survey_year),
              !is.na(pfh_horizon_bulk_dens_est) ~
                paste0("pfh_est_",
                       .data$pfh_horizon_bulk_dens_est_survey_year))),
          bulk_density = ifelse(
            !is.na(bulk_density) &
              ((layer_type %in% c("forest_floor", "peat")) |
                 (layer_type %in% c("mineral") &
                    !is.na(pfh_horizon_bulk_dens_measure_survey_year) &
                    (survey_year ==
                       pfh_horizon_bulk_dens_measure_survey_year))),
            # Keep the original bulk density data
            bulk_density,
            # Else, gap-fill
            coalesce(
              pfh_horizon_bulk_dens_measure,
              som_bulk_density,
              pfh_horizon_bulk_dens_est)))
    }

    df <- df %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        bulk_density_min = ifelse(
          !is.na(bulk_density),
          round(min(c_across(all_of(c(bulk_density_columns,
                                      "bulk_density_pre_gapfill"))),
                    na.rm = TRUE)),
          NA),
        bulk_density_max = ifelse(
          !is.na(bulk_density),
          round(max(c_across(all_of(c(bulk_density_columns,
                                      "bulk_density_pre_gapfill"))),
                    na.rm = TRUE)),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(bulk_density = round(bulk_density)) %>%
      relocate(any_of(c("bulk_density_min", "bulk_density_max",
                        "bulk_density_source")),
               .after = "bulk_density") # %>%
      # select(-any_of(c(
      #   "bulk_density_afscdb",
      #   "som_bulk_density", "som_bulk_density_survey_year",
      #   "som_bulk_density_min", "som_bulk_density_max",
      #   "pfh_horizon_bulk_dens_measure",
      #   "pfh_horizon_bulk_dens_measure_survey_year",
      #   "pfh_horizon_bulk_dens_measure_min",
      #   "pfh_horizon_bulk_dens_measure_max",
      #   "pfh_horizon_bulk_dens_est", "pfh_horizon_bulk_dens_est_survey_year",
      #   "pfh_horizon_bulk_dens_est_min", "pfh_horizon_bulk_dens_est_max",
      #   "swc_bulk_density", "swc_bulk_density_survey_year",
      #   "swc_bulk_density_min", "swc_bulk_density_max",
      #   "horizon_bulk_dens_est",
      #   "bulk_density_pre_gapfill")))


    cat("\nData sources 'bulk_density' after internal gap-filling:\n")
    print(table(
      df %>%
        mutate(bulk_density_source_harm = case_when(
          grepl("fscdb", bulk_density_source, ignore.case = TRUE) ~
            "old database versions",
          grepl("pir", bulk_density_source, ignore.case = TRUE) ~ "PIRs",
          grepl("som", bulk_density_source) ~ "som",
          grepl("swc", bulk_density_source) ~ "swc",
          grepl("pfh.*measure|measure.*pfh", bulk_density_source) ~
            "pfh (measured)",
          grepl("pfh.*est|est.*pfh", bulk_density_source) ~ "pfh (estimated)",
          .default = bulk_density_source)) %>%
        pull(bulk_density_source_harm)))




    ### 3.2.2. Coarse fragments ----

    cat("\n\nCombine coarse fragments columns\n")

    df <- df %>%
      # Rename "horizon_bulk_dens_measure" as "bulk_density"
      rename(coarse_fragment_vol = coarse_fragment_vol_converted) %>%
      rename(coarse_fragment_vol_source = horizon_coarse_weight_source) %>%
      # Update the column coarse_fragment_vol_source so that it is clear
      # that "pfh" refers to the original column "horizon_coarse_weight"
      mutate(coarse_fragment_vol_source = case_when(
        coarse_fragment_vol_source == "pfh" ~ "pfh_weight",
        TRUE ~ coarse_fragment_vol_source))

    coarse_fragment_vol_columns <- c(
      df %>%
        select(contains("coarse_frag"),
               -contains("source"), -contains("survey_year"),
               -contains("orig"), -contains("fscdb")) %>%
        names,
      "coarse_fragment_vol_pre_gapfill")


    df <- df %>%
      # Make a copy of the current column
      mutate(coarse_fragment_vol_pre_gapfill = coarse_fragment_vol) %>%
      # Gap-fill
      mutate(
        coarse_fragment_vol_source = ifelse(
          !is.na(coarse_fragment_vol) &
            (!is.na(pfh_coarse_fragment_vol_converted_survey_year) &
               (survey_year == pfh_coarse_fragment_vol_converted_survey_year)),
          # Original data are kept
          coarse_fragment_vol_source,
          # Sources for gap-filling
          case_when(
            !is.na(pfh_coarse_fragment_vol_converted) ~
              paste0("pfh_weight_",
                     .data$pfh_coarse_fragment_vol_converted_survey_year),
            !is.na(som_coarse_fragment_vol) ~
              paste0("som_", .data$som_coarse_fragment_vol_survey_year),
            !is.na(pfh_coarse_fragment_vol_avg) ~
              paste0("pfh_code_vol_",
                     .data$pfh_coarse_fragment_vol_avg_survey_year))),
        coarse_fragment_vol = ifelse(
          !is.na(coarse_fragment_vol) &
            (!is.na(pfh_coarse_fragment_vol_converted_survey_year) &
               (survey_year == pfh_coarse_fragment_vol_converted_survey_year)),
          # Keep the original cf data
          coarse_fragment_vol,
          # Else, gap-fill
          coalesce(
            pfh_coarse_fragment_vol_converted,
            som_coarse_fragment_vol,
            pfh_coarse_fragment_vol_avg))) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        coarse_fragment_vol_min = ifelse(
          !is.na(coarse_fragment_vol),
          round(min(c_across(all_of(coarse_fragment_vol_columns)),
                    na.rm = TRUE), 1),
          NA),
        coarse_fragment_vol_max = ifelse(
          !is.na(coarse_fragment_vol),
          round(max(c_across(all_of(coarse_fragment_vol_columns)),
                    na.rm = TRUE), 1),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(coarse_fragment_vol = round(coarse_fragment_vol, 1)) %>%
      relocate(any_of(c("coarse_fragment_vol_min",
                        "coarse_fragment_vol_max",
                        "coarse_fragment_vol_source")),
               .after = "coarse_fragment_vol") # %>%
      # select(-c(
      #   "coarse_fragment_vol_afscdb",
      #   "som_coarse_fragment_vol", "som_coarse_fragment_vol_survey_year",
      #   "som_coarse_fragment_vol_min", "som_coarse_fragment_vol_max",
      #   "pfh_coarse_fragment_vol_converted",
      #   "pfh_coarse_fragment_vol_converted_survey_year",
      #   "pfh_coarse_fragment_vol_converted_min",
      #   "pfh_coarse_fragment_vol_converted_max",
      #   "pfh_coarse_fragment_vol_avg",
      #   "pfh_coarse_fragment_vol_avg_survey_year",
      #   "pfh_coarse_fragment_vol_avg_min", "pfh_coarse_fragment_vol_avg_max",
      #   "coarse_fragment_vol_pre_gapfill",
      #   "horizon_coarse_weight",
      #   "coarse_fragment_vol_avg"))


    ### 3.2.3. Clay ----

    cat("\n\nCombine clay columns\n")

    df <- df %>%
      # Rename "horizon_bulk_dens_measure" as "bulk_density"
      rename(part_size_clay = horizon_clay) %>%
      rename(part_size_clay_source = horizon_clay_source)

    part_size_clay_columns <- c(
      df %>%
        select(contains("clay"),
               -contains("source"), -contains("survey_year"),
               -contains("_rt"), -contains("_loq"),
               -contains("orig"), -contains("fscdb")) %>%
        names,
      "part_size_clay_pre_gapfill")

    df <- df %>%
      # Make a copy of the current column
      mutate(part_size_clay_pre_gapfill = part_size_clay) %>%
      # Gap-fill
      mutate(
        part_size_clay_source = ifelse(
          !is.na(part_size_clay) &
            (!is.na(pfh_texture_survey_year) &
               (survey_year == pfh_texture_survey_year)),
          # Original data are kept
          part_size_clay_source,
          # Sources for gap-filling
          case_when(
            !is.na(pfh_horizon_clay) ~
              paste0("pfh_",
                     .data$pfh_texture_survey_year),
            !is.na(som_part_size_clay) ~
              paste0("som_", .data$som_texture_survey_year))),
        part_size_clay = ifelse(
          !is.na(part_size_clay) &
            (!is.na(pfh_texture_survey_year) &
               (survey_year == pfh_texture_survey_year)),
          # Keep the original data
          part_size_clay,
          # Else, gap-fill
          coalesce(
            pfh_horizon_clay,
            som_part_size_clay))) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        part_size_clay_min = ifelse(
          !is.na(part_size_clay),
          round(min(c_across(all_of(part_size_clay_columns)),
                    na.rm = TRUE), 1),
          NA),
        part_size_clay_max = ifelse(
          !is.na(part_size_clay),
          round(max(c_across(all_of(part_size_clay_columns)),
                    na.rm = TRUE), 1),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(part_size_clay = round(part_size_clay, 1)) %>%
      relocate(any_of(c("part_size_clay_min",
                        "part_size_clay_max",
                        "part_size_clay_source")),
               .after = "part_size_clay") # %>%
    # select(-c(
    #   "part_size_clay_afscdb",
    #   "som_part_size_clay",
    #   "som_part_size_clay_min", "som_part_size_clay_max",
    #   "pfh_horizon_clay",
    #   "pfh_horizon_clay_min",
    #   "pfh_horizon_clay_max",
    #   "part_size_clay_pre_gapfill"))



    ### 3.2.4. Silt ----

    cat("\n\nCombine silt columns\n")

    df <- df %>%
      # Rename "horizon_bulk_dens_measure" as "bulk_density"
      rename(part_size_silt = horizon_silt) %>%
      rename(part_size_silt_source = horizon_silt_source)

    part_size_silt_columns <- c(
      df %>%
        select(contains("silt"),
               -contains("source"), -contains("survey_year"),
               -contains("_rt"), -contains("_loq"),
               -contains("orig"), -contains("fscdb")) %>%
        names,
      "part_size_silt_pre_gapfill")

    df <- df %>%
      # Make a copy of the current column
      mutate(part_size_silt_pre_gapfill = part_size_silt) %>%
      # Gap-fill
      mutate(
        part_size_silt_source = ifelse(
          !is.na(part_size_silt) &
            (!is.na(pfh_texture_survey_year) &
               (survey_year == pfh_texture_survey_year)),
          # Original data are kept
          part_size_silt_source,
          # Sources for gap-filling
          case_when(
            !is.na(pfh_horizon_silt) ~
              paste0("pfh_",
                     .data$pfh_texture_survey_year),
            !is.na(som_part_size_silt) ~
              paste0("som_", .data$som_texture_survey_year))),
        part_size_silt = ifelse(
          !is.na(part_size_silt) &
            (!is.na(pfh_texture_survey_year) &
               (survey_year == pfh_texture_survey_year)),
          # Keep the original data
          part_size_silt,
          # Else, gap-fill
          coalesce(
            pfh_horizon_silt,
            som_part_size_silt))) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        part_size_silt_min = ifelse(
          !is.na(part_size_silt),
          round(min(c_across(all_of(part_size_silt_columns)),
                    na.rm = TRUE), 1),
          NA),
        part_size_silt_max = ifelse(
          !is.na(part_size_silt),
          round(max(c_across(all_of(part_size_silt_columns)),
                    na.rm = TRUE), 1),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(part_size_silt = round(part_size_silt, 1)) %>%
      relocate(any_of(c("part_size_silt_min",
                        "part_size_silt_max",
                        "part_size_silt_source")),
               .after = "part_size_silt") # %>%
    # select(-c(
    #   "part_size_silt_afscdb",
    #   "som_part_size_silt",
    #   "som_part_size_silt_min", "som_part_size_silt_max",
    #   "pfh_horizon_silt",
    #   "pfh_horizon_silt_min",
    #   "pfh_horizon_silt_max",
    #   "part_size_silt_pre_gapfill"))



    ### 3.2.5. Sand ----

    cat("\n\nCombine sand columns\n")

    df <- df %>%
      # Rename "horizon_bulk_dens_measure" as "bulk_density"
      rename(part_size_sand = horizon_sand) %>%
      rename(part_size_sand_source = horizon_sand_source)

    part_size_sand_columns <- c(
      df %>%
        select(contains("sand"),
               -contains("source"), -contains("survey_year"),
               -contains("_rt"), -contains("_loq"),
               -contains("orig"), -contains("fscdb")) %>%
        names,
      "part_size_sand_pre_gapfill")

    df <- df %>%
      # Make a copy of the current column
      mutate(part_size_sand_pre_gapfill = part_size_sand) %>%
      # Gap-fill
      mutate(
        part_size_sand_source = ifelse(
          !is.na(part_size_sand) &
            (!is.na(pfh_texture_survey_year) &
               (survey_year == pfh_texture_survey_year)),
          # Original data are kept
          part_size_sand_source,
          # Sources for gap-filling
          case_when(
            !is.na(pfh_horizon_sand) ~
              paste0("pfh_",
                     .data$pfh_texture_survey_year),
            !is.na(som_part_size_sand) ~
              paste0("som_", .data$som_texture_survey_year))),
        part_size_sand = ifelse(
          !is.na(part_size_sand) &
            (!is.na(pfh_texture_survey_year) &
               (survey_year == pfh_texture_survey_year)),
          # Keep the original data
          part_size_sand,
          # Else, gap-fill
          coalesce(
            pfh_horizon_sand,
            som_part_size_sand))) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        part_size_sand_min = ifelse(
          !is.na(part_size_sand),
          round(min(c_across(all_of(part_size_sand_columns)),
                    na.rm = TRUE), 1),
          NA),
        part_size_sand_max = ifelse(
          !is.na(part_size_sand),
          round(max(c_across(all_of(part_size_sand_columns)),
                    na.rm = TRUE), 1),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(part_size_sand = round(part_size_sand, 1)) %>%
      relocate(any_of(c("part_size_sand_min",
                        "part_size_sand_max",
                        "part_size_sand_source")),
               .after = "part_size_sand") # %>%
    # select(-c(
    #   "part_size_sand_afscdb",
    #   "som_part_size_sand",
    #   "som_part_size_sand_min", "som_part_size_sand_max",
    #   "pfh_horizon_sand",
    #   "pfh_horizon_sand_min",
    #   "pfh_horizon_sand_max",
    #   "part_size_sand_pre_gapfill"))


    ### 3.2.6. Organic layer weight ----

    cat("\n\nCombine organic layer weight columns\n")

    organic_layer_weight_columns <- c(
      "organic_layer_weight",
      "organic_layer_weight_bd",
      "organic_layer_weight_bd_min",
      "organic_layer_weight_bd_max")

    df <- df %>%
      # Gap-fill
      mutate(
        # Derive organic layer weight from bulk density
        organic_layer_weight_bd = ifelse(
          !is.na(bulk_density) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density * (.data$layer_thickness * 1e-2), 2),
          NA_real_),
        organic_layer_weight_bd_min = ifelse(
          !is.na(bulk_density_min) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density_min * (.data$layer_thickness * 1e-2), 2),
          NA_real_),
        organic_layer_weight_bd_max = ifelse(
          !is.na(bulk_density_max) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density_min * (.data$layer_thickness * 1e-2), 2),
          NA_real_),
        # Gap-fill
        organic_layer_weight_source = ifelse(
          layer_type %in% c("peat", "forest_floor"),
          # Sources for gap-filling
          case_when(
            !is.na(organic_layer_weight_bd) ~
              paste0("bulk_density (", .data$bulk_density_source, ")")),
          NA_character_),
        organic_layer_weight = ifelse(
          layer_type %in% c("peat", "forest_floor"),
          organic_layer_weight_bd,
          NA_real_)) %>%
      # Min-max intervals
      rowwise() %>%
      mutate(
        organic_layer_weight_min = ifelse(
          !is.na(organic_layer_weight),
          round(min(c_across(all_of(organic_layer_weight_columns)),
                    na.rm = TRUE), 2),
          NA),
        organic_layer_weight_max = ifelse(
          !is.na(organic_layer_weight),
          round(max(c_across(all_of(organic_layer_weight_columns)),
                    na.rm = TRUE), 2),
          NA)) %>%
      ungroup() %>%
      # Round
      mutate(organic_layer_weight = round(organic_layer_weight, 2)) %>%
      relocate(any_of(c("organic_layer_weight_min",
                        "organic_layer_weight_max",
                        "organic_layer_weight_source")),
               .after = "organic_layer_weight") # %>%
    # select(-c(
    #   "organic_layer_weight_afscdb", "organic_layer_weight_bd",
    #   "organic_layer_weight_bd_min", "organic_layer_weight_bd_max",
    #   "organic_layer_weight_bd_median"))




  } # End of "if pfh"














  # . ----
  # Gap-fill concentrations in forest floor ----

  if (gapfill_ff_concentrations == TRUE) {

    # Some profiles have a forest floor with known organic_layer_weight
    # but no analytical data, e.g. organic_carbon_total
    # It makes sense to gap-fill this across survey forms as long as the
    # survey years are roughly the same (+- three years), since
    # concentrations of organic layers are typically more similar.


    # 1. Add data to df ----

    source("./src/functions/depth_join.R")

    # som

    df2 <- get_env(paste0(code_survey, "_som"))

    cat(paste0(" \nAdd 'som' forest floor analytical data to '",
               survey_form, "'.\n"))

    df <- depth_join(df1 = df,
                     df2 = df2,
                     parameters = NULL,
                     prefix_parameters_in_df1 = "som_",
                     mode = "time_specific_ff_concentrations")

    # pfh

    df2 <- get_env(paste0(code_survey, "_pfh")) %>%
      rename(layer_limit_superior = horizon_limit_up) %>%
      rename(layer_limit_inferior = horizon_limit_low) %>%
      rename(code_layer = horizon_master) %>%
      rename(unique_survey_repetition = unique_survey_profile) %>%
      # Rename analytical parameters to join
      rename(organic_carbon_total = horizon_c_organic_total) %>%
      rename(organic_carbon_total_source = horizon_c_organic_total_source) %>%
      rename(n_total = horizon_n_total) %>%
      rename(n_total_source = horizon_n_total_source) %>%
      rename(exch_ca = horizon_exch_ca) %>%
      rename(exch_ca_source = horizon_exch_ca_source)

    cat(paste0(" \nAdd 'pfh' forest floor analytical data to '",
               survey_form, "'.\n"))

    df <- depth_join(df1 = df,
                     df2 = df2,
                     parameters = NULL,
                     prefix_parameters_in_df1 = "pfh_",
                     mode = "time_specific_ff_concentrations")


    # 2. Combine columns ----

    # Criteria for columns to actually be gap-filled:
    # (needs to be limited to a minimum - only if other data of the profile
    # are known and unique, i.e. no copy paste from other repetitions,
    # so that the profile would otherwise be incomplete for stock calculations
    # etc. The general rule is not to gap-fill chemical concentrations,
    # since physical parameters are gap-filled, and otherwise some profiles
    # would have no or hardly any distinct information at all if everything
    # is gap-filled.)

    # Gap-fill profiles with NA in forest floor if:
    # · There are any non-NA below-ground data:
    #   count_bg_layers_nona > 0
    # · The plot_id x survey_year contains less profiles with all forest floor
    #   layers known, than there are unique below-ground repetitions (profiles):
    #   count_all_ff_nona < count_distinct_bg_up


    # Define the parameters
    parameters_chem <- c(
      "organic_carbon_total", "n_total",
      "extrac_p", "extrac_s", "rea_fe", "rea_al", "exch_ca")

    # Generalized gap-fill processing function
    process_gapfill <- function(df, param) {
      # Dynamically create column names
      layer_type_col <- "layer_type"
      layer_number_col <- "layer_number"
      layer_number_bg_col <- "layer_number_bg"
      olw_col <- "organic_layer_weight"

      # Process profiles for the specific parameter
      profs_potential <- df %>%
        # Filter out redundant layers
        filter(!is.na(!!sym(layer_number_col))) %>%
        # Filter out forest floor layers without olw
        filter(!!sym(layer_type_col) != "forest_floor" |
                 (!!sym(layer_type_col) == "forest_floor" &
                    !is.na(!!sym(olw_col)))) %>%
        # Group per profile
        group_by(unique_survey_repetition,
                 unique_survey, code_country, code_plot) %>%
        reframe(
          any_ff_na = (any(!!sym(layer_type_col) == "forest_floor" &
                             is.na(!!sym(param)))),
          all_ff_nona ={
            ff_rows <- which(!!sym(layer_type_col) == "forest_floor")
            if (length(ff_rows) == 0) TRUE
            else all(!is.na(.data[[param]][ff_rows]))
          },
          count_bg_layers =
            length(which(!!sym(layer_type_col) != "forest_floor")),
          count_bg_layers_nona =
            length(which(!!sym(layer_type_col) != "forest_floor" &
                           !is.na(!!sym(param)))),
          !!paste0(param, "_bg_up") := {
            bg_rows <- which(!!sym(layer_number_bg_col) == 1)
            if (length(bg_rows) > 0) .data[[param]][bg_rows[1]]
            else NA_real_
          }
        ) %>%
        filter(count_bg_layers_nona > 0)

      # Process plots for gap-filling
      plots_gapfill <- profs_potential %>%
        # Group per unique_survey
        group_by(unique_survey, code_country, code_plot) %>%
        reframe(
          all_any_ff_na = all(any_ff_na == TRUE),
          any_ff_na_nona =
            any(any_ff_na == TRUE) & any(all_ff_nona == TRUE),
          count_all_ff_nona = length(which(.data$all_ff_nona == TRUE)),
          count_distinct_bg_up = n_distinct(!!sym(paste0(param, "_bg_up")),
                                            na.rm = TRUE)
        ) %>%
        filter(all_any_ff_na == TRUE |
                 any_ff_na_nona == TRUE) %>%
        filter(count_all_ff_nona < count_distinct_bg_up)

      # Profiles to gap-fill
      profs_gapfill <- profs_potential %>%
        filter(unique_survey %in% plots_gapfill$unique_survey) %>%
        filter(any_ff_na == TRUE)

      # Return a list with both intermediate and final results
      list(
        profs_potential = profs_potential,
        plots_gapfill = plots_gapfill,
        profs_gapfill = profs_gapfill
      )
    }

    # Filter parameters to only those that exist in df
    existing_params <- parameters_chem[parameters_chem %in% names(df)]

    # Apply the function to existing parameters only
    profs_gapfill <- lapply(existing_params, function(param) {
      process_gapfill(df, param)
    })

    # Name the list elements
    names(profs_gapfill) <- existing_params





    # # Carbon
    #
    # profs_potential_c <- df %>%
    #   # Filter out redundant layers
    #   filter(!is.na(layer_number)) %>%
    #   # Filter out forest floor layers without olw
    #   filter(layer_type != "forest_floor" |
    #            (layer_type == "forest_floor" &
    #               !is.na(organic_layer_weight))) %>%
    #   # Group per profile
    #   group_by(unique_survey_repetition,
    #            unique_survey, code_country, code_plot) %>%
    #   reframe(any_ff_na = (any(.data$layer_type == "forest_floor" &
    #                              is.na(.data$organic_carbon_total))),
    #           all_ff_nona =
    #             all(!is.na(organic_carbon_total[
    #               which(layer_type == "forest_floor")])),
    #           count_bg_layers =
    #             length(which(.data$layer_type != "forest_floor")),
    #           count_bg_layers_nona =
    #             length(which(.data$layer_type != "forest_floor" &
    #                      !is.na(.data$organic_carbon_total))),
    #           organic_carbon_total_bg_up =
    #             ifelse(any(layer_number_bg == 1),
    #                    organic_carbon_total[which(layer_number_bg == 1)],
    #                    NA_real_)) %>%
    #   filter(count_bg_layers_nona > 0)
    #
    # plots_gapfill_c <- profs_potential_c %>%
    #   # Group per unique_survey
    #   group_by(unique_survey, code_country, code_plot) %>%
    #   reframe(all_any_ff_na = all(any_ff_na == TRUE),
    #           any_ff_na_nona =
    #             any(any_ff_na == TRUE) & any(all_ff_nona == TRUE),
    #           count_all_ff_nona = length(which(.data$all_ff_nona == TRUE)),
    #           count_distinct_bg_up = n_distinct(organic_carbon_total_bg_up,
    #                                             na.rm = TRUE)) %>%
    #   filter(all_any_ff_na == TRUE |
    #            any_ff_na_nona == TRUE) %>%
    #   filter(count_all_ff_nona < count_distinct_bg_up)
    #
    # profs_gapfill_c <- profs_potential_c %>%
    #   filter(unique_survey %in% plots_gapfill_c$unique_survey) %>%
    #   filter(any_ff_na == TRUE)
    #
    #
    # # Nitrogen
    #
    # profs_potential_n <- df %>%
    #   # Filter out redundant layers
    #   filter(!is.na(layer_number)) %>%
    #   # Filter out forest floor layers without olw
    #   filter(layer_type != "forest_floor" |
    #            (layer_type == "forest_floor" &
    #               !is.na(organic_layer_weight))) %>%
    #   # Group per profile
    #   group_by(unique_survey_repetition,
    #            unique_survey, code_country, code_plot) %>%
    #   reframe(any_ff_na = (any(.data$layer_type == "forest_floor" &
    #                              is.na(.data$n_total))),
    #           all_ff_nona =
    #             all(!is.na(n_total[
    #               which(layer_type == "forest_floor")])),
    #           count_bg_layers =
    #             length(which(.data$layer_type != "forest_floor")),
    #           count_bg_layers_nona =
    #             length(which(.data$layer_type != "forest_floor" &
    #                            !is.na(.data$n_total))),
    #           n_total_bg_up =
    #             ifelse(any(layer_number_bg == 1),
    #                    n_total[which(layer_number_bg == 1)],
    #                    NA_real_)) %>%
    #   filter(count_bg_layers_nona > 0)
    #
    # plots_gapfill_n <- profs_potential_n %>%
    #   # Group per unique_survey
    #   group_by(unique_survey, code_country, code_plot) %>%
    #   reframe(all_any_ff_na = all(any_ff_na == TRUE),
    #           any_ff_na_nona =
    #             any(any_ff_na == TRUE) & any(all_ff_nona == TRUE),
    #           count_all_ff_nona = length(which(.data$all_ff_nona == TRUE)),
    #           count_distinct_bg_up = n_distinct(n_total_bg_up,
    #                                             na.rm = TRUE)) %>%
    #   filter(all_any_ff_na == TRUE |
    #            any_ff_na_nona == TRUE) %>%
    #   filter(count_all_ff_nona < count_distinct_bg_up)
    #
    # profs_gapfill_n <- profs_potential_n %>%
    #   filter(unique_survey %in% plots_gapfill_n$unique_survey) %>%
    #   filter(any_ff_na == TRUE)
    #



    # Combine

    # som

    if (survey_form_type == "som") {

      # Generalized gap-fill mutation function
      apply_gapfill <- function(df, parameters_chem) {
        for (param in parameters_chem) {
          # Dynamic column names
          som_col <- paste0("som_", param)
          pfh_col <- paste0("pfh_", param)
          source_col <- paste0(param, "_source")

          # Identify the specific gap-fill profiles for this parameter
          gapfill_surveys <- profs_gapfill[[param]]$profs_gapfill$unique_survey

          # Check which columns exist
          has_som <- som_col %in% names(df)
          has_pfh <- pfh_col %in% names(df)

          # Skip if neither source exists
          if (!has_som && !has_pfh) next

          # Create the mutation expressions
          if (has_som && has_pfh) {
            df <- df %>%
              mutate(
                !!source_col := ifelse(
                  is.na(!!sym(param)) &
                    layer_type == "forest_floor" &
                    unique_survey %in% gapfill_surveys,
                  case_when(
                    !is.na(.data[[som_col]]) ~
                      "som (other repetitions in same period)",
                    !is.na(.data[[pfh_col]]) ~ "pfh (same period)",
                    TRUE ~ NA_character_
                  ),
                  !!sym(source_col)
                ),
                !!param := ifelse(
                  is.na(!!sym(param)) &
                    layer_type == "forest_floor" &
                    unique_survey %in% gapfill_surveys,
                  coalesce(.data[[som_col]], .data[[pfh_col]]),
                  !!sym(param)
                )
              )
          } else if (has_som) {
            df <- df %>%
              mutate(
                !!source_col := ifelse(
                  is.na(!!sym(param)) &
                    layer_type == "forest_floor" &
                    unique_survey %in% gapfill_surveys,
                  case_when(
                    !is.na(.data[[som_col]]) ~
                      "som (other repetitions in same period)",
                    TRUE ~ NA_character_
                  ),
                  !!sym(source_col)
                ),
                !!param := ifelse(
                  is.na(!!sym(param)) &
                    layer_type == "forest_floor" &
                    unique_survey %in% gapfill_surveys,
                  .data[[som_col]],
                  !!sym(param)
                )
              )
          }
        }
        return(df)
      }


      # Apply the gap-fill mutation
      df <- apply_gapfill(df, parameters_chem)


      # df <- df %>%
      #   # Carbon
      #   mutate(
      #     organic_carbon_total_source = ifelse(
      #       is.na(organic_carbon_total) &
      #         layer_type == "forest_floor" &
      #         unique_survey %in% profs_gapfill_c$unique_survey,
      #       # Sources for gap-filling
      #       case_when(
      #         !is.na(som_organic_carbon_total) ~
      #           "som (other repetitions in same period)",
      #         !is.na(pfh_organic_carbon_total) ~ "pfh (same period)"),
      #       # Original data are kept
      #       organic_carbon_total_source),
      #     organic_carbon_total = ifelse(
      #       is.na(organic_carbon_total) &
      #         layer_type == "forest_floor" &
      #         unique_survey %in% profs_gapfill_c$unique_survey,
      #       # Gap-fill
      #       coalesce(
      #         som_organic_carbon_total,
      #         pfh_organic_carbon_total),
      #       # Else, don't change
      #       organic_carbon_total)) %>%
      #   # Nitrogen
      #   mutate(
      #     n_total_source = ifelse(
      #       is.na(n_total) &
      #         layer_type == "forest_floor" &
      #         unique_survey %in% profs_gapfill_n$unique_survey,
      #       # Sources for gap-filling
      #       case_when(
      #         !is.na(som_n_total) ~
      #           "som (other repetitions in same period)",
      #         !is.na(pfh_n_total) ~ "pfh (same period)"),
      #       # Original data are kept
      #       n_total_source),
      #     n_total = ifelse(
      #       is.na(n_total) &
      #         layer_type == "forest_floor" &
      #         unique_survey %in% profs_gapfill_n$unique_survey,
      #       # Gap-fill
      #       coalesce(
      #         som_n_total,
      #         pfh_n_total),
      #       # Else, don't change
      #       n_total))

    }


    if (survey_form_type == "pfh") {


      # Generalized gap-fill mutation function
      apply_gapfill <- function(df, parameters_chem) {
        # Only process parameters that exist in df
        existing_params <- parameters_chem[parameters_chem %in% names(df)]

        for (param in existing_params) {
          # Dynamic column names
          pfh_col <- paste0("pfh_", param)
          som_col <- paste0("som_", param)
          source_col <- paste0(param, "_source")

          # Identify the specific gap-fill profiles for this parameter
          gapfill_surveys <- profs_gapfill[[param]]$profs_gapfill$unique_survey

          # Apply mutations
          df <- df %>%
            mutate(
              !!source_col := ifelse(
                is.na(!!sym(param)) &
                  layer_type == "forest_floor" &
                  unique_survey %in% gapfill_surveys,
                case_when(
                  !is.na(.data[[pfh_col]]) ~
                    "pfh (other repetitions in same period)",
                  !is.na(.data[[som_col]]) ~ "som (same period)",
                  TRUE ~ NA_character_
                ),
                !!sym(source_col)
              ),
              !!param := ifelse(
                is.na(!!sym(param)) &
                  layer_type == "forest_floor" &
                  unique_survey %in% gapfill_surveys,
                coalesce(.data[[pfh_col]], .data[[som_col]]),
                !!sym(param)
              )
            )
        }
        return(df)
      }

      df <- apply_gapfill(df, parameters_chem)

      # df <- df %>%
      #   # carbon
      #   mutate(
      #     organic_carbon_total_source = ifelse(
      #       is.na(organic_carbon_total) &
      #         layer_type == "forest_floor" &
      #         unique_survey %in% profs_gapfill_c$unique_survey,
      #       # Sources for gap-filling
      #       case_when(
      #         !is.na(pfh_organic_carbon_total) ~
      #           "pfh (other repetitions in same period)",
      #         !is.na(som_organic_carbon_total) ~ "som (same period)"),
      #       # Original data are kept
      #       organic_carbon_total_source),
      #     organic_carbon_total = ifelse(
      #       is.na(organic_carbon_total) &
      #         layer_type == "forest_floor" &
      #         unique_survey %in% profs_gapfill_c$unique_survey,
      #       # Gap-fill
      #       coalesce(
      #         pfh_organic_carbon_total,
      #         som_organic_carbon_total),
      #       # Else, don't change
      #       organic_carbon_total)) %>%
      #   # nitrogen
      #   mutate(
      #     n_total_source = ifelse(
      #       is.na(n_total) &
      #         layer_type == "forest_floor" &
      #         unique_survey %in% profs_gapfill_n$unique_survey,
      #       # Sources for gap-filling
      #       case_when(
      #         !is.na(pfh_n_total) ~
      #           "pfh (other repetitions in same period)",
      #         !is.na(som_n_total) ~ "som (same period)"),
      #       # Original data are kept
      #       n_total_source),
      #     n_total = ifelse(
      #       is.na(n_total) &
      #         layer_type == "forest_floor" &
      #         unique_survey %in% profs_gapfill_n$unique_survey,
      #       # Gap-fill
      #       coalesce(
      #         pfh_n_total,
      #         som_n_total),
      #       # Else, don't change
      #       n_total))
    }

  } # End of "if gapfill_ff_concentrations is true"





  if (survey_form_type == "pfh") {

    # Change the variable names back to the original names

    df <- df %>%
      rename(horizon_limit_up = layer_limit_superior) %>%
      rename(horizon_limit_low = layer_limit_inferior) %>%
      rename(horizon_master = code_layer) %>%
      rename(horizon_c_organic_total = organic_carbon_total) %>%
      rename(horizon_c_organic_total_source = organic_carbon_total_source) %>%
      rename(horizon_n_total = n_total) %>%
      rename(horizon_n_total_source = n_total_source) %>%
      rename(horizon_exch_ca = exch_ca) %>%
      rename(horizon_exch_ca_source = exch_ca_source) %>%
      rename(unique_survey_profile = unique_survey_repetition)

  }




  # Save ----

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)

  } else {
    return(df)

  }

}
