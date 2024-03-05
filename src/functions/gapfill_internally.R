

gapfill_internally <- function(survey_form,
                               data_frame = NULL,
                               save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  source("./src/functions/harmonise_per_plot_layer.R")
  source("./src/functions/depth_join.R")

  cat(paste0(" \nGap-fill '", survey_form, "' internally\n"))


  code_survey <- unlist(str_split(survey_form, "_"))[1]
  survey_form_type <- unlist(str_split(survey_form, "_"))[2]



  # Parameter ranges ----

  ranges_qaqc <-
    read.csv("./data/additional_data/ranges_qaqc.csv", sep = ";")

  source("./src/functions/get_bulk_density_stats.R")
  quant_bd <- get_bulk_density_stats(mode = "quantile",
                                     matrix_type = "mineral",
                                     quantile = 99)
  quant_bd_org <- get_bulk_density_stats(mode = "quantile",
                                         matrix_type = "organic",
                                         quantile = 99)
  quant_bd <- c(294, 1920)
  quant_bd_org <- c(6, 1250)

  parameter_ranges <- bind_rows(
    data.frame(parameter = "bulk_density",
               range_min = quant_bd[1],
               range_max = quant_bd[2]),
    data.frame(parameter = "coarse_fragment_vol",
               range_min = 0,
               range_max = 100),
    data.frame(parameter = "clay",
               range_min = 0,
               range_max = 100),
    data.frame(parameter = "silt",
               range_min = 0,
               range_max = 100),
    data.frame(parameter = "sand",
               range_min = 0,
               range_max = 100))


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

      write.table(sw_plot,
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

  # Add data of "som"

  df <- depth_join(df1 = df,
                   df2 = som_plot,
                   prefix_parameters_in_df1 = "som_")

  # Add data of "pfh"

  df <- depth_join(df1 = df,
                   df2 = pfh_plot,
                   prefix_parameters_in_df1 = "pfh_")

  # Add data of "swc"

  if (code_survey == "so") {

  df <- depth_join(df1 = df,
                   df2 = swc_plot,
                   prefix_parameters_in_df1 = "swc_")
  }

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

    range_mineral_min <- parameter_ranges$range_min[
      which(parameter_ranges$parameter == "bulk_density")]
    range_mineral_max <- parameter_ranges$range_max[
      which(parameter_ranges$parameter == "bulk_density")]

    bulk_density_columns <- df %>%
      select(contains("bulk_dens"),
             -contains("source"), -contains("survey_year")) %>%
      names

    df <- df %>%
      # Remove values outside of the bulk density range
      mutate(bulk_density = ifelse(
        (!is.na(bulk_density)) &
        ((.data$layer_type != "mineral" &
           .data$bulk_density > quant_bd_org[1] &
           .data$bulk_density < quant_bd_org[2]) |
          (.data$layer_type == "mineral" &
             .data$bulk_density >= range_mineral_min &
             .data$bulk_density <= range_mineral_max)),
        bulk_density,
        NA),
        bulk_density_source = ifelse(
          (!is.na(bulk_density_source)) &
            ((.data$layer_type != "mineral" &
                .data$bulk_density > quant_bd_org[1] &
                .data$bulk_density < quant_bd_org[2]) |
               (.data$layer_type == "mineral" &
                  .data$bulk_density >= range_mineral_min &
                  .data$bulk_density <= range_mineral_max)),
          bulk_density_source,
          NA),
        bulk_density_layer_weight = ifelse(
          (!is.na(bulk_density_layer_weight)) &
            ((.data$layer_type != "mineral" &
                .data$bulk_density_layer_weight > quant_bd_org[1] &
                .data$bulk_density_layer_weight < quant_bd_org[2]) |
               (.data$layer_type == "mineral" &
                  .data$bulk_density_layer_weight >= range_mineral_min &
                  .data$bulk_density_layer_weight <= range_mineral_max)),
          bulk_density_layer_weight,
          NA)) %>%
      # Use bulk_density_layer_weight to gap-fill layers from the same year
      mutate(
        bulk_density_source = ifelse(
          (is.na(bulk_density) &
             !is.na(bulk_density_layer_weight) &
             (layer_type %in% c("forest_floor", "peat"))),
          paste0("som (organic_layer_weight", .data$survey_year, ")"),
          bulk_density_source),
        bulk_density = ifelse(
          (is.na(bulk_density) &
             !is.na(bulk_density_layer_weight) &
             (layer_type %in% c("forest_floor", "peat"))),
          bulk_density_layer_weight,
          bulk_density)) %>%
      # Gap-fill
      mutate(
        bulk_density_source = ifelse(
          !is.na(bulk_density) &
            ((layer_type %in% c("forest_floor", "peat")) |
               (layer_type == "mineral" &
                  !is.na(som_bulk_density_survey_year) &
                  (survey_year == som_bulk_density_survey_year))),
          # Original data are kept
          bulk_density_source,
          # Sources for gap-filling
          case_when(
            !is.na(som_bulk_density) ~
              paste0("som_", .data$som_bulk_density_survey_year),
            exists("swc_bulk_density") && !is.na(swc_bulk_density) ~
              paste0("swc_", .data$swc_bulk_density_survey_year),
            !is.na(pfh_horizon_bulk_dens_measure) ~
              paste0("pfh_measure_",
                     .data$pfh_horizon_bulk_dens_measure_survey_year),
            !is.na(pfh_horizon_bulk_dens_est) ~
              paste0("pfh_est_", .data$pfh_horizon_bulk_dens_est_survey_year))),
        bulk_density = ifelse(
          !is.na(bulk_density) &
            ((layer_type %in% c("forest_floor", "peat")) |
            (layer_type == "mineral" &
               !is.na(som_bulk_density_survey_year) &
               (survey_year == som_bulk_density_survey_year))),
          # Keep the original bulk density data
          bulk_density,
          # Else, gap-fill
          coalesce(
            som_bulk_density,
            ifelse(exists("swc_bulk_density"), swc_bulk_density, NULL),
            pfh_horizon_bulk_dens_measure,
            pfh_horizon_bulk_dens_est))) %>%
      # Confidence intervals
      mutate(bulk_density_sd = case_when(
        !is.na(bulk_density) ~
               sd(c_across(all_of(bulk_density_columns)),
                                  na.rm = TRUE)),
             bulk_density_n = case_when(
               !is.na(bulk_density) ~
               sum(!is.na(c_across(all_of(bulk_density_columns))))),
             bulk_density_se = case_when(
               !is.na(bulk_density) ~
          bulk_density_sd / sqrt(bulk_density_n)),
             bulk_density_t = case_when(
               !is.na(bulk_density) ~
          qt(0.975, df = (.data$bulk_density_n - 1))),
             bulk_density_avg = case_when(
               !is.na(bulk_density) ~
          mean(c_across(all_of(bulk_density_columns)),
                                     na.rm = TRUE)),
             bulk_density_min_ci = case_when(
               !is.na(bulk_density) ~
               round(bulk_density_avg - bulk_density_t * bulk_density_se)),
             bulk_density_max_ci = case_when(
               !is.na(bulk_density) ~
               round(bulk_density_avg + bulk_density_t * bulk_density_se))) %>%
      # Round
      mutate(bulk_density = round(bulk_density)) %>%
      select(-any_of(c(
        "bulk_density_afscdb",
        "bulk_density_layer_weight",
        "som_bulk_density", "som_bulk_density_survey_year",
        "som_bulk_density_min", "som_bulk_density_max",
        "pfh_horizon_bulk_dens_measure",
        "pfh_horizon_bulk_dens_measure_survey_year",
        "pfh_horizon_bulk_dens_measure_min",
        "pfh_horizon_bulk_dens_measure_max",
        "pfh_horizon_bulk_dens_est", "pfh_horizon_bulk_dens_est_survey_year",
        "pfh_horizon_bulk_dens_est_min", "pfh_horizon_bulk_dens_est_max",
        "swc_bulk_density", "swc_bulk_density_survey_year",
        "swc_bulk_density_min", "swc_bulk_density_max",
        "bulk_density_sd", "bulk_density_n",
        "bulk_density_se", "bulk_density_t",
        "bulk_density_avg"))) %>%
      relocate(any_of(c("bulk_density_min_ci", "bulk_density_max_ci")),
               .after = "bulk_density")



    ### 3.1.2. Coarse fragments ----

    coarse_fragment_columns <- df %>%
      select(contains("coarse_frag"),
             -contains("source"), -contains("survey_year")) %>%
      names

    df <- df %>%
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
      # Confidence intervals
      mutate(coarse_fragment_vol_sd = case_when(
        !is.na(coarse_fragment_vol) ~
          sd(c_across(all_of(coarse_fragment_columns)), na.rm = TRUE)),
             coarse_fragment_vol_n = case_when(
               !is.na(coarse_fragment_vol) ~
               sum(!is.na(c_across(all_of(coarse_fragment_columns))))),
             coarse_fragment_vol_se =  case_when(
               !is.na(coarse_fragment_vol) ~
               coarse_fragment_vol_sd / sqrt(coarse_fragment_vol_n)),
             coarse_fragment_vol_t = case_when(
               !is.na(coarse_fragment_vol) ~
               qt(0.975, df = (.data$coarse_fragment_vol_n - 1))),
             coarse_fragment_vol_avg = case_when(
               !is.na(coarse_fragment_vol) ~
               mean(c_across(all_of(coarse_fragment_columns)), na.rm = TRUE)),
             coarse_fragment_vol_min_ci = case_when(
               !is.na(coarse_fragment_vol) ~
               round(coarse_fragment_vol_avg -
                       coarse_fragment_vol_t * coarse_fragment_vol_se, 1)),
             coarse_fragment_vol_max_ci = case_when(
               !is.na(coarse_fragment_vol) ~
               round(coarse_fragment_vol_avg +
                       coarse_fragment_vol_t * coarse_fragment_vol_se, 1))) %>%
      # Round
      mutate(coarse_fragment_vol = round(coarse_fragment_vol, 1)) %>%
      select(-c(
        "coarse_fragment_vol_afscdb",
        "som_coarse_fragment_vol", "som_coarse_fragment_vol_survey_year",
        "som_coarse_fragment_vol_min", "som_coarse_fragment_vol_max",
        "pfh_coarse_fragment_vol_converted",
        "pfh_coarse_fragment_vol_converted_survey_year",
        "pfh_coarse_fragment_vol_converted_min",
        "pfh_coarse_fragment_vol_converted_max",
        "pfh_coarse_fragment_vol_avg",
        "pfh_coarse_fragment_vol_avg_survey_year",
        "pfh_coarse_fragment_vol_avg_min", "pfh_coarse_fragment_vol_avg_max",
        "coarse_fragment_vol_sd", "coarse_fragment_vol_n",
        "coarse_fragment_vol_se", "coarse_fragment_vol_t",
        "coarse_fragment_vol_avg")) %>%
      relocate(any_of(c("coarse_fragment_vol_min_ci",
                        "coarse_fragment_vol_max_ci")),
               .after = "coarse_fragment_vol")



    ### 3.1.3. Clay ----

    clay_columns <- df %>%
      select(contains("clay"),
             -contains("source"), -contains("survey_year"),
             -contains("_rt"), -contains("_loq")) %>%
      names

    df <- df %>%
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
      # Confidence intervals
      mutate(part_size_clay_sd = case_when(
        !is.na(part_size_clay) ~
          sd(c_across(all_of(clay_columns)), na.rm = TRUE)),
        part_size_clay_n = case_when(
          !is.na(part_size_clay) ~
            sum(!is.na(c_across(all_of(clay_columns))))),
        part_size_clay_se =  case_when(
          !is.na(part_size_clay) ~
            part_size_clay_sd / sqrt(part_size_clay_n)),
        part_size_clay_t = case_when(
          !is.na(part_size_clay) ~
            qt(0.975, df = (.data$part_size_clay_n - 1))),
        part_size_clay_avg = case_when(
          !is.na(part_size_clay) ~
            mean(c_across(all_of(clay_columns)), na.rm = TRUE)),
        part_size_clay_min_ci = case_when(
          !is.na(part_size_clay) ~
            round(part_size_clay_avg -
                    part_size_clay_t * part_size_clay_se, 1)),
        part_size_clay_max_ci = case_when(
          !is.na(part_size_clay) ~
            round(part_size_clay_avg +
                    part_size_clay_t * part_size_clay_se, 1))) %>%
      # Round
      mutate(part_size_clay = round(part_size_clay, 1)) %>%
      select(-c(
        "part_size_clay_afscdb",
        "som_part_size_clay",
        "som_part_size_clay_min", "som_part_size_clay_max",
        "pfh_horizon_clay",
        "pfh_horizon_clay_min",
        "pfh_horizon_clay_max",
        "part_size_clay_sd", "part_size_clay_n",
        "part_size_clay_se", "part_size_clay_t",
        "part_size_clay_avg")) %>%
      relocate(any_of(c("part_size_clay_min_ci",
                        "part_size_clay_max_ci")),
               .after = "part_size_clay")



    ### 3.1.4. Silt ----

    silt_columns <- df %>%
      select(contains("silt"),
             -contains("source"), -contains("survey_year"),
             -contains("_rt"), -contains("_loq")) %>%
      names

    df <- df %>%
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
      # Confidence intervals
      mutate(part_size_silt_sd = case_when(
        !is.na(part_size_silt) ~
          sd(c_across(all_of(silt_columns)), na.rm = TRUE)),
        part_size_silt_n = case_when(
          !is.na(part_size_silt) ~
            sum(!is.na(c_across(all_of(silt_columns))))),
        part_size_silt_se =  case_when(
          !is.na(part_size_silt) ~
            part_size_silt_sd / sqrt(part_size_silt_n)),
        part_size_silt_t = case_when(
          !is.na(part_size_silt) ~
            qt(0.975, df = (.data$part_size_silt_n - 1))),
        part_size_silt_avg = case_when(
          !is.na(part_size_silt) ~
            mean(c_across(all_of(silt_columns)), na.rm = TRUE)),
        part_size_silt_min_ci = case_when(
          !is.na(part_size_silt) ~
            round(part_size_silt_avg -
                    part_size_silt_t * part_size_silt_se, 1)),
        part_size_silt_max_ci = case_when(
          !is.na(part_size_silt) ~
            round(part_size_silt_avg +
                    part_size_silt_t * part_size_silt_se, 1))) %>%
      # Round
      mutate(part_size_silt = round(part_size_silt, 1)) %>%
      select(-c(
        "part_size_silt_afscdb",
        "som_part_size_silt",
        "som_part_size_silt_min", "som_part_size_silt_max",
        "pfh_horizon_silt",
        "pfh_horizon_silt_min",
        "pfh_horizon_silt_max",
        "part_size_silt_sd", "part_size_silt_n",
        "part_size_silt_se", "part_size_silt_t",
        "part_size_silt_avg")) %>%
      relocate(any_of(c("part_size_silt_min_ci",
                        "part_size_silt_max_ci")),
               .after = "part_size_silt")


    ### 3.1.5. Sand ----

    sand_columns <- df %>%
      select(contains("sand"),
             -contains("source"), -contains("survey_year"),
             -contains("_rt"), -contains("_loq")) %>%
      names

    df <- df %>%
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
      # Confidence intervals
      mutate(part_size_sand_sd = case_when(
        !is.na(part_size_sand) ~
          sd(c_across(all_of(sand_columns)), na.rm = TRUE)),
        part_size_sand_n = case_when(
          !is.na(part_size_sand) ~
            sum(!is.na(c_across(all_of(sand_columns))))),
        part_size_sand_se =  case_when(
          !is.na(part_size_sand) ~
            part_size_sand_sd / sqrt(part_size_sand_n)),
        part_size_sand_t = case_when(
          !is.na(part_size_sand) ~
            qt(0.975, df = (.data$part_size_sand_n - 1))),
        part_size_sand_avg = case_when(
          !is.na(part_size_sand) ~
            mean(c_across(all_of(sand_columns)), na.rm = TRUE)),
        part_size_sand_min_ci = case_when(
          !is.na(part_size_sand) ~
            round(part_size_sand_avg -
                    part_size_sand_t * part_size_sand_se, 1)),
        part_size_sand_max_ci = case_when(
          !is.na(part_size_sand) ~
            round(part_size_sand_avg +
                    part_size_sand_t * part_size_sand_se, 1))) %>%
      # Round
      mutate(part_size_sand = round(part_size_sand, 1)) %>%
      select(-c(
        "part_size_sand_afscdb",
        "som_part_size_sand", "som_texture_survey_year",
        "som_part_size_sand_min", "som_part_size_sand_max",
        "pfh_horizon_sand", "pfh_texture_survey_year",
        "pfh_horizon_sand_min",
        "pfh_horizon_sand_max",
        "part_size_sand_sd", "part_size_sand_n",
        "part_size_sand_se", "part_size_sand_t",
        "part_size_sand_avg")) %>%
      relocate(any_of(c("part_size_sand_min_ci",
                        "part_size_sand_max_ci")),
               .after = "part_size_sand")



    ### 3.1.6. Organic layer weight ----

    df <- df %>%
      # Gap-fill
      mutate(
        organic_layer_weight_bd = ifelse(
            !is.na(bulk_density) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density * (.data$layer_thickness * 1e-2), 3),
          NA_real_),
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
      # Confidence intervals
      mutate(
        organic_layer_weight_bd_min = ifelse(
          !is.na(bulk_density) &
            !is.na(bulk_density_min_ci) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density_min_ci * (.data$layer_thickness * 1e-2), 3),
          NA_real_),
        organic_layer_weight_bd_max = ifelse(
          !is.na(bulk_density) &
            !is.na(bulk_density_max_ci) &
            !is.na(layer_thickness) &
            layer_type %in% c("peat", "forest_floor"),
          # kg m-2
          round(.data$bulk_density_max_ci * (.data$layer_thickness * 1e-2), 3),
          NA_real_),
        organic_layer_weight_min_ci = case_when(
          !is.na(organic_layer_weight) ~
            round(min(c(.data$organic_layer_weight,
                        .data$organic_layer_weight_bd_min)), 1)),
        organic_layer_weight_max_ci = case_when(
          !is.na(organic_layer_weight) ~
            round(max(c(.data$organic_layer_weight,
                        .data$organic_layer_weight_bd_max)), 1))) %>%
      # Round
      mutate(part_size_sand = round(part_size_sand, 1)) %>%
      select(-c(
        "organic_layer_weight_afscdb", "organic_layer_weight_bd",
        "organic_layer_weight_bd_min", "organic_layer_weight_bd_max") %>%
      relocate(any_of(c("organic_layer_weight_min_ci",
                        "organic_layer_weight_max_ci")),
               .after = "organic_layer_weight")






  # OLW niet vergeten

  }





  ## 3.2. "pfh" ----

  if (survey_form_type == "pfh") {

    ### 3.2.1. Bulk density ----

    range_mineral_min <- parameter_ranges$range_min[
      which(parameter_ranges$parameter == "bulk_density")]
    range_mineral_max <- parameter_ranges$range_max[
      which(parameter_ranges$parameter == "bulk_density")]

    bulk_density_columns <- df %>%
      select(contains("bulk_dens"),
             -contains("source"), -contains("survey_year")) %>%
      names

    df <- df %>%
      # Copy horizon_bulk_dens_measure to a new column bulk_density
      mutate(bulk_density = horizon_bulk_dens_measure) %>%
      # Rename horizon_bulk_dens_measure_source
      rename(bulk_density_source = horizon_bulk_dens_measure_source) %>%
      # Remove values outside of the bulk density range
      mutate(bulk_density = ifelse(
        (!is.na(bulk_density)) &
          ((.data$layer_type != "mineral" &
              .data$bulk_density > quant_bd_org[1] &
              .data$bulk_density < quant_bd_org[2]) |
             (.data$layer_type == "mineral" &
                .data$bulk_density >= range_mineral_min &
                .data$bulk_density <= range_mineral_max)),
        bulk_density,
        NA),
        bulk_density_source = ifelse(
          (!is.na(bulk_density_source)) &
            ((.data$layer_type != "mineral" &
                .data$bulk_density > quant_bd_org[1] &
                .data$bulk_density < quant_bd_org[2]) |
               (.data$layer_type == "mineral" &
                  .data$bulk_density >= range_mineral_min &
                  .data$bulk_density <= range_mineral_max)),
          bulk_density_source,
          NA)) %>%
      # Gap-fill
      mutate(
        bulk_density_source = ifelse(
          !is.na(bulk_density) &
            ((layer_type %in% c("forest_floor", "peat")) |
               (layer_type == "mineral" &
                  !is.na(pfh_bulk_dens_measure_survey_year) &
                  (survey_year == pfh_bulk_dens_measure_survey_year))),
          # Original data are kept
          bulk_density_source,
          # Sources for gap-filling
          case_when(
            !is.na(pfh_horizon_bulk_dens_measure) ~
              paste0("pfh_measure_",
                     .data$pfh_horizon_bulk_dens_measure_survey_year),
            exists("swc_bulk_density") && !is.na(swc_bulk_density) ~
              paste0("swc_", .data$swc_bulk_density_survey_year),
            !is.na(som_bulk_density) ~
              paste0("som_",
                     .data$som_bulk_density_survey_year),
            !is.na(pfh_horizon_bulk_dens_est) ~
              paste0("pfh_est_", .data$pfh_horizon_bulk_dens_est_survey_year))),
        bulk_density = ifelse(
          !is.na(bulk_density) &
            ((layer_type %in% c("forest_floor", "peat")) |
               (layer_type == "mineral" &
                  !is.na(pfh_bulk_dens_measure_survey_year) &
                  (survey_year == pfh_bulk_dens_measure_survey_year))),
          # Keep the original bulk density data
          bulk_density,
          # Else, gap-fill
          coalesce(
            pfh_horizon_bulk_dens_measure,
            ifelse(exists("swc_bulk_density"), swc_bulk_density, NULL),
            som_bulk_density,
            pfh_horizon_bulk_dens_est))) %>%
      # Confidence intervals
      mutate(bulk_density_sd = case_when(
        !is.na(bulk_density) ~
          sd(c_across(all_of(bulk_density_columns)),
             na.rm = TRUE)),
        bulk_density_n = case_when(
          !is.na(bulk_density) ~
            sum(!is.na(c_across(all_of(bulk_density_columns))))),
        bulk_density_se = case_when(
          !is.na(bulk_density) ~
            bulk_density_sd / sqrt(bulk_density_n)),
        bulk_density_t = case_when(
          !is.na(bulk_density) ~
            qt(0.975, df = (.data$bulk_density_n - 1))),
        bulk_density_avg = case_when(
          !is.na(bulk_density) ~
            mean(c_across(all_of(bulk_density_columns)),
                 na.rm = TRUE)),
        bulk_density_min_ci = case_when(
          !is.na(bulk_density) ~
            round(bulk_density_avg - bulk_density_t * bulk_density_se)),
        bulk_density_max_ci = case_when(
          !is.na(bulk_density) ~
            round(bulk_density_avg + bulk_density_t * bulk_density_se))) %>%
      # Round
      mutate(bulk_density = round(bulk_density)) %>%
      select(-any_of(c(
        "bulk_density_afscdb",
        "bulk_density_layer_weight",
        "som_bulk_density", "som_bulk_density_survey_year",
        "som_bulk_density_min", "som_bulk_density_max",
        "pfh_horizon_bulk_dens_measure",
        "pfh_horizon_bulk_dens_measure_survey_year",
        "pfh_horizon_bulk_dens_measure_min",
        "pfh_horizon_bulk_dens_measure_max",
        "pfh_horizon_bulk_dens_est", "pfh_horizon_bulk_dens_est_survey_year",
        "pfh_horizon_bulk_dens_est_min", "pfh_horizon_bulk_dens_est_max",
        "swc_bulk_density", "swc_bulk_density_survey_year",
        "swc_bulk_density_min", "swc_bulk_density_max",
        "bulk_density_sd", "bulk_density_n",
        "bulk_density_se", "bulk_density_t",
        "bulk_density_avg"))) %>%
      relocate(any_of(c("bulk_density_min_ci", "bulk_density_max_ci")),
               .after = "bulk_density")



    ### 3.2.2. Coarse fragments ----

    coarse_fragment_columns <- df %>%
      select(contains("coarse_frag"),
             -contains("source"), -contains("survey_year")) %>%
      names

    df <- df %>%
      # Copy coarse_fragment_vol_converted to a new column coarse_fragment_vol
      mutate(coarse_fragment_vol = coarse_fragment_vol_converted) %>%
      # Rename horizon_coarse_weight_source
      rename(coarse_fragment_vol_source = horizon_coarse_weight_source) %>%
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
      # Confidence intervals
      mutate(coarse_fragment_vol_sd = case_when(
        !is.na(coarse_fragment_vol) ~
          sd(c_across(all_of(coarse_fragment_columns)), na.rm = TRUE)),
        coarse_fragment_vol_n = case_when(
          !is.na(coarse_fragment_vol) ~
            sum(!is.na(c_across(all_of(coarse_fragment_columns))))),
        coarse_fragment_vol_se =  case_when(
          !is.na(coarse_fragment_vol) ~
            coarse_fragment_vol_sd / sqrt(coarse_fragment_vol_n)),
        coarse_fragment_vol_t = case_when(
          !is.na(coarse_fragment_vol) ~
            qt(0.975, df = (.data$coarse_fragment_vol_n - 1))),
        coarse_fragment_vol_avg = case_when(
          !is.na(coarse_fragment_vol) ~
            mean(c_across(all_of(coarse_fragment_columns)), na.rm = TRUE)),
        coarse_fragment_vol_min_ci = case_when(
          !is.na(coarse_fragment_vol) ~
            round(coarse_fragment_vol_avg -
                    coarse_fragment_vol_t * coarse_fragment_vol_se, 1)),
        coarse_fragment_vol_max_ci = case_when(
          !is.na(coarse_fragment_vol) ~
            round(coarse_fragment_vol_avg +
                    coarse_fragment_vol_t * coarse_fragment_vol_se, 1))) %>%
      # Round
      mutate(coarse_fragment_vol = round(coarse_fragment_vol, 1)) %>%
      select(-c(
        "coarse_fragment_vol_afscdb",
        "som_coarse_fragment_vol", "som_coarse_fragment_vol_survey_year",
        "som_coarse_fragment_vol_min", "som_coarse_fragment_vol_max",
        "pfh_coarse_fragment_vol_converted",
        "pfh_coarse_fragment_vol_converted_survey_year",
        "pfh_coarse_fragment_vol_converted_min",
        "pfh_coarse_fragment_vol_converted_max",
        "pfh_coarse_fragment_vol_avg",
        "pfh_coarse_fragment_vol_avg_survey_year",
        "pfh_coarse_fragment_vol_avg_min", "pfh_coarse_fragment_vol_avg_max",
        "coarse_fragment_vol_sd", "coarse_fragment_vol_n",
        "coarse_fragment_vol_se", "coarse_fragment_vol_t",
        "coarse_fragment_vol_avg")) %>%
      relocate(any_of(c("coarse_fragment_vol_min_ci",
                        "coarse_fragment_vol_max_ci")),
               .after = "coarse_fragment_vol")












  }





  # Change names of pfh columns back to the original ones


# . ----
# . ----


  # Redundant layers do need to be removed so this column is needed

  assertthat::assert_that("layer_number" %in% names(pfh))

  # This takes some time, so check whether this form already exists
  # in the global environment

  name <- paste0(unlist(strsplit(survey_form, "_"))[1], "_pfh_plot_layer")

  if (!exists(name) ||
      (exists(name) &&
       any(!"data.frame" %in% class(get_env(name))))) {

    pfh_plot <-
      harmonise_per_plot_layer(
        survey_form_input =
          paste0(unlist(strsplit(survey_form, "_"))[1], "_pfh"),
        data_frame_input =
          get_env(paste0(unlist(strsplit(survey_form, "_"))[1], "_pfh")))

    assign_env(name,
               pfh_plot)

    write.table(sw_plot,
                file = paste0("./output/gap_filling_details/",
                              name,
                              ".csv"),
                row.names = FALSE,
                na = "",
                sep = ";",
                dec = ".")

  } else {

    pfh_fixed <- get_env(name)

  }





  # The function harmonise_into_fixed_depth_layers automatically makes a
  # column "bulk_density" which contains values for "horizon_bulk_dens_measure"
  # if this exists, else "horizon_bulk_dens_est".

  # Convert coarse fragments to the right units (volume %)

  d_soil_coarse_fragments <-
    read.csv2("./data/additional_data/d_soil_coarse_fragments.csv") %>%
    select(code, coarse_fragment_vol_avg)

  pfh_fixed <- pfh_fixed %>%
    # Convert volumetric coarse fragment codes to actual average vol %
    left_join(d_soil_coarse_fragments,
              by = join_by(code_horizon_coarse_vol == code)) %>%
    # Convert weight percentages to volumetric percentages:
    # Imagine: 1 m³ of fine earth contains
    # e.g. 1300 kg fine earth (bulk density).
    # Then, imagine the weight percentage of coarse fragments
    # from that soil is 11 %.
    # That means that there is 1300 kg * 11/89 = 160.7 kg of coarse fragments
    # for 1 m³ of fine earth in this soil.
    # Imagine the coarse fragments have a particle density of 2650 kg per m³.
    # Then, we can calculate that this 160.7 kg of coarse fragments occupies
    # 160.7/2650 = 0.061 m³.
    # As such, the vol % of coarse fragments will be 0.061 / (1 + 0.061)
  mutate(coarse_fragment_aid =
           ifelse(!is.na(bulk_density) & !is.na(horizon_coarse_weight),
                  (.data$bulk_density *
                     (.data$horizon_coarse_weight /
                        (100 - .data$horizon_coarse_weight))) / 2650,
                  NA)) %>%
    mutate(coarse_fragment_vol_converted =
             ifelse(!is.na(.data$coarse_fragment_aid),
                    as.numeric((.data$coarse_fragment_aid /
                                  (1 + .data$coarse_fragment_aid)) * 100),
                    NA)) %>%
    select(-coarse_fragment_aid) %>%
    mutate(coarse_fragment_vol = case_when(
      # Priority 1: converted weight %
      !is.na(.data$coarse_fragment_vol_converted) ~ coarse_fragment_vol_converted,
      # Priority 2: volumetric classes
      !is.na(.data$coarse_fragment_vol_avg) ~ coarse_fragment_vol_avg,
      TRUE ~ NA_real_))



  # Aggregate per plot survey (per survey layer) across different profiles

  pfh_fixed_depths_agg_prof <- pfh_fixed %>%
    mutate(unique_survey_layer = paste0(code_country, "_",
                                        survey_year, "_",
                                        code_plot, "_",
                                        code_layer)) %>%
    mutate(unique_layer = paste0(code_country, "_",
                                 code_plot, "_",
                                 code_layer)) %>%
    group_by(unique_survey_layer, unique_layer,
             code_country, survey_year, code_plot, code_layer) %>%
    summarise(layer_limit_superior =
                mean(layer_limit_superior, na.rm = TRUE),
              layer_limit_inferior =
                mean(layer_limit_inferior, na.rm = TRUE),
              horizon_c_organic_total =
                mean(horizon_c_organic_total, na.rm = TRUE),
              horizon_clay =
                mean(horizon_clay, na.rm = TRUE),
              horizon_silt =
                mean(horizon_silt, na.rm = TRUE),
              horizon_sand =
                mean(horizon_sand, na.rm = TRUE),
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))

  # Aggregate per unique layer (plot_id x code_layer)
  # across different survey years
  # To gap-fill data not from the same survey_year

  pfh_fixed_otheryear <- pfh_fixed %>%
    mutate(unique_layer = paste0(code_country, "_",
                                 code_plot, "_",
                                 code_layer)) %>%
    group_by(unique_layer,
             code_country, code_plot, code_layer) %>%
    summarise(layer_limit_superior =
                mean(layer_limit_superior, na.rm = TRUE),
              layer_limit_inferior =
                mean(layer_limit_inferior, na.rm = TRUE),
              horizon_clay =
                mean(horizon_clay, na.rm = TRUE),
              horizon_silt =
                mean(horizon_silt, na.rm = TRUE),
              horizon_sand =
                mean(horizon_sand, na.rm = TRUE),
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(layer_limit_superior = ifelse(is.nan(layer_limit_superior),
                                         NA, layer_limit_superior),
           layer_limit_inferior = ifelse(is.nan(layer_limit_inferior),
                                         NA, layer_limit_inferior),
           horizon_clay = ifelse(is.nan(horizon_clay),
                                 NA, horizon_clay),
           horizon_silt = ifelse(is.nan(horizon_silt),
                                 NA, horizon_silt),
           horizon_sand = ifelse(is.nan(horizon_sand),
                                 NA, horizon_sand),
           bulk_density = ifelse(is.nan(bulk_density),
                                 NA, bulk_density),
           coarse_fragment_vol = ifelse(is.nan(coarse_fragment_vol),
                                        NA, coarse_fragment_vol))

  # horizon_c_organic_total should not be assessed based on values from
  # other survey years









  # 3. Source 3: "som" (other survey years) ----

  som_otheryear <- df %>%
    group_by(unique_layer,
             code_country, code_plot, code_layer) %>%
    summarise(layer_limit_superior =
                mean(layer_limit_superior, na.rm = TRUE),
              layer_limit_inferior =
                mean(layer_limit_inferior, na.rm = TRUE),
              part_size_clay =
                mean(part_size_clay, na.rm = TRUE),
              part_size_silt =
                mean(part_size_silt, na.rm = TRUE),
              part_size_sand =
                mean(part_size_sand, na.rm = TRUE),
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))

  # organic_layer_weight and organic_carbon_total should not be assessed
  # based on other survey years








  # 4. Compile ----

  ## 4.1. Add data to df ----

  if (unlist(strsplit(survey_form, "_"))[1] == "so") {
      # Level II

  df <- df %>%
    # sw_swc same year
    left_join(swc_fixed %>%
                select(-unique_layer) %>%
                rename(bulk_density_sw_swc_sameyear =
                         bulk_density),
              by = "unique_survey_layer")
  }

  df <- df %>%
    # pfh same year
    left_join(pfh_fixed_depths_agg_prof %>%
                rename(organic_carbon_total_pfh_sameyear =
                         horizon_c_organic_total,
                       bulk_density_pfh_sameyear =
                         bulk_density,
                       coarse_fragment_vol_pfh_sameyear =
                         coarse_fragment_vol,
                       part_size_clay_pfh_sameyear =
                         horizon_clay,
                       part_size_silt_pfh_sameyear =
                         horizon_silt,
                       part_size_sand_pfh_sameyear =
                         horizon_sand) %>%
                select(unique_survey_layer,
                       organic_carbon_total_pfh_sameyear,
                       bulk_density_pfh_sameyear,
                       coarse_fragment_vol_pfh_sameyear,
                       part_size_clay_pfh_sameyear,
                       part_size_silt_pfh_sameyear,
                       part_size_sand_pfh_sameyear),
              by = "unique_survey_layer") %>%
    # som other year
    left_join(som_otheryear %>%
                rename(bulk_density_som_otheryear =
                         bulk_density,
                       coarse_fragment_vol_som_otheryear =
                         coarse_fragment_vol,
                       part_size_clay_som_otheryear =
                         part_size_clay,
                       part_size_silt_som_otheryear =
                         part_size_silt,
                       part_size_sand_som_otheryear =
                         part_size_sand) %>%
                select(unique_layer,
                       bulk_density_som_otheryear,
                       coarse_fragment_vol_som_otheryear,
                       part_size_clay_som_otheryear,
                       part_size_silt_som_otheryear,
                       part_size_sand_som_otheryear),
              by = "unique_layer")

  if (unlist(strsplit(survey_form, "_"))[1] == "so") {
      # Level II

  df <- df %>%
    # sw_swc other year
    left_join(swc_fixed_otheryear %>%
                rename(bulk_density_sw_swc_otheryear =
                         bulk_density),
              by = "unique_layer")
  }

  df <- df %>%
    # pfh other year
    left_join(pfh_fixed_otheryear %>%
                rename(bulk_density_pfh_otheryear =
                         bulk_density,
                       coarse_fragment_vol_pfh_otheryear =
                         coarse_fragment_vol,
                       part_size_clay_pfh_otheryear =
                         horizon_clay,
                       part_size_silt_pfh_otheryear =
                         horizon_silt,
                       part_size_sand_pfh_otheryear =
                         horizon_sand) %>%
                select(unique_layer,
                       bulk_density_pfh_otheryear,
                       coarse_fragment_vol_pfh_otheryear,
                       part_size_clay_pfh_otheryear,
                       part_size_silt_pfh_otheryear,
                       part_size_sand_pfh_otheryear),
              by = "unique_layer")


  ## 4.2. Bulk density: combine columns ----

  if (!"bulk_density_orig" %in% names(df)) {
    df$bulk_density_orig <- df$bulk_density
  }

  if (unlist(strsplit(survey_form, "_"))[1] == "so") {
    # Level II

  df <- df %>%
    mutate(
      bulk_density_source = case_when(
        !is.na(.data$bulk_density) ~ "som (same year)",
        !is.na(.data$bulk_density_sw_swc_sameyear) ~ "sw_swc (same year)",
        !is.na(.data$bulk_density_pfh_sameyear) ~ "pfh (same year)",
        !is.na(.data$bulk_density_som_otheryear) ~ "som (other year)",
        !is.na(.data$bulk_density_sw_swc_otheryear) ~ "sw_swc (other year)",
        !is.na(.data$bulk_density_pfh_otheryear) ~ "pfh (other year)",
        !is.na(.data$bulk_density_layer_weight) ~ "som (layer_weight)",
        TRUE ~ NA_character_),
      bulk_density = coalesce(
        .data$bulk_density,
        .data$bulk_density_sw_swc_sameyear,
        .data$bulk_density_pfh_sameyear,
        .data$bulk_density_som_otheryear,
        .data$bulk_density_sw_swc_otheryear,
        .data$bulk_density_pfh_otheryear,
        .data$bulk_density_layer_weight)) %>%
    select(-bulk_density_sw_swc_sameyear,
           -bulk_density_pfh_sameyear,
           -bulk_density_som_otheryear,
           -bulk_density_sw_swc_otheryear,
           -bulk_density_pfh_otheryear,
           -bulk_density_layer_weight)

  } else {

    # Level I

    df <- df %>%
      mutate(
        bulk_density_source = case_when(
          !is.na(.data$bulk_density) ~ "som (same year)",
          !is.na(.data$bulk_density_pfh_sameyear) ~ "pfh (same year)",
          !is.na(.data$bulk_density_som_otheryear) ~ "som (other year)",
          !is.na(.data$bulk_density_pfh_otheryear) ~ "pfh (other year)",
          TRUE ~ NA_character_),
        bulk_density = coalesce(
          .data$bulk_density,
          .data$bulk_density_pfh_sameyear,
          .data$bulk_density_som_otheryear,
          .data$bulk_density_pfh_otheryear)) %>%
      select(-bulk_density_pfh_sameyear,
             -bulk_density_som_otheryear,
             -bulk_density_pfh_otheryear)

  }

  cat("Data sources 'bulk_density' after internal gap-filling:\n")
  print(table(df$bulk_density_source))


  ## 4.3. Coarse fragments: combine columns ----

  if (!"coarse_fragment_vol_orig" %in% names(df)) {
    df$coarse_fragment_vol_orig <- df$coarse_fragment_vol
  }

  df <- df %>%
    mutate(
      coarse_fragment_source = case_when(
        !is.na(.data$coarse_fragment_vol) ~ "som (same year)",
        !is.na(.data$coarse_fragment_vol_pfh_sameyear) ~ "pfh (same year)",
        !is.na(.data$coarse_fragment_vol_som_otheryear) ~ "som (other year)",
        !is.na(.data$coarse_fragment_vol_pfh_otheryear) ~ "pfh (other year)",
        TRUE ~ NA_character_),
      coarse_fragment_vol = coalesce(
        .data$coarse_fragment_vol,
        .data$coarse_fragment_vol_pfh_sameyear,
        .data$coarse_fragment_vol_som_otheryear,
        .data$coarse_fragment_vol_pfh_otheryear)) %>%
    select(-coarse_fragment_vol_pfh_sameyear,
           -coarse_fragment_vol_som_otheryear,
           -coarse_fragment_vol_pfh_otheryear)


  cat("Data sources 'coarse_fragment_vol' after internal gap-filling:\n")
  print(table(df$coarse_fragment_source))


  # Organic layer weight: combine columns
  # (Note: no other data sources for organic_layer_weight)


  ## 4.4. Total organic carbon: combine columns ----



  # TO REMOVE !!!!

  if (!"organic_carbon_total_orig" %in% names(df)) {
    df$organic_carbon_total_orig <- df$organic_carbon_total
  }

  df <- df %>%
    mutate(
      organic_carbon_total_source = case_when(
        !is.na(.data$organic_carbon_total) ~ "som (same year)",
        !is.na(.data$organic_carbon_total_pfh_sameyear) ~ "pfh (same year)",
        TRUE ~ NA_character_),
      organic_carbon_total = coalesce(
        .data$organic_carbon_total,
        .data$organic_carbon_total_pfh_sameyear)) %>%
    select(-organic_carbon_total_pfh_sameyear)

  cat("Data sources 'organic_carbon_total' after internal gap-filling:\n")
  print(table(df$organic_carbon_total_source))


  ## 4.5. Clay: combine columns ----

  if (!"part_size_clay_orig" %in% names(df)) {
    df$part_size_clay_orig <- df$part_size_clay
  }

  df <- df %>%
    mutate(
      part_size_clay_source = case_when(
        !is.na(.data$part_size_clay) ~ "som (same year)",
        !is.na(.data$part_size_clay_pfh_sameyear) ~ "pfh (same year)",
        !is.na(.data$part_size_clay_som_otheryear) ~ "som (other year)",
        !is.na(.data$part_size_clay_pfh_otheryear) ~ "pfh (other year)",
        TRUE ~ NA_character_),
      part_size_clay = coalesce(
        .data$part_size_clay,
        .data$part_size_clay_pfh_sameyear,
        .data$part_size_clay_som_otheryear,
        .data$part_size_clay_pfh_otheryear)) %>%
    select(-part_size_clay_pfh_sameyear,
           -part_size_clay_som_otheryear,
           -part_size_clay_pfh_otheryear)

  cat("Data sources 'part_size_clay' after internal gap-filling:\n")
  print(table(df$part_size_clay_source))


  ## 4.6. Silt: combine columns ----

  if (!"part_size_silt_orig" %in% names(df)) {
    df$part_size_silt_orig <- df$part_size_silt
  }

  df <- df %>%
    mutate(
      part_size_silt_source = case_when(
        !is.na(.data$part_size_silt) ~ "som (same year)",
        !is.na(.data$part_size_silt_pfh_sameyear) ~ "pfh (same year)",
        !is.na(.data$part_size_silt_som_otheryear) ~ "som (other year)",
        !is.na(.data$part_size_silt_pfh_otheryear) ~ "pfh (other year)",
        TRUE ~ NA_character_),
      part_size_silt = coalesce(
        .data$part_size_silt,
        .data$part_size_silt_pfh_sameyear,
        .data$part_size_silt_som_otheryear,
        .data$part_size_silt_pfh_otheryear)) %>%
    select(-part_size_silt_pfh_sameyear,
           -part_size_silt_som_otheryear,
           -part_size_silt_pfh_otheryear)

  cat("Data sources 'part_size_silt' after internal gap-filling:\n")
  print(table(df$part_size_silt_source))


  ## 4.7. Sand: combine columns ----

  if (!"part_size_sand_orig" %in% names(df)) {
    df$part_size_sand_orig <- df$part_size_sand
  }

  df <- df %>%
    mutate(
      part_size_sand_source = case_when(
        !is.na(.data$part_size_sand) ~ "som (same year)",
        !is.na(.data$part_size_sand_pfh_sameyear) ~ "pfh (same year)",
        !is.na(.data$part_size_sand_som_otheryear) ~ "som (other year)",
        !is.na(.data$part_size_sand_pfh_otheryear) ~ "pfh (other year)",
        TRUE ~ NA_character_),
      part_size_sand = coalesce(
        .data$part_size_sand,
        .data$part_size_sand_pfh_sameyear,
        .data$part_size_sand_som_otheryear,
        .data$part_size_sand_pfh_otheryear)) %>%
    select(-part_size_sand_pfh_sameyear,
           -part_size_sand_som_otheryear,
           -part_size_sand_pfh_otheryear)

  cat("Data sources 'part_size_sand' after internal gap-filling:\n")
  print(table(df$part_size_sand_source))



  ## 4.8. Organic layer weight: combine columns ----

  if (!"organic_layer_weight_orig" %in% names(df)) {
    df$organic_layer_weight_orig <- df$organic_layer_weight
  }

  df <- df %>%
    mutate(
      organic_layer_weight_source =
        ifelse(!is.na(.data$organic_layer_weight),
               "som (same year)",
               ifelse(!is.na(.data$layer_thickness) &
                        !is.na(.data$bulk_density),
                      "som (bulk_density)",
                      NA)),
      organic_layer_weight =
        ifelse(!is.na(.data$organic_layer_weight),
               .data$organic_layer_weight,
               ifelse(!is.na(.data$layer_thickness) &
                        !is.na(.data$bulk_density),
                      (.data$bulk_density) *
                        (0.01 * .data$layer_thickness),
                      NA)))

  cat("Data sources 'organic_layer_weight' after internal gap-filling:\n")
  print(table(df$organic_layer_weight_source))


#  } # End of "som"







  # . ----
  # pfh ----


  if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

    # Import survey forms ----

    if (is.null(data_frame)) {
      df <- get_env(survey_form)
    } else {
      df <- data_frame
    }

    som <- get_env(paste0(unlist(strsplit(survey_form, "_"))[1], "_som"))





    # 1. Source 1: "som" (same year) ----

    # Redundant layers do already need to be removed before
    # being able to harmonise the layers into pre-defined depth intervals

    assertthat::assert_that("layer_number" %in% names(som))


    # Convert fixed depth layers ("som")
    # into a dataframe with pre-defined pedogenic horizons ("pfh")

    # This takes some time, so check whether this form already exists
    # in the global environment

    name_som_pedogenic <-
      paste0(unlist(strsplit(survey_form, "_"))[1], "_som_pedogenic")

    if (!exists(name_som_pedogenic) ||
        (exists(name_som_pedogenic) &&
         any(!"data.frame" %in% class(get_env(name_som_pedogenic))))) {

      source("./src/functions/harmonise_into_fixed_depth_layers.R")

      name_som <- paste0(unlist(strsplit(survey_form, "_"))[1], "_som")

      som_pedogenic <-
        harmonise_into_fixed_depth_layers(data_frame = som,
                                          survey_form_df = name_som,
                                          target_layers_df = df)
      assign_env(name_som_pedogenic,
                 som_pedogenic)

    } else {

      som_pedogenic <- get_env(name_som_pedogenic)

    }

    # No need to aggregate per plot survey layer
    # since we can use the unique_survey_profile column to join







    # Aggregate per plot survey (per survey layer) across different profiles

    som_pedogenic_agg_prof <- som_pedogenic %>%
      mutate(unique_layer_repetition = paste0(code_country, "_",
                                              survey_year, "_",
                                              code_plot, "_",
                                              horizon_master, "_",
                                              profile_pit_id)) %>%
      group_by(unique_layer_repetition,
               code_country, survey_year, code_plot,
               horizon_master, profile_pit_id) %>%
      summarise(horizon_limit_up =
                  mean(horizon_limit_up, na.rm = TRUE),
                horizon_limit_low =
                  mean(horizon_limit_low, na.rm = TRUE),
                organic_carbon_total =
                  mean(organic_carbon_total, na.rm = TRUE),
                part_size_clay =
                  mean(part_size_clay, na.rm = TRUE),
                part_size_silt =
                  mean(part_size_silt, na.rm = TRUE),
                part_size_sand =
                  mean(part_size_sand, na.rm = TRUE),
                bulk_density =
                  mean(bulk_density, na.rm = TRUE),
                coarse_fragment_vol =
                  mean(coarse_fragment_vol, na.rm = TRUE),
                .groups = "drop")
     # as.data.frame %>%

    # No need to aggregate per plot layer
    # since we cannot use data from other surveys (different layer composition)







    # Source 2: "pfh" (other survey years)

    # We don't do this, because the composition of the layers can be entirely
    # different



    # 2. Compile ----

    ## 2.1. Add data to df ----

    df <- df %>%
      # som same year
      left_join(som_pedogenic_agg_prof %>%
                  # Don't use the TOC data
                  # because sometimes there are simply no TOC data in
                  # pfh from the start.
                  rename(bulk_density_som_sameyear =
                           bulk_density,
                         coarse_fragment_vol_som_sameyear =
                           coarse_fragment_vol,
                         part_size_clay_som_sameyear =
                           part_size_clay,
                         part_size_silt_som_sameyear =
                           part_size_silt,
                         part_size_sand_som_sameyear =
                           part_size_sand) %>%
                  select(unique_layer_repetition,
                         bulk_density_som_sameyear,
                         coarse_fragment_vol_som_sameyear,
                         part_size_clay_som_sameyear,
                         part_size_silt_som_sameyear,
                         part_size_sand_som_sameyear),
                by = "unique_layer_repetition")


    ## 2.2. Bulk density: combine columns ----

      df <- df %>%
        mutate(
          bulk_density_source = case_when(
            !is.na(.data$horizon_bulk_dens_measure) ~ "pfh (measured)",
            !is.na(.data$horizon_bulk_dens_est) ~ "pfh (estimated)",
            !is.na(.data$bulk_density_som_sameyear) ~ "som (same year)",
            TRUE ~ NA_character_),
          bulk_density = coalesce(
            .data$horizon_bulk_dens_measure,
            .data$horizon_bulk_dens_est,
            .data$bulk_density_som_sameyear)) %>%
        select(-bulk_density_som_sameyear)



    cat("Data sources 'bulk_density' after internal gap-filling:\n")
    print(table(df$bulk_density_source))


    ## 2.3. Coarse fragments: combine columns ----

    d_soil_coarse_fragments <-
      read.csv2("./data/additional_data/d_soil_coarse_fragments.csv") %>%
      select(code, coarse_fragment_vol_avg)


    df <- df %>%
      # Convert volumetric coarse fragment codes to actual average vol %
      left_join(d_soil_coarse_fragments,
                by = join_by(code_horizon_coarse_vol == code)) %>%
      # Convert weight percentages to volumetric percentages:
      # Imagine: 1 m³ of fine earth contains
      # e.g. 1300 kg fine earth (bulk density).
      # Then, imagine the weight percentage of coarse fragments
      # from that soil is 11 %.
      # That means that there is 1300 kg * 11/89 = 160.7 kg of coarse fragments
      # for 1 m³ of fine earth in this soil.
      # Imagine the coarse fragments have a particle density of 2650 kg per m³.
      # Then, we can calculate that this 160.7 kg of coarse fragments occupies
      # 160.7/2650 = 0.061 m³.
      # As such, the vol % of coarse fragments will be 0.061 / (1 + 0.061)
      mutate(coarse_fragment_aid =
             ifelse(!is.na(bulk_density) & !is.na(horizon_coarse_weight),
                    (.data$bulk_density *
                       (.data$horizon_coarse_weight /
                          (100 - .data$horizon_coarse_weight))) / 2650,
                    NA)) %>%
      mutate(coarse_fragment_vol_converted =
               ifelse(!is.na(.data$coarse_fragment_aid),
                      as.numeric((.data$coarse_fragment_aid /
                                    (1 + .data$coarse_fragment_aid)) * 100),
                      NA)) %>%
      select(-coarse_fragment_aid) %>%
      mutate(coarse_fragment_vol = case_when(
        # Priority 1: converted weight %
        !is.na(coarse_fragment_vol_converted) ~ coarse_fragment_vol_converted,
        # Priority 2: fractions from "som"
        !is.na(coarse_fragment_vol_som_sameyear) ~
          coarse_fragment_vol_som_sameyear,
        # Priority 3: volumetric classes
        !is.na(coarse_fragment_vol_avg) ~ coarse_fragment_vol_avg,
        TRUE ~ NA_real_)) %>%
      mutate(
        coarse_fragment_source = case_when(
          !is.na(.data$coarse_fragment_vol_converted) ~
                               "pfh (horizon_coarse_weight)",
          !is.na(.data$coarse_fragment_vol_som_sameyear) ~ "som (same year)",
          !is.na(.data$coarse_fragment_vol_avg) ~
                               "pfh (code_horizon_coarse_vol)",
          TRUE ~ NA_character_)) %>%
      select(-coarse_fragment_vol_converted,
             -coarse_fragment_vol_som_sameyear,
             -coarse_fragment_vol_avg)



    cat("Data sources 'coarse_fragment_vol' after internal gap-filling:\n")
    print(table(df$coarse_fragment_source))




    ## Total organic carbon: do not use other data sources
    ## since this is sometimes a limiting parameter (not reported in the
    ## whole profile)



    ## 2.4. Clay: combine columns ----

    if (!"horizon_clay" %in% names(df)) {
      df$horizon_clay_orig <- df$horizon_clay
    }

    df <- df %>%
      mutate(
        part_size_clay_source = case_when(
          !is.na(.data$horizon_clay) ~ "pfh (same year)",
          !is.na(.data$part_size_clay_som_sameyear) ~ "som (same year)",
          TRUE ~ NA_character_),
        horizon_clay = coalesce(
          .data$horizon_clay,
          .data$part_size_clay_som_sameyear)) %>%
      select(-part_size_clay_som_sameyear)

    cat("Data sources 'part_size_clay' after internal gap-filling:\n")
    print(table(df$part_size_clay_source))



    ## 2.5. Silt: combine columns ----

    if (!"horizon_silt" %in% names(df)) {
      df$horizon_silt_orig <- df$horizon_silt
    }

    df <- df %>%
      mutate(
        part_size_silt_source = case_when(
          !is.na(.data$horizon_silt) ~ "pfh (same year)",
          !is.na(.data$part_size_silt_som_sameyear) ~ "som (same year)",
          TRUE ~ NA_character_),
        horizon_silt = coalesce(
          .data$horizon_silt,
          .data$part_size_silt_som_sameyear)) %>%
      select(-part_size_silt_som_sameyear)

    cat("Data sources 'part_size_silt' after internal gap-filling:\n")
    print(table(df$part_size_silt_source))



    ## 2.6. Sand: combine columns ----

    if (!"horizon_sand" %in% names(df)) {
      df$horizon_sand_orig <- df$horizon_sand
    }

    df <- df %>%
      mutate(
        part_size_sand_source = case_when(
          !is.na(.data$horizon_sand) ~ "pfh (same year)",
          !is.na(.data$part_size_sand_som_sameyear) ~ "som (same year)",
          TRUE ~ NA_character_),
        horizon_sand = coalesce(
          .data$horizon_sand,
          .data$part_size_sand_som_sameyear)) %>%
      select(-part_size_sand_som_sameyear)

    cat("Data sources 'part_size_sand' after internal gap-filling:\n")
    print(table(df$part_size_sand_source))





    ## 2.7. Organic layer weight ----

    df <- df %>%
      mutate(layer_thickness = ifelse(!is.na(horizon_limit_up) &
                                        !is.na(horizon_limit_low),
                                      abs(.data$horizon_limit_up -
                                            .data$horizon_limit_low),
                                      NA),
              organic_layer_weight =
                     ifelse(!is.na(.data$layer_thickness) &
                              !is.na(.data$bulk_density) &
                              (.data$layer_type != "mineral"),
                            (.data$bulk_density) *
                              (0.01 * .data$layer_thickness),
                            NA))



  } # End of "pfh"








  # Save ----

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)

  } else {
    return(df)

  }

}
