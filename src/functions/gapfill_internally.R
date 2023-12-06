

gapfill_internally <- function(survey_form,
                               data_frame = NULL,
                               save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(paste0(" \nGap-fill '", survey_form, "' internally\n"))


  # . ----
  # som ----


  if (unlist(strsplit(survey_form, "_"))[2] == "som") {

  # Import survey forms ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }

  pfh <- get_env(paste0(unlist(strsplit(survey_form, "_"))[1], "_pfh"))



  # 1. Source 1: "sw_swc" ----

  if (unlist(strsplit(survey_form, "_"))[1] == "so") {
    # Level II

    # Import additional sw_swc file with corresponding fixed-depth layers
    # by Nathalie (manually created)

    # TO DO: automate the assignment of corresponding fixed-depth layers

    file_path <- "./data/additional_data/sw_swc/SW_SWC_code_layer_SOM.csv"

    assertthat::assert_that(file.exists(file_path),
                            msg = paste0("'", file_path, "' ",
                                         "does not exist."))

    sw_swc_adds <- read.csv2(file_path)

    # Preprocess file

    sw_swc_adds_sameyear <- sw_swc_adds %>%
      rename(code_layer_som = code_layer_SOM) %>%
      rename(plot_id = PLOTID) %>%
      # Remove records without corresponding fixed-depth layer
      filter(code_layer_som %in%
               c("O", "OH", "OFH",
                 "M05", "M51", "M01", "M12", "M24", "M48")) %>%
      # Create unique_survey_layer
      mutate(unique_survey_layer =
               paste0(code_country, "_",
                      survey_year, "_",
                      code_plot, "_",
                      code_layer_som)) %>%
      # Create unique_layer
      mutate(unique_layer =
               paste0(code_country, "_",
                      code_plot, "_",
                      code_layer_som)) %>%
      # Aggregate over replicates
      group_by(unique_survey_layer, unique_layer) %>%
      summarise(bulk_density =
                  mean(bulk_density, na.rm = TRUE),
                .groups = "drop")


    # Aggregate different survey years per unique layer (plot_id x code_layer)
    # To gap-fill data not from the same survey_year

    sw_swc_adds_otheryear <- sw_swc_adds_sameyear %>%
      group_by(unique_layer) %>%
      summarise(bulk_density =
                  mean(bulk_density, na.rm = TRUE),
                .groups = "drop")

  }




  # 2. Source 2: "pfh" ----

  # Redundant layers do already need to be removed from so_pfh before
  # being able to harmonise the layers into pre-defined depth intervals

  assertthat::assert_that("layer_number" %in% names(pfh))


  # Convert pedogenic depth layers ("pfh")
  # into a dataframe with pre-defined fixed-depth layers ("som")

  # This takes some time, so check whether this form already exists
  # in the global environment

  name_pfh_fixed <- paste0(unlist(strsplit(survey_form, "_"))[1], "_pfh_fixed")

  if (!exists(name_pfh_fixed) ||
       (exists(name_pfh_fixed) &&
        any(!"data.frame" %in% class(get_env(name_pfh_fixed))))) {

    source("./src/functions/harmonise_into_fixed_depth_layers.R")

    name_pfh <- paste0(unlist(strsplit(survey_form, "_"))[1], "_pfh")

    pfh_fixed <-
      harmonise_into_fixed_depth_layers(data_frame = pfh,
                                        survey_form_df = name_pfh)

    assign_env(name_pfh_fixed,
               pfh_fixed)

  } else {

    pfh_fixed <- get_env(name_pfh_fixed)

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
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    as.data.frame %>%
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
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    as.data.frame %>%
    mutate(layer_limit_superior = ifelse(is.nan(layer_limit_superior),
                                         NA, layer_limit_superior),
           layer_limit_inferior = ifelse(is.nan(layer_limit_inferior),
                                         NA, layer_limit_inferior),
           horizon_clay = ifelse(is.nan(horizon_clay),
                                 NA, horizon_clay),
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
              bulk_density =
                mean(bulk_density, na.rm = TRUE),
              coarse_fragment_vol =
                mean(coarse_fragment_vol, na.rm = TRUE),
              .groups = "drop") %>%
    as.data.frame %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))

  # organic_layer_weight and organic_carbon_total should not be assessed
  # based on other survey years








  # 4. Compile ----

  ## 4.1. Add data to df ----

  if (unlist(strsplit(survey_form, "_"))[1] == "so") {
      # Level II

  df <- df %>%
    # sw_swc same year
    left_join(sw_swc_adds_sameyear %>%
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
                         horizon_clay) %>%
                select(unique_survey_layer,
                       organic_carbon_total_pfh_sameyear,
                       bulk_density_pfh_sameyear,
                       coarse_fragment_vol_pfh_sameyear,
                       part_size_clay_pfh_sameyear),
              by = "unique_survey_layer") %>%
    # som other year
    left_join(som_otheryear %>%
                rename(bulk_density_som_otheryear =
                         bulk_density,
                       coarse_fragment_vol_som_otheryear =
                         coarse_fragment_vol,
                       part_size_clay_som_otheryear =
                         part_size_clay) %>%
                select(unique_layer,
                       bulk_density_som_otheryear,
                       coarse_fragment_vol_som_otheryear,
                       part_size_clay_som_otheryear),
              by = "unique_layer")

  if (unlist(strsplit(survey_form, "_"))[1] == "so") {
      # Level II

  df <- df %>%
    # sw_swc other year
    left_join(sw_swc_adds_otheryear %>%
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
                         horizon_clay) %>%
                select(unique_layer,
                       bulk_density_pfh_otheryear,
                       coarse_fragment_vol_pfh_otheryear,
                       part_size_clay_pfh_otheryear),
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



  ## 4.6. Organic layer weight: combine columns ----

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


  } # End of "som"







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
                bulk_density =
                  mean(bulk_density, na.rm = TRUE),
                coarse_fragment_vol =
                  mean(coarse_fragment_vol, na.rm = TRUE),
                .groups = "drop")
     # as.data.frame %>%

    # No need to aggregate per plot layer
    # since we cannot use data from other surveys (different layer composition)






    # # Aggregate per unique layer (plot_id x code_layer)
    # # across different survey years
    # # To gap-fill data not from the same survey_year
    #
    # som_pedogenic_otheryear <- som_pedogenic %>%
    #   mutate(unique_layer = paste0(code_country, "_",
    #                                code_plot, "_",
    #                                code_layer)) %>%
    #   group_by(unique_layer,
    #            code_country, code_plot, code_layer) %>%
    #   summarise(layer_limit_superior =
    #               mean(layer_limit_superior, na.rm = TRUE),
    #             layer_limit_inferior =
    #               mean(layer_limit_inferior, na.rm = TRUE),
    #             horizon_clay =
    #               mean(horizon_clay, na.rm = TRUE),
    #             bulk_density =
    #               mean(bulk_density, na.rm = TRUE),
    #             coarse_fragment_vol =
    #               mean(coarse_fragment_vol, na.rm = TRUE),
    #             .groups = "drop") %>%
    #   as.data.frame %>%
    #   mutate(layer_limit_superior = ifelse(is.nan(layer_limit_superior),
    #                                        NA, layer_limit_superior),
    #          layer_limit_inferior = ifelse(is.nan(layer_limit_inferior),
    #                                        NA, layer_limit_inferior),
    #          horizon_clay = ifelse(is.nan(horizon_clay),
    #                                NA, horizon_clay),
    #          bulk_density = ifelse(is.nan(bulk_density),
    #                                NA, bulk_density),
    #          coarse_fragment_vol = ifelse(is.nan(coarse_fragment_vol),
    #                                       NA, coarse_fragment_vol))
    #
    # # horizon_c_organic_total should not be assessed based on values from
    # # other survey years









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
                           part_size_clay) %>%
                  select(unique_layer_repetition,
                         bulk_density_som_sameyear,
                         coarse_fragment_vol_som_sameyear,
                         part_size_clay_som_sameyear),
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



    ## 2.5. Organic layer weight ----

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
