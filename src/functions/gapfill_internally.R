

gapfill_internally <- function(survey_form,
                               data_frame = NULL,
                               save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  # Note: This function is designed for internal gap-filling of 'som'
  # survey forms only. It won't work with other types of survey forms.

  assertthat::assert_that(unlist(strsplit(survey_form, "_"))[2] == "som",
                          msg = paste0("This function is designed for ",
                                       "internal gap-filling of 'som' ",
                                       "survey forms only"))

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
    
    pfh_fixed <-
      harmonise_into_fixed_depth_layers(survey_form = pfh)
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
    mutate(coarse_fragment_vol =
             # If both volumetric codes and weight % are available:
             ifelse(!is.na(.data$coarse_fragment_vol_avg) &
                      !is.na(.data$coarse_fragment_vol_converted),
                    # Better not to take the average in that case,
                    # because the converted weight % seem more reliable
                    # (volumetric classes were broad)
                    # Priority: converted weight %
                    .data$coarse_fragment_vol_converted,
                    # Else, take whichever measure for coarse fragments
                    # that is available
                    ifelse(!is.na(.data$coarse_fragment_vol_converted),
                           .data$coarse_fragment_vol_converted,
                           .data$coarse_fragment_vol_avg)))
  
  
  
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
    left_join(som_otheryear %>% #! 7773 of x; 10663 of y
                # c(15, 15)_c(2007, 2007)_c(1620, 1620)_c("M4050", "M5060")_c(1, 1)
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
        TRUE ~ NA_character_),
      bulk_density = coalesce(
        .data$bulk_density,
        .data$bulk_density_sw_swc_sameyear,
        .data$bulk_density_pfh_sameyear,
        .data$bulk_density_som_otheryear,
        .data$bulk_density_sw_swc_otheryear,
        .data$bulk_density_pfh_otheryear)) %>%
    select(-bulk_density_sw_swc_sameyear,
           -bulk_density_pfh_sameyear,
           -bulk_density_som_otheryear,
           -bulk_density_sw_swc_otheryear,
           -bulk_density_pfh_otheryear)
  
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
  
  
  # Save ----
  
  if (save_to_env == TRUE) {
    assign_env(survey_form, df)

  } else {
    return(df)

  }

}