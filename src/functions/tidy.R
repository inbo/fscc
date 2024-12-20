
tidy <- function(survey_form,
                 data_frame = NULL,
                 save_to_env = FALSE) {


  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(paste0(" \nTidy up '", survey_form, "'\n"))

  code_survey <- unlist(str_split(survey_form, "_"))[1]
  survey_form_type <- unlist(str_split(survey_form, "_"))[2]


  # Import survey forms ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }


  # Columns to exclude ----


  columns_to_exclude <- c(
    "bulk_density_afscdb",
    "bulk_density_layer_weight",
    "bulk_density_pre_gapfill",
    "bulk_density_fscdb",
    "bd_ptf",
    "change_date_google_drive",
    "coarse_fragment_vol_afscdb",
    "coarse_fragment_vol_avg",
    "coarse_fragment_vol_pre_gapfill",
    "coarse_fragment_vol_fscdb",
    "horizon_bulk_dens_est",
    "horizon_coarse_weight",
    "n_total_fscdb",
    "n_total_afscdb",
    "pfh_n_total",
    "som_n_total",
    "extrac_p_fscdb",
    "extrac_p_afscdb",
    "som_extrac_p",
    "extrac_s_fscdb",
    "extrac_s_afscdb",
    "som_extrac_s",
    "rea_fe_fscdb",
    "rea_fe_afscdb",
    "som_rea_fe",
    "rea_al_fscdb",
    "rea_al_afscdb",
    "som_rea_al",
    "exch_ca_fscdb",
    "exch_ca_afscdb",
    "pfh_exch_ca",
    "som_exch_ca",
    "organic_carbon_total_afscdb",
    "organic_layer_weight_afscdb",
    "organic_layer_weight_bd",
    "organic_layer_weight_bd_max",
    "organic_layer_weight_bd_median",
    "organic_layer_weight_bd_min",
    "organic_layer_weight_ptf",
    "organic_layer_weight_ptf_min",
    "organic_layer_weight_ptf_max",
    "organic_layer_weight_fscdb",
    "part_size_clay_afscdb",
    "part_size_clay_pre_gapfill",
    "part_size_sand_afscdb",
    "part_size_sand_pre_gapfill",
    "part_size_silt_afscdb",
    "part_size_silt_pre_gapfill",
    "pfh_organic_carbon_total",
    "pfh_coarse_fragment_vol_avg",
    "pfh_coarse_fragment_vol_avg_max",
    "pfh_coarse_fragment_vol_avg_min",
    "pfh_coarse_fragment_vol_avg_survey_year",
    "pfh_coarse_fragment_vol_converted",
    "pfh_coarse_fragment_vol_converted_max",
    "pfh_coarse_fragment_vol_converted_min",
    "pfh_coarse_fragment_vol_converted_survey_year",
    "pfh_horizon_bulk_dens_est",
    "pfh_horizon_bulk_dens_est_max",
    "pfh_horizon_bulk_dens_est_min",
    "pfh_horizon_bulk_dens_est_survey_year",
    "pfh_horizon_bulk_dens_measure",
    "pfh_horizon_bulk_dens_measure_max",
    "pfh_horizon_bulk_dens_measure_min",
    "pfh_horizon_bulk_dens_measure_survey_year",
    "pfh_horizon_clay",
    "pfh_horizon_clay_max",
    "pfh_horizon_clay_min",
    "pfh_horizon_sand",
    "pfh_horizon_sand_max",
    "pfh_horizon_sand_min",
    "pfh_horizon_silt",
    "pfh_horizon_silt_max",
    "pfh_horizon_silt_min",
    "pfh_texture_survey_year",
    "som_organic_carbon_total",
    "som_bulk_density",
    "som_bulk_density_max",
    "som_bulk_density_min",
    "som_bulk_density_survey_year",
    "som_coarse_fragment_vol",
    "som_coarse_fragment_vol_max",
    "som_coarse_fragment_vol_min",
    "som_coarse_fragment_vol_survey_year",
    "som_part_size_clay",
    "som_part_size_clay_max",
    "som_part_size_clay_min",
    "som_part_size_sand",
    "som_part_size_sand_max",
    "som_part_size_sand_min",
    "som_part_size_silt",
    "som_part_size_silt_max",
    "som_part_size_silt_min",
    "som_texture_survey_year",
    "swc_bulk_density",
    "swc_bulk_density_max",
    "swc_bulk_density_min",
    "swc_bulk_density_survey_year",
    "organic_carbon_total_fscdb",

    "origin",
    "q_flag",
    "line_nr",
    "qif_key",
    # "unique_survey",
    # "unique_survey_repetition",
    # "unique_survey_layer",
    # "unique_layer_repetition",
    # "unique_survey_profile",
    "unique_layer",
    "origin_merged",
    "origin_merge_info",
    "layer_number_bg",
    "layer_number_ff",
    "organic_layer_weight_wrong_unit",
    "sum_texture",
    "bulk_density_layer_weight",
    "horizon_number",
    "colour_moist_hex")


# Tidy dataframe ----

  if (survey_form_type == "som") {

     df <- df %>%
      select(-any_of(columns_to_exclude)) %>%
      relocate(plot_id, .after = code_plot) %>%
      relocate(layer_thickness, .after = layer_limit_inferior) %>%
      mutate(profile_id = paste0(survey_year, "_",
                                 plot_id, "_",
                                 repetition)) %>%
      relocate(profile_id, .after = repetition) %>%
      relocate(date_labor_analyses, .before = change_date) %>%
      relocate(download_date, .after = change_date) %>%
      relocate(sum_acid_cations, .after = rea_fe) %>%
      relocate(sum_base_cations, .after = rea_fe) %>%
      relocate(c_to_n_ratio, .after = rea_fe) %>%
      relocate(organic_layer_weight, .after = bulk_density) %>%
      relocate(partner_code, .after = code_country) %>%
      select(
        # Selecting columns without the specified patterns
        which(!grepl("_rt|_loq|_orig|_source", names(.))),
        # Selecting columns with the specified patterns in the desired order
        contains("_source"),
        # contains("_orig"),
        contains("_loq"),
        contains("_rt")) %>%
      select(
        country:layer_thickness,
        starts_with("bulk_density"),
        starts_with("organic_layer_weight"),
        starts_with("coarse_fragment"),
        starts_with("part_size_clay"),
        starts_with("part_size_silt"),
        starts_with("part_size_sand"),
        contains("texture"),
        starts_with("organic_carbon_total"),
        starts_with("n_total"),
        starts_with("extrac_p"),
        starts_with("extrac_s"),
        starts_with("rea_fe"),
        starts_with("rea_al"),
        starts_with("exch_ca"),
        # horizon_caco3_total:sum_base_cations,
        everything()) %>%
       relocate(extrac_pb, .after = extrac_ni) %>%
       relocate(extrac_pb_loq, .after = extrac_ni_loq) %>%
       relocate(extrac_pb_rt, .after = extrac_ni_rt) %>%
       # Move "unique" columns to the end
       select(which(!grepl("unique_|date", names(.))),
              contains("date"),
              contains("unique_")) %>%
       relocate(other_obs, .before = date_labor_analyses) %>%
       relocate(code_line, .before = unique_survey)

  }

  if (survey_form_type == "pfh") {

    df <- df %>%
      select(-any_of(columns_to_exclude)) %>%
      relocate(plot_id, .after = code_plot) %>%
      relocate(layer_thickness, .after = horizon_limit_low) %>%
      mutate(profile_id = paste0(survey_year, "_",
                                 plot_id, "_",
                                 profile_pit_id)) %>%
      relocate(profile_id, .after = profile_pit_id) %>%
      relocate(date_labor_analyses, .before = change_date) %>%
      relocate(download_date, .after = change_date) %>%
      relocate(sum_base_cations, .after = horizon_cec) %>%
      relocate(c_to_n_ratio, .after = horizon_cec) %>%
      relocate(partner_code, .after = code_country) %>%
      relocate(organic_layer_weight, .after = bulk_density) %>%
      relocate(code_horizon_coarse_vol, .after = coarse_fragment_vol) %>%
      select(
        # Selecting columns without the specified patterns
        which(!grepl("_rt|_loq|_orig|_source", names(.))),
        # Selecting columns with the specified patterns in the desired order
        contains("_source"),
        # contains("_orig"),
        contains("_loq"),
        contains("_rt")) %>%
      select(
        country:layer_thickness,
        starts_with("bulk_density"),
        starts_with("organic_layer_weight"),
        starts_with("coarse_fragment"),
        starts_with("part_size_clay"),
        starts_with("part_size_silt"),
        starts_with("part_size_sand"),
        contains("texture"),
        starts_with("horizon_c_organic_total"),
        starts_with("horizon_n_total"),
        starts_with("horizon_exch_ca"),
        horizon_caco3_total:sum_base_cations,
        everything()) %>%
      # Move "unique" columns to the end
      select(-starts_with("unique"),
             starts_with("unique")) %>%
      # Move "unique" columns to the end
      select(which(!grepl("unique_|date", names(.))),
             contains("date"),
             contains("unique_")) %>%
      relocate(other_obs, .before = date_labor_analyses) %>%
      relocate(code_line, .before = unique_survey)

  }







  # Save ----

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)
  } else {
    return(df)
  }

}
