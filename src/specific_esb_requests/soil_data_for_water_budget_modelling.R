

# Compile dataset for LWF-Brook90
# -------------------------------

# Question: Deliver LII (physical) soil data which are relevant for
# water budget modelling with LWF-Brook90, for one profile per
# plot_id until the effective soil depth.

# Script initiation date: 16 Apr 2024

# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O



# Prepare packages ----

# Define required packages
stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"))


# Harmonise and correct data ----

# This is the procedure followed to generate the so-called "layer 1" data
# (i.e. corrected and harmonised). However, we will use the data version from
# before the internal gap-filling (gap-filling using directly reported data).
# That is why we do not simply import the most recent "layer 1" data.


## Import data ----

source("./src/functions/read_raw.R")

read_raw("si", save_to_env = TRUE)
read_raw("so", save_to_env = TRUE)
read_raw("sw", save_to_env = TRUE)


## Solve issues with duplicate records ----

source("./src/functions/solve_record_inconsistencies.R")

so_som <- solve_record_inconsistencies(survey_form = "so_som",
                                       data_frame = so_som,
                                       solve = TRUE,
                                       save_to_env = FALSE)

so_pfh <- solve_record_inconsistencies(survey_form = "so_pfh",
                                       data_frame = so_pfh,
                                       solve = TRUE,
                                       save_to_env = FALSE)


## Gap-fill using new data from Partner Inconsistency Reports ----

source("./src/functions/gapfill_from_pir.R")

gapfill_from_pir(code_survey = "si",
                 save_to_env = TRUE)
gapfill_from_pir(code_survey = "so",
                 save_to_env = TRUE)
gapfill_from_pir(code_survey = "sw",
                 save_to_env = TRUE)


## Gap-fill using old database sources ----

source("./src/functions/gapfill_from_old_data.R")

so_som <- gapfill_from_old_data(survey_form = "so_som",
                                data_frame = so_som,
                                save_to_env = FALSE)


## Automated data corrections: primary keys (survey_year, code_layer) ----

source("./src/functions/get_primary_inconsistencies.R")

get_primary_inconsistencies(code_survey = "si",
                            save_to_env = TRUE)
get_primary_inconsistencies(code_survey = "so", solve = TRUE,
                            save_to_env = TRUE)
get_primary_inconsistencies(code_survey = "sw",
                            save_to_env = TRUE)


## Automated data corrections: range/presence of data ----

source("./src/functions/get_range_inconsistencies.R")

so_som <- get_range_inconsistencies("so_som", so_som,
                                    solve = TRUE, save_to_env = FALSE)
so_pfh <- get_range_inconsistencies("so_pfh", so_pfh,
                                    solve = TRUE, save_to_env = FALSE)
sw_swc <- get_range_inconsistencies("sw_swc", sw_swc,
                                    solve = TRUE, save_to_env = FALSE)
so_prf <- get_range_inconsistencies("so_prf", so_prf,
                                    save_to_env = FALSE)
so_pls <- get_range_inconsistencies("so_pls", so_pls,
                                    save_to_env = FALSE)



source("./src/functions/harmonise_below_loqs.R")

so_som <- harmonise_below_loqs(survey_form = "so_som",
                               data_frame = so_som)

so_pfh <- harmonise_below_loqs(survey_form = "so_pfh",
                               data_frame = so_pfh)





## Automated data corrections: soil layers ----
# "solve" indicates whether the obvious mistakes can be solved

# Attention: make sure that "pfh" survey forms are processed first
# to facilitate gap-filling of "som" based on "pfh"
# (e.g. layer limits forest floor)

source("./src/functions/get_layer_inconsistencies.R")

so_pfh <- get_layer_inconsistencies(survey_form = "so_pfh",
                                    data_frame = so_pfh,
                                    solve = TRUE,
                                    save_to_env = FALSE)
so_som <- get_layer_inconsistencies(survey_form = "so_som",
                                    data_frame = so_som,
                                    solve = TRUE,
                                    save_to_env = FALSE)


## Automated data corrections: derived variables ----

source("./src/functions/get_derived_variable_inconsistencies.R")

so_som <- get_derived_variable_inconsistencies("so_som", so_som,
                                               save_to_env = FALSE)
so_pfh <- get_derived_variable_inconsistencies("so_pfh", so_pfh,
                                               save_to_env = FALSE)


## Additional manual corrections ----

source("./src/functions/apply_additional_manual_corr.R")

so_som <- apply_additional_manual_corr(survey_form = "so_som",
                                       data_frame = so_som)
so_pfh <- apply_additional_manual_corr(survey_form = "so_pfh",
                                       data_frame = so_pfh)




# Summarise physical parameters in one profile per plot ----

# From different survey years and repetitions/profiles, this function retrieves
# the "best data" (i.e. usually from the most recent survey year) for different
# depths per plot, and the uncertainty associated with this selection.

source("./src/functions/harmonise_per_plot_layer.R")

som_plot <-
  harmonise_per_plot_layer(
    survey_form_input = "so_som",
    data_frame_input = so_som)

pfh_plot <-
  harmonise_per_plot_layer(
    survey_form_input = "so_pfh",
    data_frame_input = so_pfh)

swc_plot <-
  harmonise_per_plot_layer(
    survey_form_input = "sw_swc",
    data_frame_input = sw_swc)


# Get harmonised stratifiers ----
# This contains the effective soil depths

# Note that we limit the effective soil depths to 100 cm, since it was not
# mandatory for partners to report the exact soil depth if this was deeper than
# 100 cm. Therefore, it is better to consistently ignore soil below 100 cm,
# even though some partners did report exact soil depths below 100 cm.

source("./src/functions/harmonise_prf.R")

so_prf <- harmonise_prf(survey_form = "so_prf",
                        data_frame = so_prf,
                        save_to_env = FALSE)


source("./src/functions/get_stratifiers.R")

so_strat <- get_stratifiers("LII") %>%
  select(-eff_soil_depth_0,
         -date_profile_desc)






# Harmonise depth layers with eff_soil_depth ----

# Make sure that the lower layer limit of the lowest layer equals the
# effective soil depth.
# Add an extra layer at the bottom when the effective soil depth is deeper
# than the lower layer limit of the lowest layer.


df <- som_plot %>%
  mutate(
    # To indicate layers that fall completely below the effective soil depth
    records_to_remove = NA,
    # To indicate layers that were included in any of the original datasets
    # ("so_som", "so_pfh", "sw_swc")
    obs = TRUE)

# To add any extra records (layers) below the observations until the effective
# soil depth
extra_rows <- NULL


for (i in seq_along(unique(df$plot_id))) {

  plot_id_i <- unique(df$plot_id)[i]

  assertthat::assert_that(any(!is.na(df$layer_limit_inferior)))

  soil_depth_i <- so_strat %>%
    filter(plot_id == plot_id_i) %>%
    pull(eff_soil_depth)

  # If no information on soil depth, assume it is 100 cm

  if (is.na(soil_depth_i)) {
    soil_depth_i <- 100
  }

  df_sub <- df %>%
    filter(plot_id == plot_id_i)

  # Remove any layers that fall completely below the effective soil depth

  if (any(!is.na(df_sub$layer_limit_superior) &
          df_sub$layer_limit_superior >= soil_depth_i)) {

    df <- df %>%
      mutate(
        records_to_remove = ifelse(
          plot_id == plot_id_i &
            !is.na(layer_limit_superior) &
            layer_limit_superior >= soil_depth_i,
          TRUE,
          records_to_remove))

    df_sub <- df %>%
      filter(plot_id == plot_id_i) %>%
      filter(is.na(records_to_remove))
  }

  # If the effective soil depth falls within the depth range of the lowest
  # layer, make sure the inferior layer limit of that layer equals the
  # effective soil depth

  df_sub_lowest <- df_sub %>%
    filter(layer_number == max(df_sub$layer_number))

  if (soil_depth_i <= df_sub_lowest$layer_limit_inferior &&
      soil_depth_i >= df_sub_lowest$layer_limit_superior) {

    df <- df %>%
      mutate(
        layer_limit_inferior = ifelse(
          plot_id == plot_id_i &
            !is.na(layer_number) &
            layer_number == df_sub_lowest$layer_number,
          soil_depth_i,
          layer_limit_inferior))

  } else {

    assertthat::assert_that(
      soil_depth_i > df_sub_lowest$layer_limit_inferior)

    extra_row_i <- df_sub_lowest %>%
      mutate(layer_number = layer_number + 1,
             layer_limit_superior = layer_limit_inferior,
             layer_limit_inferior = soil_depth_i,
             code_layer = "X",
             obs = FALSE) %>%
      mutate_at(vars(any_of(contains(c("bulk_density",
                                       "coarse_fragment_vol",
                                       "part_size",
                                       "texture")))), ~NA)

    extra_rows <- bind_rows(extra_rows,
                            extra_row_i)

  }

} # End of "for" loop over plot_ids

df <- bind_rows(df,
                extra_rows) %>%
  filter(is.na(records_to_remove)) %>%
  arrange(country,
          plot_id,
          layer_number) %>%
  select(-records_to_remove)


assertthat::assert_that(
  all(is.na(df$layer_limit_inferior) |
        is.na(df$layer_limit_superior) |
        df$layer_limit_inferior > df$layer_limit_superior))







# Add physical parameters from "pfh" and "swc" to "som" ----

source("./src/functions/depth_join.R")

df <- depth_join(df1 = df,
                 df2 = pfh_plot,
                 parameters = NULL,
                 prefix_parameters_in_df1 = "pfh_")


df <- depth_join(df1 = df,
                 df2 = swc_plot,
                 prefix_parameters_in_df1 = "swc_")





# Add organic_layer_weight ----

df <- depth_join(df1 = df,
                 df2 = so_som,
                 parameters = "organic_layer_weight",
                 mode = "most_recent") %>%
  mutate(
    organic_layer_weight_source = ifelse(
      !is.na(organic_layer_weight),
      paste0("som (",
             organic_layer_weight_year,
             ")"),
      NA_character_)) %>%
  select(-organic_layer_weight_year) %>%
  relocate(organic_layer_weight, .after = bulk_density) %>%
  relocate(organic_layer_weight_source, .after = organic_layer_weight)


# Add total organic carbon content ----

df <- depth_join(df1 = df,
                 df2 = so_som,
                 parameters = "organic_carbon_total",
                 prefix_parameters_in_df1 = "som_",
                 mode = "most_recent")

df <- depth_join(df1 = df,
                 df2 = so_pfh,
                 parameters = "horizon_c_organic_total",
                 prefix_parameters_in_df1 = "pfh_",
                 mode = "most_recent")

df <- df %>%
  mutate(
    organic_carbon_total_source = case_when(
      !is.na(som_organic_carbon_total) ~
        paste0("som (", som_organic_carbon_total_year, ")"),
      !is.na(pfh_horizon_c_organic_total) ~
        paste0("pfh (", pfh_horizon_c_organic_total_year, ")"),
      .default = NA_character_),
    organic_carbon_total = coalesce(som_organic_carbon_total,
                                    pfh_horizon_c_organic_total)) %>%
  select(-som_organic_carbon_total,
         -som_organic_carbon_total_year,
         -pfh_horizon_c_organic_total,
         -pfh_horizon_c_organic_total_year) %>%
  relocate(organic_carbon_total, .after = layer_limit_inferior) %>%
  relocate(organic_carbon_total_source, .after = organic_carbon_total)


# Add texture code

df <- depth_join(df1 = df,
                 df2 = so_som,
                 parameters = "code_texture_class",
                 prefix_parameters_in_df1 = "som_",
                 mode = "constant_physical_parameters")

df <- depth_join(df1 = df,
                 df2 = so_pfh,
                 parameters = "code_horizon_texture_class",
                 prefix_parameters_in_df1 = "pfh_",
                 mode = "constant_physical_parameters")

d_texture_class <-
  read.csv("./data/raw_data/so/adds/dictionaries/d_texture_class.csv",
           sep = ";")

df <- df %>%
  mutate(code_texture_class = coalesce(som_code_texture_class,
                                       pfh_code_horizon_texture_class)) %>%
  select(-som_code_texture_class, -pfh_code_horizon_texture_class) %>%
  left_join(d_texture_class %>%
              rename(code_texture_class = code) %>%
              rename(texture_class = description) %>%
              select(code_texture_class, texture_class),
            by = "code_texture_class") %>%
  select(-code_texture_class)








# Combine physical parameters ----

## Bulk density ----

bulk_density_columns <-
  df %>%
  select(contains("bulk_dens"), -contains("survey_year")) %>%
  names

df <- df %>%
  mutate(
    bulk_density_source = case_when(
      !is.na(bulk_density) ~
        paste0("som (", .data$bulk_density_survey_year, ")"),
      !is.na(.$swc_bulk_density) ~
        paste0("swc (", swc_bulk_density_survey_year, ")"),
      !is.na(pfh_horizon_bulk_dens_measure) ~
        paste0("pfh_measure (",
               .data$pfh_horizon_bulk_dens_measure_survey_year, ")"),
      !is.na(pfh_horizon_bulk_dens_est) ~
        paste0("pfh_est (", .data$pfh_horizon_bulk_dens_est_survey_year, ")")),
    bulk_density = coalesce(
      bulk_density,
      swc_bulk_density,
      pfh_horizon_bulk_dens_measure,
      pfh_horizon_bulk_dens_est)) %>%
  rowwise() %>%
  mutate(
    bulk_density_min = ifelse(
      !is.na(bulk_density),
      round(min(c_across(all_of(bulk_density_columns)),
                na.rm = TRUE)),
      NA),
    bulk_density_max = ifelse(
      !is.na(bulk_density),
      round(max(c_across(all_of(bulk_density_columns)),
                na.rm = TRUE)),
      NA)) %>%
  ungroup() %>%
  mutate(bulk_density = round(bulk_density)) %>%
  relocate(any_of(c("bulk_density_min", "bulk_density_max",
                    "bulk_density_source")),
           .after = "bulk_density") %>%
  select(-bulk_density_survey_year,
         -contains("pfh_horizon_bulk_dens"),
         -contains("swc_bulk_dens"))


## Coarse fragments ----

coarse_fragment_vol_columns <-
  df %>%
    select(contains("coarse_frag"), -contains("survey_year")) %>%
    names

df <- df %>%
  mutate(
    coarse_fragment_vol_source = case_when(
      !is.na(coarse_fragment_vol) ~
        paste0("som (", .data$coarse_fragment_vol_survey_year, ")"),
      !is.na(pfh_coarse_fragment_vol_converted) ~
        paste0("pfh_weight (",
               .data$pfh_coarse_fragment_vol_converted_survey_year, ")"),
      !is.na(pfh_coarse_fragment_vol_avg) ~
        paste0("pfh_code_vol (",
               .data$pfh_coarse_fragment_vol_avg_survey_year, ")")),
    coarse_fragment_vol = coalesce(
      coarse_fragment_vol,
      pfh_coarse_fragment_vol_converted,
      pfh_coarse_fragment_vol_avg)) %>%
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
  mutate(coarse_fragment_vol = round(coarse_fragment_vol, 1)) %>%
  relocate(any_of(c("coarse_fragment_vol_min",
                    "coarse_fragment_vol_max",
                    "coarse_fragment_vol_source")),
           .after = "coarse_fragment_vol") %>%
  select(-coarse_fragment_vol_survey_year,
         -contains("pfh_coarse_fragment"))


## Clay ----

part_size_clay_columns <-
  df %>%
    select(contains("clay"), -contains("survey_year")) %>%
    names

df <- df %>%
  mutate(
    part_size_clay_source = case_when(
      !is.na(part_size_clay) ~
        paste0("som (", .data$texture_survey_year, ")"),
      !is.na(pfh_horizon_clay) ~
        paste0("pfh (",
               .data$pfh_texture_survey_year, ")")),
    part_size_clay =  coalesce(
      part_size_clay,
      pfh_horizon_clay)) %>%
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
  mutate(part_size_clay = round(part_size_clay, 1)) %>%
  relocate(any_of(c("part_size_clay_min",
                    "part_size_clay_max",
                    "part_size_clay_source")),
           .after = "part_size_clay") %>%
  select(-contains("pfh_horizon_clay"))


## Silt ----

part_size_silt_columns <-
  df %>%
  select(contains("silt"), -contains("survey_year")) %>%
  names

df <- df %>%
  mutate(
    part_size_silt_source = case_when(
      !is.na(part_size_silt) ~
        paste0("som (", .data$texture_survey_year, ")"),
      !is.na(pfh_horizon_silt) ~
        paste0("pfh (",
               .data$pfh_texture_survey_year, ")")),
    part_size_silt =  coalesce(
      part_size_silt,
      pfh_horizon_silt)) %>%
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
  mutate(part_size_silt = round(part_size_silt, 1)) %>%
  relocate(any_of(c("part_size_silt_min",
                    "part_size_silt_max",
                    "part_size_silt_source")),
           .after = "part_size_silt") %>%
  select(-contains("pfh_horizon_silt"))


## Sand ----

part_size_sand_columns <-
  df %>%
  select(contains("sand"), -contains("survey_year")) %>%
  names

df <- df %>%
  mutate(
    part_size_sand_source = case_when(
      !is.na(part_size_sand) ~
        paste0("som (", .data$texture_survey_year, ")"),
      !is.na(pfh_horizon_sand) ~
        paste0("pfh (",
               .data$pfh_texture_survey_year, ")")),
    part_size_sand =  coalesce(
      part_size_sand,
      pfh_horizon_sand)) %>%
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
  mutate(part_size_sand = round(part_size_sand, 1)) %>%
  relocate(any_of(c("part_size_sand_min",
                    "part_size_sand_max",
                    "part_size_sand_source")),
           .after = "part_size_sand") %>%
  select(-contains("pfh_horizon_sand"),
         -contains("texture_survey_year"))


## Organic layer weight ----

df <- df %>%
  mutate(
    layer_thickness =
      ifelse(!is.na(.data$layer_limit_superior) &
               !is.na(.data$layer_limit_inferior) &
               (.data$layer_limit_superior !=
                  .data$layer_limit_inferior),
             abs(.data$layer_limit_superior -
                   .data$layer_limit_inferior),
             NA_real_),
    # Derive organic layer weight from bulk density
    organic_layer_weight_bd = ifelse(
      !is.na(bulk_density) &
        !is.na(layer_thickness) &
        layer_type %in% c("peat", "forest_floor"),
      # kg m-2
      round(.data$bulk_density * (.data$layer_thickness * 1e-2), 2),
      NA_real_),
    organic_layer_weight_source = ifelse(
      layer_type %in% c("peat", "forest_floor"),
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
  relocate("organic_layer_weight_source",
           .after = "organic_layer_weight") %>%
  select(-organic_layer_weight_bd,
         -layer_thickness)








# Apply pedotransfer function ----

source("./src/functions/bulk_density_ptf.R")

df <- df %>%
  mutate(bd_ptf = case_when(
    # Mineral layers: ptf
    layer_type == "mineral" ~ round(bd_ptf(.data$organic_carbon_total)),
    # Peat layers: bd_peat
    layer_type == "peat" ~ round(bd_peat),
    # Forest floor layers: bd_ff
    layer_type == "forest_floor" ~ round(bd_ff),
    .default = NA_integer_)) %>%
  # Bulk density: combine columns
  mutate(
    bulk_density_source = case_when(
      !is.na(bulk_density) ~ bulk_density_source,
      !is.na(bd_ptf) ~ "PTF",
      .default = NA_character_),
    bulk_density_min = case_when(
      !is.na(bulk_density) ~ bulk_density_min,
      !is.na(bd_ptf) ~ bd_ptf_lower(bd_ptf),
      .default = NA_integer_),
    bulk_density_max = case_when(
      !is.na(bulk_density) ~ bulk_density_max,
      !is.na(bd_ptf) ~ bd_ptf_upper(bd_ptf),
      .default = NA_integer_),
    bulk_density = coalesce(bulk_density,
                            bd_ptf)) %>%
  select(-bd_ptf)




# Gap-fill data in empty layers ----
# (by taking the information from the layer above)

col_names <- grep("bulk_density|coarse_fragment_vol|part_size|organic",
                  names(df),
                  value = TRUE)

df <- df %>%
  relocate(obs, .after = layer_limit_inferior) %>%
  mutate(all_na = if_all(.cols = all_of(col_names), ~is.na(.x))) %>%
  ungroup()

# Some Hungarian plots contain no data whatsoever. Remove them.

empty_plots <- df %>%
  filter(layer_type != "forest_floor") %>%
  group_by(plot_id) %>%
  reframe(all_na = all(all_na)) %>%
  filter(all_na == TRUE) %>%
  pull(plot_id)

df <- df %>%
  filter(!plot_id %in% empty_plots) %>%
  mutate(
    obs = ifelse(
      obs == TRUE &
        all_na == TRUE,
      FALSE,
      obs)) %>%
  select(-all_na)


# Row indices of records without any data and/or extra layers at the bottom

ind_added_layers <- as.numeric(which(df$obs == FALSE))

for (i in ind_added_layers) {

  # Select columns with names containing specified substrings
  selected_cols <- df[i, col_names]

  # If everything is empty
  # (e.g. nothing has been gap-filled based on "so_pfh" or "sw_swc"
  # in the extra layers at the bottom),
  # use data from the layer above

  if (all(is.na(selected_cols))) {

    assertthat::assert_that(
      df$layer_number[i] > 1
    )

    df_i_source <- df %>%
      filter(plot_id == df$plot_id[i]) %>%
      filter(layer_number == df$layer_number[i] - 1)

    for (col in col_names) {

      df[i, col] <- df_i_source[1, col]
    }
  }

  # Else, if anything has been gap-filled (i.e. in the extra layers at
  # the bottom):
  # set "obs" to TRUE

  if (any(!is.na(selected_cols))) {

    df$obs[i] <- TRUE
  }

}


# Add extra columns

df <- df %>%
  left_join(data_availability_so %>%
              select(plot_id,
                     code_country, partner_code, code_plot, survey_years),
            by = "plot_id") %>%
  relocate(any_of(c("code_country", "partner_code",
                    "code_plot", "survey_years")),
           .after = plot_id)


# Remove plots without data in so_strat

so_strat <- so_strat %>%
  filter(plot_id %in% unique(df$plot_id))





# Export ----

write.table(df,
            file = paste0("./output/specific_esb_requests/",
                          as.character(format(Sys.Date(), format = "%Y%m%d")),
                          "_so_data_for_water_budget_modelling.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

write.table(so_strat,
            file = paste0("./output/specific_esb_requests/",
                          as.character(format(Sys.Date(), format = "%Y%m%d")),
                          "_so_strat.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")


# Number of plots with any information on the requested parameters

df %>%
  group_by(plot_id) %>%
  reframe(any_bd = any(!is.na(bulk_density)),
          any_cf = any(!is.na(coarse_fragment_vol)),
          any_texture = any(!is.na(part_size_clay) &
                           !is.na(part_size_silt) &
                           !is.na(part_size_sand)),
          any_texture_class = any(!is.na(texture_class)),
          any_toc = any(!is.na(organic_carbon_total)),
          any_olw = (!any(layer_type == "forest_floor")) |
            any(!is.na(organic_layer_weight))) %>%
  mutate(all_known = (any_bd &
                        any_cf &
                        any_texture &
                        any_toc &
                        any_olw)) %>%
  select(-plot_id) %>%
  summarise(across(everything(), sum, na.rm = TRUE))




