

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
          require("assertthat"),
          require("soiltexture"))


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
# This contains the effective soil depths. Effective soil depths equal to 999
# refer to plots that are either known to be deep (999 reported by partner)
# or to plots without information (direct or indirect) on the soil depth.

source("./src/functions/get_stratifiers.R")

so_strat <- get_stratifiers("LII")







# Reorganise sequence and ranges of depth layers ----

# Principles:
# - 80 cm and the eff_soil_depth should always be located at the boundary
#   of/between layer(s). This facilitates the gap-filling (with assumptions
#   valid below 80 cm or eff_soil_depth)
# - If "so_pfh" (pfh_plot) has observations below the lowest observations
#   of "so_som" (som_plot), add those at the bottom
# - Add layers covering the profile until a depth of 200 cm.


df <- som_plot %>%
  mutate(
    # To indicate layers located above the effective soil depth
    above_rock = TRUE,
    # To indicate layers that were included in any of the original datasets
    # ("so_som", "so_pfh", "sw_swc")
    obs = TRUE,
    # Observations below 200 cm to be removed
    to_remove = FALSE)

# Here, we will insert the reorganised rows for each plot
df_reorganised <- NULL


for (i in seq_along(unique(df$plot_id))) {

  plot_id_i <- unique(df$plot_id)[i]

  soil_depth_i <- so_strat %>%
    filter(plot_id == plot_id_i) %>%
    pull(eff_soil_depth)

  # If no information on soil depth, assume that the lowest layer
  # should go until 200 cm (lower depth)

  if (is.na(soil_depth_i)) {
    soil_depth_i <- 200
  }

  df_sub <- df %>%
    filter(plot_id == plot_id_i)

  assertthat::assert_that(any(!is.na(df_sub$layer_limit_inferior)))

  # Identify the lowest layer at this point

  df_sub_lowest <- df_sub %>%
    filter(layer_number == max(df_sub$layer_number))

  ## Check if there are any deeper observations available in "pfh_plot" ----

  pfh_sub <- pfh_plot %>%
    filter(plot_id == plot_id_i) %>%
    filter(!is.na(layer_limit_inferior) &
             !is.na(layer_limit_superior) &
             layer_limit_inferior >
             max(df_sub$layer_limit_inferior, na.rm = TRUE))

  # If so, add them at the bottom.
  # At this stage, it is sufficient to just add the layers along with
  # their layer limits and code_layer.
  # The actual data will be filled later on.

  if (nrow(pfh_sub) > 0) {

    # To add any extra records (layers)

    extra_rows <- NULL


    # Add another layer for each of those deep layers from "pfh_plot"

    for (i in seq_len(nrow(pfh_sub))) {

      extra_row_i <- df_sub_lowest %>%
        mutate(layer_number = layer_number + i,
               layer_limit_superior = ifelse(
                 i == 1,
                 # Use the lower limit of the currently lowest layer
                 layer_limit_inferior,
                 # Else, take over the layer limits from pfh_plot
                 pfh_sub$layer_limit_superior[i]),
               layer_limit_inferior = pfh_sub$layer_limit_inferior[i],
               layer_type = pfh_sub$layer_type[i],
               code_layer = pfh_sub$code_layer[i]) %>%
        mutate_at(vars(any_of(contains(c("bulk_density",
                                         "coarse_fragment_vol",
                                         "part_size",
                                         "texture")))), ~NA)

      assertthat::assert_that(
        extra_row_i$layer_limit_inferior > extra_row_i$layer_limit_superior)

      extra_rows <- bind_rows(extra_rows,
                              extra_row_i)

    } # End of for loop along deeper layers in pfh_plot

    # Add those extra rows to the profile and reset

    df_sub <- bind_rows(df_sub,
                        extra_rows)

    # Reset extra_rows

    extra_rows <- NULL

  } # End of "if any deeper layers in pfh_plot"



  ## Check if 80 cm is located in the middle of the layer ----

  # If so, that layer needs to be split into two layers,
  # because below 80 cm, we can use different gap-filling rules
  # (due to different assumptions)
  # If 80 cm is equal to any of the boundaries or if it is below the lowest
  # observation, there is no problem.

  df_sub_80 <- df_sub %>%
    filter(layer_limit_superior < 80 &
             layer_limit_inferior > 80)

  # If so, split the layer into two:

  if (nrow(df_sub_80) > 0) {

    # Should be one layer
    assertthat::assert_that(nrow(df_sub_80) == 1)

    ind <- which(df_sub$code_layer == df_sub_80$code_layer &
                   df_sub$layer_limit_superior ==
                   df_sub_80$layer_limit_superior)

    # This should become the extra layer with 80 cm as upper limit

    df_sub_80 <- df_sub_80 %>%
      mutate(
        layer_limit_superior = 80,
        layer_number = layer_number + 1,
        code_layer = paste0(code_layer, "X"))

    # Change the original layer so that its lower layer limit becomes 80 too.

    df_sub$layer_limit_inferior[ind] <- 80

    # If there are any layers below it, change their layer numbers

    if (ind < nrow(df_sub)) {

      for (i in seq(ind + 1, nrow(df_sub), by = 1)) {

        df_sub$layer_number[i] <- df_sub$layer_number[i] + 1

      }
    }

    # Add the extra layer and rearrange

    df_sub <- bind_rows(df_sub,
                        df_sub_80) %>%
      arrange(layer_number)

  } # End of "if 80 is in the middle of any layer"




  # Identify at which depth the eff_soil_depth is located,
  # as compared to the depth range with observations.
  # This matters because below the eff_soil_depth we can gap-fill
  # the coarse fragments content assuming 90 %.

  # These are the different options:

  # - eff_soil_depth <= lowest obs:
  #   · Either in the middle of a layer → split into two
  #   · Or at the transition of a layer or at lowest_obs
  #   Check if 80 cm is below lowest obs. If so, add a layer until 80 cm
  #   (obs = FALSE)

  # - eff_soil_depth > lowest obs:
  #   · Either with 80 cm <= lowest obs
  #     → add layer until eff_soil_depth (max. 200; obs = FALSE)
  #   · Or with 80 cm between lowest obs and eff_soil_depth
  #     → add layer until 80 + another layer from 80 to eff_soil_depth
  #       (obs = FALSE; max. 200)
  #   · Or with 80 cm below eff_soil_depth
  #     → add layer until eff_soil_depth +
  #       another layer from eff_soil_depth until 80 (obs = FALSE)




  ## Check if eff_soil_depth <= lowest obs ----

  if (soil_depth_i <= max(df_sub$layer_limit_inferior, na.rm = TRUE)) {

    # If eff_soil_depth is in the middle of a layer

    df_sub_eff_soil_depth <- df_sub %>%
      filter(layer_limit_superior < soil_depth_i &
               layer_limit_inferior > soil_depth_i)

    # If so, split the layer into two:

    if (nrow(df_sub_eff_soil_depth) > 0) {

      # Should be one layer
      assertthat::assert_that(nrow(df_sub_eff_soil_depth) == 1)

      ind <- which(df_sub$code_layer == df_sub_eff_soil_depth$code_layer &
                     df_sub$layer_limit_superior ==
                     df_sub_eff_soil_depth$layer_limit_superior)

      # This should become the extra layer with eff_soil_depth as upper limit

      df_sub_eff_soil_depth <- df_sub_eff_soil_depth %>%
        mutate(
          layer_limit_superior = soil_depth_i,
          layer_number = layer_number + 1,
          code_layer = paste0(code_layer, "Y"),
          above_rock = FALSE)

      # Change the original layer so that its lower layer limit becomes
      # eff_soil_depth.

      df_sub$layer_limit_inferior[ind] <- soil_depth_i

      # If there are any layers below it, change their layer numbers

      if (ind < nrow(df_sub)) {

        for (i in seq(ind + 1, nrow(df_sub), by = 1)) {

          df_sub$layer_number[i] <- df_sub$layer_number[i] + 1

        }
      }

      # Add the extra layer and rearrange

      df_sub <- bind_rows(df_sub,
                          df_sub_eff_soil_depth) %>%
        arrange(layer_number)



    } # End of "if eff_soil_depth is in the middle of a layer"




    #   Check if 80 cm is below lowest obs. If so, add a layer until 80 cm
    #   (obs = FALSE)

    if (80 > max(df_sub$layer_limit_inferior, na.rm = TRUE)) {

      # Make another layer until 200 cm and add it

      df_sub_lowest <- df_sub %>%
        filter(layer_number == max(df_sub$layer_number))

      extra_row <- df_sub_lowest %>%
        mutate(layer_number = layer_number + 1,
               layer_limit_superior = layer_limit_inferior,
               layer_limit_inferior = 80,
               code_layer = "X",
               obs = FALSE) %>%
        mutate_at(vars(any_of(contains(c("bulk_density",
                                         "coarse_fragment_vol",
                                         "part_size",
                                         "texture")))), ~NA)

      df_sub <- bind_rows(df_sub,
                          extra_row) %>%
        arrange(layer_number)

    } # End of "if 80 cm is below lowest obs"

  } else # End of "if eff_soil_depth <= lowest obs"




    ## Check if eff_soil_depth > lowest obs ----

    if (soil_depth_i > max(df_sub$layer_limit_inferior, na.rm = TRUE)) {

    #   · Either with 80 cm <= lowest obs
    #     → add layer from lowest obs until eff_soil_depth
    #       (max. 200; obs = FALSE)

    if (80 <= max(df_sub$layer_limit_inferior, na.rm = TRUE) ||
        80 == soil_depth_i) {

      df_sub_lowest <- df_sub %>%
        filter(layer_number == max(df_sub$layer_number))

      extra_row <- df_sub_lowest %>%
        mutate(layer_number = layer_number + 1,
               layer_limit_superior = layer_limit_inferior,
               # Maximum 200 cm
               layer_limit_inferior = min(c(soil_depth_i, 200)),
               code_layer = "X",
               obs = FALSE) %>%
        mutate_at(vars(any_of(contains(c("bulk_density",
                                         "coarse_fragment_vol",
                                         "part_size",
                                         "texture")))), ~NA)

      df_sub <- bind_rows(df_sub,
                          extra_row) %>%
        arrange(layer_number)


    } else

      #   · Or with 80 cm between lowest obs and eff_soil_depth
      #     → add layer from lowest obs until 80 (obs = FALSE) +
      #       another layer from 80 to eff_soil_depth (obs = FALSE; max. 200)

      if (80 > max(df_sub$layer_limit_inferior, na.rm = TRUE) &&
          80 < soil_depth_i) {

        df_sub_lowest <- df_sub %>%
          filter(layer_number == max(df_sub$layer_number))

        extra_rows <- bind_rows(
          # From lowest obs until 80
          df_sub_lowest %>%
            mutate(layer_number = layer_number + 1,
                   layer_limit_superior = layer_limit_inferior,
                   # Maximum 200 cm
                   layer_limit_inferior = 80,
                   code_layer = "X",
                   obs = FALSE),
          # From 80 until eff_soil_depth
          df_sub_lowest %>%
            mutate(layer_number = layer_number + 2,
                   layer_limit_superior = 80,
                   # Maximum 200 cm
                   layer_limit_inferior = min(c(soil_depth_i, 200)),
                   code_layer = "X",
                   obs = FALSE)) %>%
          mutate_at(vars(any_of(contains(c("bulk_density",
                                           "coarse_fragment_vol",
                                           "part_size",
                                           "texture")))), ~NA)


        df_sub <- bind_rows(df_sub,
                            extra_rows) %>%
          arrange(layer_number)



      } else

        #   · Or with 80 cm below eff_soil_depth
        #     → add layer until eff_soil_depth (obs = FALSE) +
        #       another layer from eff_soil_depth until 80 (obs = FALSE)

        if (80 > soil_depth_i) {

          df_sub_lowest <- df_sub %>%
            filter(layer_number == max(df_sub$layer_number))

          extra_rows <- bind_rows(
            # From lowest obs until eff_soil_depth
            df_sub_lowest %>%
              mutate(layer_number = layer_number + 1,
                     layer_limit_superior = layer_limit_inferior,
                     # Maximum 200 cm
                     layer_limit_inferior = soil_depth_i,
                     code_layer = "X",
                     obs = FALSE),
            # From eff_soil_depth until 80
            df_sub_lowest %>%
              mutate(layer_number = layer_number + 2,
                     layer_limit_superior = soil_depth_i,
                     # Maximum 200 cm
                     layer_limit_inferior = 80,
                     code_layer = "X",
                     obs = FALSE)) %>%
            mutate_at(vars(any_of(contains(c("bulk_density",
                                             "coarse_fragment_vol",
                                             "part_size",
                                             "texture")))), ~NA)

          df_sub <- bind_rows(df_sub,
                              extra_rows) %>%
            arrange(layer_number)

        }


  } # End of "if eff_soil_depth > lowest obs"





  # If this does not go until 200 cm yet:

  ## Add layer until 200 if needed ----

  if (max(df_sub$layer_limit_inferior, na.rm = TRUE) < 200) {

    # Make another layer until 200 cm and add it

    df_sub_lowest <- df_sub %>%
      filter(layer_number == max(df_sub$layer_number))

    extra_row <- df_sub_lowest %>%
      mutate(layer_number = layer_number + 1,
             layer_limit_superior = layer_limit_inferior,
             layer_limit_inferior = 200,
             code_layer = "X",
             obs = FALSE) %>%
      mutate_at(vars(any_of(contains(c("bulk_density",
                                       "coarse_fragment_vol",
                                       "part_size",
                                       "texture")))), ~NA)

    df_sub <- bind_rows(df_sub,
                        extra_row) %>%
      arrange(layer_number)

  } # End of "if layer until 200 cm needs to be added"


  # Flag layers below eff_soil_depth

  vec <- which(df_sub$layer_limit_superior >= soil_depth_i)

  if (!identical(vec, integer(0))) {

    df_sub$above_rock[vec] <- FALSE

  }





  ## Remove any layers that fall completely below 200 cm ----

  ind <- which(!is.na(df_sub$layer_limit_superior) &
                 df_sub$layer_limit_superior >= 200)

  if (!identical(ind, integer(0))) {

    df_sub$to_remove[ind] <- TRUE

    df_sub <- df_sub %>%
      filter(to_remove == FALSE)
  }

  # If 200 cm falls within the depth range of the lowest
  # layer, make sure the inferior layer limit of that layer equals 200

  df_sub_lowest <- df_sub %>%
    filter(layer_number == max(df_sub$layer_number))

  if (200 <= df_sub_lowest$layer_limit_inferior &&
      200 >= df_sub_lowest$layer_limit_superior) {

    ind <- which(df_sub$layer_number == max(df_sub$layer_number))
    df_sub$layer_limit_inferior[ind] <- 200

  }

  # Add df_sub to df_reorganised

  df_reorganised <- bind_rows(df_reorganised,
                              df_sub)

} # End of "for" loop over plot_ids



df <- df_reorganised %>%
  arrange(country,
          plot_id,
          layer_number) %>%
  select(-to_remove)


assertthat::assert_that(
  all(is.na(df$layer_limit_inferior) |
        is.na(df$layer_limit_superior) |
        df$layer_limit_inferior > df$layer_limit_superior))










# Add physical parameters from "pfh" and "swc" to "som" ----

source("./src/functions/depth_join.R")

# This function adds the data from "pfh_plot" and "swc_plot", but already
# adjusted to the depth ranges of df (using weighted averages etc)

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
                 # It only takes data from the most recent survey_year
                 # reported, corresponding with the target depth range
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




# Add texture class ----

# ICP Forests uses the texture classes by FAO-USDA (with 63 µm as the
# boundary between sand and silt), but the database also contains
# some old classes, i.e. the classes used by HYPRES
# and the European Soil Database

df <- depth_join(df1 = df,
                 df2 = so_som,
                 parameters = "code_texture_class",
                 prefix_parameters_in_df1 = "som_")

df <- depth_join(df1 = df,
                 df2 = so_pfh,
                 parameters = "code_horizon_texture_class",
                 prefix_parameters_in_df1 = "pfh_")

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


# Assert that all textures are either known or unknown, and sum up to 100 %

df <- df %>%
  rowwise() %>%
  mutate(
    # Add a column that indicates TRUE if any column is non-NA and
    # any column is NA (since we basically don't want and don't expect this)
    inconsistent_pres =
      any(!is.na(c_across(c(part_size_clay, part_size_silt, part_size_sand)))) &
      any(is.na(c_across(c(part_size_clay, part_size_silt, part_size_sand)))),
    # Sum of texture fractions
    sum_texture = ifelse(
      is.na(part_size_clay),
      NA_real_,
      sum(c_across(c(part_size_clay, part_size_silt, part_size_sand)),
          na.rm = TRUE))) %>%
  ungroup()

# Assert that there is no "inconsistent presence" of texture data

assertthat::assert_that(all(df$inconsistent_pres == FALSE))

# Assert that all textures sum up to roughly 100 %

assertthat::assert_that(all(is.na(df$sum_texture) |
                             (round(df$sum_texture, 1) >= 99.8 &
                                round(df$sum_texture, 1) <= 100.2)))

df <- df %>%
  select(-inconsistent_pres,
         -sum_texture)




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







# At this point, the dataset contains only direct observations.
# Indicate which records do not have any data at this point.

col_names <- grep("bulk_density|coarse_fragment_vol|part_size|organic",
                  names(df),
                  value = TRUE)

df <- df %>%
  relocate(obs, .after = layer_limit_inferior) %>%
  mutate(all_na = if_all(.cols = all_of(col_names), ~is.na(.x))) %>%
  ungroup()

# Some Hungarian plots contain no data whatsoever. Flag them but
# do not remove them yet.

empty_plots <- df %>%
  filter(layer_type != "forest_floor") %>%
  filter(layer_limit_superior < 80) %>%
  group_by(plot_id) %>%
  reframe(all_na = all(all_na)) %>%
  filter(all_na == TRUE) %>%
  pull(plot_id)

df <- df %>%
  mutate(
    all_na = case_when(
      plot_id %in% empty_plots ~ "Plot without original observations",
      all_na == TRUE ~ "Record without original observations",
      TRUE ~ NA_character_))






# Gap-fill thicknesses of forest floor layers ----

# Retrieve the representative bulk density for forest floor layers

source("./src/functions/get_parameter_stats.R")

bd_ff <- get_parameter_stats(parameter = "bulk_density",
                             mode = "stat",
                             layer_type = "forest_floor_excl_ol")

bd_ff <- as.numeric(bd_ff[which(names(bd_ff) == "Mean")])

# Bulk density of forest floor (excluding OL): 120 kg m-3

bd_ol <- get_parameter_stats(parameter = "bulk_density",
                             mode = "stat",
                             layer_type = "ol")

bd_ol <- as.numeric(bd_ol[which(names(bd_ol) == "Mean")])


df <- df %>%
  mutate(
    # This column indicates whether forest floor thickness are estimated
    # using the organic_layer_weight and representative bulk_density
    ff_thickness_est = FALSE)


# Evaluate all profiles

for (i in seq_along(unique(df$plot_id))) {

  plot_id_i <- unique(df$plot_id)[i]

  df_sub <- df %>%
    filter(plot_id == plot_id_i)

  # If all layer limits are known, skip and go to the next plot

  if (all(!is.na(df_sub$layer_limit_superior))) {
    next
  }

  # Row indices of plots with missing forest floor thicknesses in df

  vec <- which(df$plot_id == plot_id_i &
                 is.na(df$layer_limit_superior))

  # Assert that the layers with missing depths are from the forest floor

  assertthat::assert_that(
    all(df$layer_type[vec] == "forest_floor"))

  # Assert that the row indices are sorted

  assertthat::assert_that(
    all(vec == sort(vec)))

  # Assert that any differences between row indices are 1 (i.e. they are
  # consecutive) if more than one layer

  assertthat::assert_that(
    length(vec) == 1 ||
      all(diff(vec) == 1))

  # Assert that the layer below still belongs to the same profile

  assertthat::assert_that(
    df$plot_id[max(vec) + 1] == plot_id_i)

  # Evaluate for each of the layers (from bottom to top)

  for (j in sort(vec, decreasing = TRUE)) {

    # If the layer below is a forest floor layer without
    # known organic_layer_weight, we cannot estimate the layer limits.

    if (df$layer_type[j + 1] == "forest_floor" &&
        (is.na(df$organic_layer_weight[j + 1]) ||
         is.na(df$layer_limit_superior[j + 1]))) {
      next
    }


    # If layer_limit_inferior is not known, this can be taken from the
    # upper limit of the layer below, which should be known

    if (is.na(df$layer_limit_inferior[j])) {

      df$layer_limit_inferior[j] <- df$layer_limit_superior[j + 1]

      # Assert that the layer limit is known now

      assertthat::assert_that(
        !is.na(df$layer_limit_inferior[j]))

    }


    # If the organic_layer_weight is also unknown, we cannot estimate
    # the layer thickness and we can basically not use the record.

    # At the moment, such records are just left in the dataset,
    # but they can easily be removed by filtering for known layer limits.

    if (is.na(df$organic_layer_weight[j])) {
      next
    }

    # Check the name of the layer. To estimate the thickness of the layer,
    # use the general forest floor bulk density value unless the layer is
    # names "OL". In that case, use the bulk density for "OL".

    # It may make sense to remove any forest floor layers containing the
    # letter "L" for the water budget modelling, though.

    if (df$code_layer[j] != "OL") {

      df$layer_limit_superior[j] <- round(
        df$layer_limit_inferior[j] -
        # Thickness of the layer
        df$organic_layer_weight[j] / (bd_ff * 1E-2), 1)

    } else

      if (df$code_layer[j] == "OL") {

        df$layer_limit_superior[j] <- round(
          df$layer_limit_inferior[j] -
            # Thickness of the layer
            df$organic_layer_weight[j] / (bd_ol * 1E-2), 1)

      }

    # Flag the layer for having an estimated forest floor thickness

    df$ff_thickness_est[j] <- TRUE

  } # End of for loop along forest floor layers of the given profile

} # End of for loop along profiles






# Gap-fill coarse fragments below eff_soil_depth ----

df <- df %>%
  mutate(
    coarse_fragment_vol_source = ifelse(
      is.na(coarse_fragment_vol) &
        above_rock == FALSE,
      "below eff_soil_depth",
      coarse_fragment_vol_source),
    coarse_fragment_vol_min = ifelse(
      is.na(coarse_fragment_vol) &
        above_rock == FALSE,
      80,
      coarse_fragment_vol_min),
    coarse_fragment_vol_max = ifelse(
      is.na(coarse_fragment_vol) &
        above_rock == FALSE,
      100,
      coarse_fragment_vol_max),
    coarse_fragment_vol = ifelse(
      is.na(coarse_fragment_vol) &
        above_rock == FALSE,
      90,
      coarse_fragment_vol))






# Gap-fill total organic carbon below 80 cm ----

# Assumption: TOC is consistently low across plots below a depth of 80 cm.
# Therefore, we can use one representative (low) value for TOC across Europe
# based on observations from all Level I plots below 80 cm.
# Applying the pedotransfer function (with TOC as input) to this value is
# assumed to be a better approach to estimate bulk densities below 80 cm
# than "LOCF" (last observation carried forward), since bulk densities
# probably increase with an increasing depth.

# Retrieve additional subsoil TOC data on ICP Forests LI shared by Denmark

denmark_toc <-
  openxlsx::read.xlsx(paste0("data/additional_data/",
                             "national_coauthor_carbon_stocks/Denmark/",
                             "Denmark_BIOSOIL Level I soil C data from 2007 ",
                             "and 20187Mar2024.xlsx"),
                      sheet = 1) %>%
  as_tibble %>%
  rename(organic_carbon_total = "C.concentration.(%)",
         bulk_density = "Bulk.density",
         soil_mass = "Soil.dry.weight.(Mg.ha-1)") %>%
  # No NAs in data
  filter(!is.na(organic_carbon_total)) %>%
  # Convert to g kg-1
  mutate(organic_carbon_total = 1E1 * organic_carbon_total) %>%
  mutate(plot_id = paste0("8_", Plot)) %>%
  filter(Layer == "80-100 cm") %>%
  mutate(profile_id = plot_id,
         layer_limit_superior = 80,
         layer_limit_inferior = 100,
         layer_type = ifelse(
           organic_carbon_total < 200,
           "mineral",
           "peat"),
         bulk_density = 1E3 * bulk_density,
         fine_earth_frac =
           soil_mass / (1E-3 * 1E4 * 0.2 * bulk_density),
         coarse_fragment_vol = ifelse(
           fine_earth_frac <= 1,
           100 * (1 - fine_earth_frac),
           0)) %>%
  # Data for 50 LI plots
  select(plot_id, profile_id,
         layer_limit_superior, layer_limit_inferior, layer_type,
         bulk_density, coarse_fragment_vol, organic_carbon_total)



# Combine data with deep ICP Forests data from the systematic Level I network
# (so that the estimated value is theoretically representative for Europe)
# Aggregate the data per plot so that each plot contributes to the same
# extent to the representative value

subsoil_toc <- bind_rows(
  s1_som %>%
    select(plot_id, profile_id,
           layer_limit_superior, layer_limit_inferior, layer_type,
           bulk_density, coarse_fragment_vol, organic_carbon_total),
  s1_pfh %>%
    rename(layer_limit_superior = horizon_limit_up) %>%
    rename(layer_limit_inferior = horizon_limit_low) %>%
    rename(organic_carbon_total = horizon_c_organic_total) %>%
    select(plot_id, profile_id,
           layer_limit_superior, layer_limit_inferior, layer_type,
           bulk_density, coarse_fragment_vol, organic_carbon_total),
  denmark_toc) %>%
  # Filter for the wanted depths
  filter(!is.na(layer_limit_superior) &
           !is.na(layer_limit_inferior) &
           layer_limit_superior >= 80 &
           layer_limit_inferior > 80 &
           layer_limit_inferior > layer_limit_superior) %>%
  # Assume that the subsoil (>= 80 cm) will mostly be mineral
  filter(layer_type == "mineral") %>%
  mutate(coarse_fragment_vol_frac =
           ifelse(is.na(.data$coarse_fragment_vol),
                  0,
                  .data$coarse_fragment_vol / 100)) %>%
  mutate(layer_thickness = ifelse(!is.na(.data$layer_limit_superior) &
                                    !is.na(.data$layer_limit_inferior) &
                                    (.data$layer_limit_superior !=
                                       .data$layer_limit_inferior),
                                  abs(.data$layer_limit_superior -
                                        .data$layer_limit_inferior),
                                  NA_real_)) %>%
  filter(!is.na(organic_carbon_total)) %>%
  group_by(profile_id, plot_id) %>%
  mutate(
    # Use the same weight for bulk density in case of any gap
    bulk_density = ifelse(
      any(is.na(bulk_density)),
      1,
      bulk_density)) %>%
  ungroup() %>%
  arrange(plot_id, profile_id) %>%
  rowwise() %>%
  mutate(
    # To use as a weighting factor; only the relative values between any layers
    # within a profile matter
    soil_mass =
      bulk_density * (1 - coarse_fragment_vol_frac) * layer_thickness) %>%
  ungroup() %>%
  group_by(profile_id, plot_id) %>%
  reframe(
    organic_carbon_total = weighted.mean(organic_carbon_total,
                                         w = soil_mass,
                                         na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate average per plot
  group_by(plot_id) %>%
  reframe(organic_carbon_total = mean(organic_carbon_total,
                                      na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(organic_carbon_total) %>%
  pull(organic_carbon_total)

# Representative TOC below 80 cm (g kg-1)
subsoil_toc_repr <- mean(subsoil_toc) # 3.52 g C kg-1


# Determine the PTF-estimated bulk density based on this TOC

source("./src/functions/bulk_density_ptf.R")

# Representative bulk density
subsoil_bd_repr <- bd_ptf(subsoil_toc_repr) # 1359 kg m-3

# By way of comparison, the median bulk_density based on the subsoil dataset
# above is 1438 kg m-3 (95 % quantile: 1000 - 1683),
# so the estimate based on TOC seems sound.


# Add the TOC data

df <- df %>%
  mutate(
    # We can also assume that the zone below 80 cm is predominantly mineral
    # (not peat) for those records
    layer_type = ifelse(
      is.na(organic_carbon_total) &
        layer_limit_superior >= 80,
      "mineral",
      layer_type),
    organic_carbon_total_source = ifelse(
      is.na(organic_carbon_total) &
        layer_limit_superior >= 80,
      "fixed value subsoil",
      organic_carbon_total_source),
    organic_carbon_total = ifelse(
      is.na(organic_carbon_total) &
        layer_limit_superior >= 80,
      round(subsoil_toc_repr, 1),
      organic_carbon_total))







# Gap-fill bulk densities (PTF and LOCF) ----

# Here, we will add a column with the estimates based on the pedotransfer
# function + the estimates based on LOCF (observations from layers above)

# Above 80 cm, PTF estimates can be used to gap-fill bulk densities.
# (and after that, LOCF and NOCB can be applied, which is however not done here,
# and probably not necessary for bulk density)

# Below 80 cm, we assume that the maximum of both bulk density estimates
# should be taken (because the deeper, the denser soil becomes)

# LOCF = last observation carried forward
# NOCB = next observation carried backward

df <- df %>%
  mutate(bulk_density_ptf = case_when(
    # Mineral layers: ptf
    layer_type == "mineral" ~ round(bd_ptf(.data$organic_carbon_total)),
    # Peat layers: bd_peat
    layer_type == "peat" ~ round(bd_peat),
    # Forest floor layers: bd_ff
    layer_type == "forest_floor" ~ round(bd_ff),
    .default = NA_integer_)) %>%
  # Bulk density: combine columns
  # (only for records without estimated subsoil TOC at this stage,
  # because those PTF-estimated bulk densities (not from the TOC estimate)
  # can also be used for LOCF)
  mutate(
    bulk_density_source = ifelse(
      organic_carbon_total_source != "fixed value subsoil",
      case_when(
        !is.na(bulk_density) ~ bulk_density_source,
        !is.na(bulk_density_ptf) ~ "PTF"),
      bulk_density_source),
    bulk_density_min = ifelse(
      organic_carbon_total_source != "fixed value subsoil",
      coalesce(bulk_density_min,
               bd_ptf_lower(bulk_density_ptf)),
      bulk_density_min),
    bulk_density_max = ifelse(
      organic_carbon_total_source != "fixed value subsoil",
      coalesce(bulk_density_max,
               bd_ptf_upper(bulk_density_ptf)),
      bulk_density_max),
    bulk_density = ifelse(
      organic_carbon_total_source != "fixed value subsoil",
      coalesce(bulk_density,
               bulk_density_ptf),
      bulk_density)) %>%
  # Copy the bulk density columns to be used for LOCF
  mutate(bulk_density_locf = bulk_density,
         bulk_density_locf_min = bulk_density_min,
         bulk_density_locf_max = bulk_density_max) %>%
  # Apply LOCF within each group
  group_by(plot_id) %>%
  # Ensure data is sorted by layer_number (which anyway should be fine already)
  arrange(layer_number) %>%
  # Apply LOCF (filling from above)
  fill(bulk_density_locf, .direction = "down") %>%
  fill(bulk_density_locf_min, .direction = "down") %>%
  fill(bulk_density_locf_max, .direction = "down") %>%
  ungroup() %>%
  relocate(bulk_density_locf,
           bulk_density_locf_min,
           bulk_density_locf_max,
           .after = bulk_density) %>%
  relocate(bulk_density_ptf, .after = bulk_density) %>%
  arrange(country, plot_id, layer_number) %>%
  # Now gap-fill the bulk densities for depth layers below 80 cm,
  # in which TOC was estimated as a fixed value.
  # Take the maximum bulk density based on LOCF versus PTF.
  rowwise() %>%
  mutate(
    bulk_density_source = ifelse(
      organic_carbon_total_source == "fixed value subsoil" &
        is.na(bulk_density),
      case_when(
        bulk_density_locf >= bulk_density_ptf ~ "LOCF",
        bulk_density_ptf > bulk_density_locf ~ "PTF (fixed TOC)",
        TRUE ~ bulk_density_source),
      bulk_density_source),
    # The widest possible uncertainty range
    bulk_density_min = ifelse(
      organic_carbon_total_source == "fixed value subsoil" &
        is.na(bulk_density),
      # If there is no uncertainty in the LOCF value (because only one
      # value was originally reported for a certain depth layer of a
      # certain plot),
      # it takes the uncertainty range of the PTF value,
      # which is realistic (because the uncertainty in our assumptions
      # are considerable)
      min(bulk_density_locf_min,
          bd_ptf_lower(bulk_density_ptf)),
      bulk_density_min),
    bulk_density_max = ifelse(
      organic_carbon_total_source == "fixed value subsoil" &
        is.na(bulk_density),
      max(bulk_density_locf_max,
          bd_ptf_upper(bulk_density_ptf)),
      bulk_density_max),
    bulk_density = ifelse(
      # Gap-fill bulk densities in layers below 80 cm
      organic_carbon_total_source == "fixed value subsoil" &
        is.na(bulk_density),
      # Take the maximum of the LOCF value versus the estimated bulk density
      # (using the PTF)
      max(bulk_density_locf,
          bulk_density_ptf),
      bulk_density)) %>%
  ungroup() %>%
  select(-bulk_density_ptf,
         -bulk_density_locf,
         -bulk_density_locf_min,
         -bulk_density_locf_max)















# Add texture data from European Soil Database ----

# This information can optionally be used for water budget modelling.
# Even though they are less accurate, simulations for plots without
# texture information are useful.

# The European Soil Database uses exactly the same classes as HYPRES
# (i.e. very fine, fine, medium, medium fine, coarse)

path_soil_database <-
  paste0("./data/additional_data/shapefiles/European Soil Database/",
         "SoilDB_rasters/")


legend_texture <- read.csv(paste0(path_soil_database,
                                  "texture-dictionary.csv"), sep = ";")
legend_text_chg_dep <-
  read.csv(paste0(path_soil_database,
                  "textural-change-depth-dictionary.csv"), sep = ";")

source("./src/functions/as_sf.R")
source("./src/functions/overlay_tif.R")


# Add the texture classes by doing overlays

texture_esd <- coordinates_so %>%
  as_sf %>%
  # Surface texture
  # There is also "txsrfse" and "txsubse" with the secondary texture class
  # appearing (laterally) within an STU, but this does not add any
  # information to gaps.
  overlay_tif(path_tif = paste0(path_soil_database,
                                "txsrfdo.tif"),
              # Avoid category "No information" (in that case,
              # search for the most abundant value within a radius of 2000 m,
              # or within a radius of 5000 m if 2000 meter does not help)
              values_to_avoid = 0) %>%
  # Subsoil texture
  overlay_tif(path_tif = paste0(path_soil_database,
                                "txsubdo.tif"), # or txsubdo.tif?
              values_to_avoid = 0) %>%
  # Depth of textural change
  overlay_tif(path_tif = paste0(path_soil_database,
                                "txdepchg.tif"),
              values_to_avoid = 0) %>%
  # Convert the codes into actual classes
  # Surface texture
  left_join(legend_texture %>%
              rename(txsrfdo = texture_code) %>%
              rename(tex_srf_esd = texture_esd) %>%
              select(txsrfdo, tex_srf_esd),
            by = "txsrfdo") %>%
  # Subsoil texture
  left_join(legend_texture %>%
              rename(txsubdo = texture_code) %>%
              rename(tex_sub_esd = texture_esd) %>%
              select(txsubdo, tex_sub_esd),
            by = "txsubdo") %>%
  # Average depth of textural change
  left_join(legend_text_chg_dep %>%
              rename(txdepchg = code_change) %>%
              rename(tex_chg_depth_esd = depth_cm) %>%
              select(txdepchg, tex_chg_depth_esd),
            by = "txdepchg") %>%
  st_drop_geometry() %>%
  select(plot_id, tex_srf_esd, tex_sub_esd, tex_chg_depth_esd)


# Canary Islands and Cyprus are not in European Soil Database
# Assert that all other plots received a texture class

assertthat::assert_that(
  all(texture_esd %>%
        filter(is.na(tex_srf_esd)) %>%
        pull(plot_id) %in% c("11_51", "11_52", "11_53",
                             "66_101", "66_102", "66_103", "66_104")))







# Add ESD data per layer to df

df <- df %>%
  mutate(
    texture_class_esd = NA)

# Evaluate each plot in df

for (i in seq_along(unique(df$plot_id))) {

  tex_i <- texture_esd %>%
    filter(plot_id == unique(df$plot_id)[i])

  # If unknown (e.g. Canaries and Cyprus), skip the current plot

  if (is.na(tex_i$tex_srf_esd)) {
    next
  }

  # Identify the mineral layers of the plot

  vec <- which(
    df$plot_id == unique(df$plot_id)[i] &
      df$layer_type == "mineral")

  # If the vector is empty, assert that it contains peat, and skip

  if (identical(vec, integer(0))) {

    assertthat::assert_that(
      any(df %>%
            filter(plot_id == unique(df$plot_id)[i]) %>%
            pull(layer_type) == "peat"))

    next
  }

  # Evaluate each layer

  for (j in vec) {

    # If the depth to a textural change is below the upper layer limit
    # of if the subsoil texture or the depth to textural change is unknown
    # → use dominant surface texture.
    # Else, use dominant subsurface texture

    if (is.na(tex_i$tex_chg_depth_esd) ||
        df$layer_limit_superior[j] < tex_i$tex_chg_depth_esd ||
        tex_i$tex_sub_esd == "No information") {

      df$texture_class_esd[j] <- tex_i$tex_srf_esd

    } else {

      df$texture_class_esd[j] <- tex_i$tex_sub_esd
    }

  }

}






# Intermezzo: check overlap between ESD and ICP Forests texture classes ----

# Map possible overlaps between ESD and ICPF texture systems

combi_fractions <- expand.grid(part_size_clay = seq(0, 100, by = 1),
                               part_size_silt = seq(0, 100, by = 1)) %>%
  rowwise() %>%
  mutate(sum = sum(c(part_size_clay, part_size_silt))) %>%
  ungroup() %>%
  filter(sum <= 100) %>%
  mutate(part_size_sand = 100 - sum) %>%
  select(-sum)

tx_class_overlaps <- cbind(
  combi_fractions,
  # ESD = HYPRES classification
  esd =
    TT.points.in.classes(
      tri.data = as.data.frame(combi_fractions),
      class.sys = "HYPRES.TT",
      css.names = c("part_size_clay", "part_size_silt", "part_size_sand"),
      PiC.type = "t",
      collapse = "_") %>%
    as_tibble %>%
    mutate(
      esd = case_when(
        value == "VF" ~ "Very fine",
        value == "F" ~ "Fine",
        value == "M" ~ "Medium",
        value == "MF" ~ "Medium fine",
        value == "C" ~ "Coarse")) %>%
    select(-value),
  icpf =
    # USDA = ICP Forests classification
    TT.points.in.classes(
      tri.data = as.data.frame(combi_fractions),
      class.sys = "USDA.TT",
      css.names = c("part_size_clay", "part_size_silt", "part_size_sand"),
      PiC.type = "t",
      collapse = "_") %>%
    as_tibble %>%
    mutate(
      icpf = case_when(
        value == "Cl" ~ "Clay",
        value == "SiCl" ~ "Silty clay",
        value == "SaCl" ~ "Sandy clay",
        value == "ClLo" ~ "Clay loam",
        value == "SiClLo" ~ "Silty clay loam",
        value == "SaClLo" ~ "Sandy clay loam",
        value == "Lo" ~ "Loam",
        value == "SiLo" ~ "Silty loam",
        value == "SaLo" ~ "Sandy loam",
        value == "Si" ~ "Silt",
        value == "LoSa" ~ "Loamy sand",
        value == "Sa" ~ "Sand")) %>%
    select(-value)) %>%
  filter(!is.na(esd) & !is.na(icpf)) %>%
  mutate(esd_icpf = paste0(esd, "_", icpf)) %>%
  distinct(esd_icpf) %>%
  arrange(esd_icpf) %>%
  pull(esd_icpf)


# How many of the observed ICP Forests - ESD combinations are in theory
# possible (i.e. overlap of fractions in texture triangle)?

flag_possible <- df %>%
  filter(!is.na(texture_class) & !is.na(texture_class_esd)) %>%
  filter(texture_class %in%
           (d_texture_class %>%
              filter(!grepl("-", code)) %>%
              filter(is.na(valid_to_survey_year)) %>%
              filter(code != "HD") %>%
              pull(description))) %>%
  filter(texture_class_esd %in%
           (legend_texture %>%
              filter(!texture_esd %in% c("No information",
                                         "Peat soils")) %>%
              pull(texture_esd))) %>%
  mutate(
    texture_class_esd_icpf = paste0(
      texture_class_esd, "_", texture_class),
    possible = (texture_class_esd_icpf %in% tx_class_overlaps))

nrow(flag_possible)
length(which(pull(flag_possible)))

# 961 / 1843 (i.e. 52 %) of the ESD predictions are possible in combination
# with the ICP Forests classes






# Organise final dataset ----


# Add extra columns

df <- df %>%
  left_join(data_availability_so %>%
              select(plot_id,
                     code_country, partner_code, code_plot, survey_years),
            by = "plot_id") %>%
  relocate(any_of(c("code_country", "partner_code",
                    "code_plot", "survey_years")),
           .after = plot_id) %>%
  relocate(any_of(c("obs", "above_rock", "ff_thickness_est")),
           .after = layer_limit_inferior)


# Remove plots without data in so_strat

so_strat <- so_strat %>%
  filter(plot_id %in% unique(df$plot_id))












# Export ----

so_data_for_wbm <- df
output_dir <- paste0("./output/specific_esb_requests/")
date <- as.character(format(Sys.Date(), format = "%Y%m%d"))

write.table(df,
            file = paste0(output_dir, date,
                          "_so_data_for_wbm.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")


source("./src/functions/create_attribute_catalogue.R")

create_attribute_catalogue(data_frame = "so_data_for_wbm",
                           path_to_save = paste0(output_dir,
                                                 "so_data_for_wbm_"))


write.table(so_strat,
            file = paste0(output_dir, date,
                          "_so_strat.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

create_attribute_catalogue(data_frame = "so_strat",
                           path_to_save = paste0(output_dir, "so_strat_"))


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
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))




