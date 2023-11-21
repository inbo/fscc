
# Calculate carbon stocks until 100 cm of depth
# Using a mass-preserving spline function of soil layers/horizons

# Script initiated by Bruno

# Input data: "./data/layer1_data/"
# Output data: "./output/stocks/"


# 1. Specify origin data ----
# To do: convert into a function.
# Possible origin data: s1_som, so_som, s1_pfh, so_pfh

survey_form <- "s1_som"

code_survey <- unlist(strsplit(survey_form, "_"))[1]


# Load packages and functions

stopifnot(require("tidyverse"),
          require("assertthat"),
          require("aqp"),
          require("mpspline2"))

source("./src/stock_calculations/functions/soilspline.R")
source("./src/stock_calculations/functions/calculate_stocks.R")

source("./src/functions/get_env.R")

# 2. Import data and preprocess data ----

if (!exists(survey_form)) {
  source("./src/functions/read_processed.R")
  read_processed(survey_forms = c("si", "so"), save_to_env = TRUE)
}

df <- get_env(survey_form)
prf <- get_env(paste0(code_survey, "_prf"))



# Add eff_soil_depth data

prf_agg <- prf %>%
  # Aggregate per plot_id
  group_by(plot_id) %>%
  summarise(eff_soil_depth = mean(eff_soil_depth, na.rm = TRUE))

df_working <- df %>%
  left_join(prf_agg,
            by = "plot_id") %>%
  # If the effective soil depth is deeper than 100 cm or unknown (NA):
  # assume it is 100 cm (for stocks)
  mutate(eff_soil_depth =
           ifelse(is.na(.data$eff_soil_depth) |
                          (.data$eff_soil_depth > 100),
                  100,
                  .data$eff_soil_depth)) %>%
  # If there is a layer at the bottom for which the layer limits are not
  # known (possible in "pfh"):
  # Apply the lower layer limit of the layer on top as upper layer limit
  # and a default lower layer limit of the eff_soil_depth if this is deeper
  group_by(unique_survey_repetition) %>%
  mutate(layer_number_deepest = suppressWarnings(max(.data$layer_number,
                                                     na.rm = TRUE)),
         prev_layer_limit_inferior = lag(.data$layer_limit_inferior),
         layer_limit_superior = ifelse(is.na(.data$layer_limit_superior) &
                                       (.data$layer_number ==
                                         .data$layer_number_deepest) &
                                       !is.na(.data$prev_layer_limit_inferior),
                                       .data$prev_layer_limit_inferior,
                                       .data$layer_limit_superior),
         depth_max = suppressWarnings(max(.data$layer_limit_inferior,
                                          na.rm = TRUE)),
         layer_limit_inferior = ifelse(is.na(.data$layer_limit_inferior) &
                                       !is.na(.data$eff_soil_depth) &
                                       (.data$eff_soil_depth > .data$depth_max),
                                       .data$eff_soil_depth,
                                       .data$layer_limit_inferior)) %>%
  ungroup() %>%
  select(-layer_number_deepest,
         -prev_layer_limit_inferior,
         -depth_max)

# Recode helping variables for stock calculation

df_working <- df_working %>%
  # If coarse fragments is unknown:
  # Assume it is 0
  # To do: the uncertainty associated with this assumption needs to be assessed
  mutate(coarse_fragment_vol_frac =
           ifelse(is.na(.data$coarse_fragment_vol),
                  0,
                  .data$coarse_fragment_vol / 100)) %>%
  # plot_id (code_country and code_plot) already exists
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






# 3. Below-ground layers ----

## 3.1. Derive layer-based dataset for mineral and peat layers ----

df_below_ground <- df_working %>%
  filter(.data$layer_type %in% c("mineral", "peat")) %>%
  # Filter out redundant layers
  # (i.e. layers not needed to compose the complete profile)
  filter(!is.na(.data$layer_number)) %>%
  select(partner_short,
         plot_id,
         profile_id,
         survey_year,
         partner_code,
         code_country,
         code_plot,
         repetition,
         code_layer,
         layer_type,
         layer_number,
         depth_top,
         depth_bottom,
         depth_avg,
         layer_thickness,
         eff_soil_depth,
         bulk_density,
         organic_layer_weight,
         coarse_fragment_vol,
         coarse_fragment_vol_frac,
         organic_carbon_total) %>%
  # Calculate carbon density per cm (t C ha-1 cm-1)
  # Units: (g C/kg fine earth) * (kg fine earth/m3 soil) = g C/m3 soil
  # 1 ha * 1 cm = 100 m * 100 m * 0.01 m = 100 m3
  # 1 ton C = 1E6 g C
  # Unit conversion factor: * 100 / 1000000
  mutate(c_density =
           (.data$organic_carbon_total * .data$bulk_density *
              (1 - .data$coarse_fragment_vol_frac)) / 10000) %>%
  # Carbon stock per layer (instead of per cm)
  # Units: t C ha-1 (per layer)
  mutate(c_stock_layer =
           .data$c_density * .data$layer_thickness) %>%
  # In peat layers, organic layer weight may have been reported
  # instead of bulk density etc
  # In that case, use the formula for forest floor
  mutate(c_stock_layer =
           ifelse(is.na(.data$c_stock_layer) &
                    !is.na(.data$organic_layer_weight),
                  (.data$organic_carbon_total * .data$organic_layer_weight) /
                    100,
                  .data$c_stock_layer)) %>%
  mutate(c_density =
           ifelse(is.na(.data$c_density) &
                    !is.na(.data$c_stock_layer),
                  .data$c_stock_layer / .data$layer_thickness,
                  .data$c_density)) %>%
  # Add data availability index
  mutate(avail_thick = ifelse(is.na(.data$layer_thickness), 0, 1),
         avail_toc = ifelse(is.na(.data$organic_carbon_total), 0, 1),
         avail_bd = ifelse(is.na(.data$bulk_density), 0, 1),
         avail_cf = ifelse(is.na(.data$coarse_fragment_vol), 0, 1)) %>%
  select(-coarse_fragment_vol)


# Save the df_below_ground dataset (all data, also missing data)

write.csv2(df_below_ground,
           paste0("./output/stocks/", survey_form, "_below_ground.csv"),
           row.names = FALSE,
           na = "")






## 3.2. Calculate below-ground carbon stocks ----

profile_c_stocks_below_ground <- NULL
profile_list <- unique(df_below_ground$profile_id)

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
  
  assertthat::assert_that(length(unique(df_profile_i$eff_soil_depth)) == 1)
  soil_depth_i <- unique(df_profile_i$eff_soil_depth)
  
  # Prepare input for calculate_stocks function
  
  prof <- df_profile_i %>%
    select(plot_id,
           profile_id,
           code_layer,
           depth_top,
           depth_bottom,
           c_density,
           eff_soil_depth) %>%
    # Only calculate carbon stocks based on layers
    # for which the carbon density is known
    filter(!is.na(.data$c_density)) %>%
    # and for which their layer limits are known
    filter(!is.na(.data$depth_top) &
             !is.na(.data$depth_bottom))
  
  # Assert that all the depth layers are below-ground
  
  if (!prof_id_i %in% c("1995_7_139_1",
                        "1995_7_208_1",
                        "2008_13_164_1",
                        "2008_13_1161_1")) {
    
  assertthat::assert_that(
    all(prof$depth_top >= 0) &&
      all(prof$depth_bottom >= 0),
    msg = paste0("Not all layer limits are below-ground ",
                 "(i.e. not below 0) for the profile '",
                 prof_id_i, "'."))
  }
  
  # Issue:
  # Not possible to calculate splines in case of overlapping layers in
  # the profile
  # Solution: take average of the adjacent overlapping depth limits
  
  prof <- prof %>%
    mutate(prev_depth_bottom = lag(depth_bottom),
           next_depth_top = lead(depth_top)) %>%
    rowwise() %>%
    mutate(depth_bottom = ifelse(!is.na(.data$depth_bottom) &
                                !is.na(.data$next_depth_top) &
                                (.data$next_depth_top < .data$depth_bottom),
                              mean(c(.data$depth_bottom,
                                     .data$next_depth_top)),
                              .data$depth_bottom),
           depth_top = ifelse(!is.na(.data$depth_top) &
                                    !is.na(.data$prev_depth_bottom) &
                                    (.data$prev_depth_bottom > .data$depth_top),
                                  mean(c(.data$depth_top,
                                         .data$prev_depth_bottom)),
                                  .data$depth_top)) %>%
    select(-prev_depth_bottom,
           -next_depth_top)
    
  
  # Apply calculate_stocks and soilspline functions on profile
  
  # Only fit splines to profiles with:
  # - known carbon densities and layer limits for at least two layers
  # - and one of them should be the upper below-ground layer
  
  # If there are less than two layers or
  # if the highest layer is not the upper below-ground layer,
  # any stock calculations beyond the boundaries of the known layers
  # are simply too unreliable
  
  if ((nrow(prof) >= 2) &&
      (min(prof$depth_top) == 0) &&
      # The issue with this profile needs to be solved
      (!prof_id_i %in% c("1995_7_139_1",
                         "1995_7_208_1",
                         "2008_13_164_1",
                         "2008_13_1161_1"))) {
    
    profile_stock_output_i <- calculate_stocks(prof = prof,
                                               graph = FALSE)
    
    profile_c_stocks_i <-
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
    
    profile_c_stocks_below_ground <-
      rbind(profile_c_stocks_below_ground,
            profile_c_stocks_i)

  }
  
  # Update progress bar
  setTxtProgressBar(progress_bar, i)

} # End of for loop along profiles

close(progress_bar)
View(profile_c_stocks_below_ground)

# Save output

write.csv2(profile_c_stocks_below_ground %>%
             filter(c_stock_10 > 0),
           file = paste0("./output/stocks/",
                         survey_form,
                         "_profile_carbon_stocks_below_ground.csv"),
           row.names = FALSE,
           na = "")





## 3.3. Aggregate below-ground per plot ----

plot_c_stocks_below_ground <- profile_c_stocks_below_ground %>%
  filter(c_stock_10 > 0) %>%
  group_by(partner_short, partner_code, code_country, code_plot,
           plot_id, survey_year) %>%
  summarise(c_stock_below_ground_avg =
              round(mean(c_stock_below_ground, na.rm = TRUE), 2),
            # Standard deviation across spatial repetitions
            # (this also accounts for some sample preprocessing and
            # lab analytical uncertainty)
            c_stock_below_ground_stdev =
              ifelse(length(c_stock_below_ground) > 1,
                     round(sd(c_stock_below_ground, na.rm = TRUE), 2),
                     NA),
            nlay_min =
              min(nlay, na.rm = TRUE),
            nlay_max =
              max(nlay, na.rm = TRUE),
            obs_depth_avg =
              round(mean(obs_depth, na.rm = TRUE), 2),
            soil_depth_avg =
              round(mean(soil_depth, na.rm = TRUE), 2),
            contains_peat =
              any(contains_peat == TRUE),
            rmse_mpspline_max =
              max(rmse_mpspline, na.rm = TRUE),
            .groups = "drop") %>%
  rename(c_stock_below_ground = c_stock_below_ground_avg) %>%
  arrange(partner_short,
          code_plot,
          survey_year) %>%
  mutate_all(function(x) ifelse(is.nan(x), NA, x))

View(plot_c_stocks_below_ground)

# Save output

write.csv2(plot_c_stocks_below_ground,
           file = paste0("./output/stocks/",
                         survey_form,
                         "_plot_carbon_stocks_below_ground.csv"),
           row.names = FALSE,
           na = "")

# Number of plots

plot_c_stocks_below_ground %>%
  distinct(plot_id) %>%
  nrow

# Number of plots with more than two surveys

plot_c_stocks_below_ground %>%
  group_by(plot_id) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow







# 4. Forest floor layers ----
## 4.1. Derive layer-based dataset for forest floors ----

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
         survey_year,
         partner_code,
         code_country,
         code_plot,
         repetition,
         code_layer,
         layer_type,
         layer_number,
         depth_top,
         depth_bottom,
         depth_avg,
         layer_thickness,
         eff_soil_depth,
         bulk_density,
         organic_layer_weight,
         coarse_fragment_vol,
         coarse_fragment_vol_frac,
         organic_carbon_total) %>%
  # Carbon stock per layer (t C ha-1 for each layer)
  # Units: g C/kg forest floor * kg forest floor/m2
  mutate(c_stock_layer =
           (.data$organic_carbon_total * .data$organic_layer_weight) / 100) %>%
  # Carbon density (t C ha-1 cm-1)
  mutate(c_density =
           .data$c_stock_layer / .data$layer_thickness) %>%
  # Add data availability index
  mutate(avail_thick = ifelse(is.na(.data$layer_thickness), 0, 1),
         avail_toc = ifelse(is.na(.data$organic_carbon_total), 0, 1),
         avail_bd = ifelse(is.na(.data$bulk_density), 0, 1),
         avail_org_layer_weight =
           ifelse(is.na(.data$organic_layer_weight), 0, 1)) %>%
  select(-coarse_fragment_vol)

# Save the df_forest_floor dataset (all data, also missing data)

write.csv2(df_forest_floor,
           paste0("./output/stocks/", survey_form, "_forest_floor.csv"),
           row.names = FALSE,
           na = "")




## 4.2. Calculate forest floor carbon stocks ----

profile_c_stocks_forest_floor <- NULL
profile_list <- unique(df_forest_floor$profile_id)

# Set progress bar
progress_bar <- txtProgressBar(min = 0,
                               max = length(profile_list), style = 3)

for (i in seq_along(profile_list)) {
  
  # Select profile
  
  df_profile_i <- df_forest_floor %>%
    filter(profile_id == profile_list[i]) %>%
    filter(!is.na(c_stock_layer))
  
  if (nrow(df_profile_i) > 0) {
  
  # Combine stocks in OL, OFH and O per profile
  
  profile_c_stocks_i <-
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
               nlay = length(df_profile_i$layer_number),
               forest_floor_layers = paste(c(df_profile_i$code_layer),
                                           collapse = "_"),
               c_stock_ol =
                 ifelse("OL" %in% df_profile_i$code_layer,
                        df_profile_i %>%
                          filter(code_layer == "OL") %>%
                          pull(c_stock_layer) %>%
                          round(2),
                        NA),
               c_stock_ofh =
                 ifelse(("OF" %in% df_profile_i$code_layer &&
                          "OH" %in% df_profile_i$code_layer) ||
                          ("OFH" %in% df_profile_i$code_layer),
                        df_profile_i %>%
                          filter(code_layer %in% c("OF", "OH", "OFH")) %>%
                          pull(c_stock_layer) %>%
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
               c_stock_forest_floor =
                 round(sum(df_profile_i$c_stock_layer), 2))

  profile_c_stocks_forest_floor <-
    rbind(profile_c_stocks_forest_floor,
          profile_c_stocks_i)
  
  }
  
  # Update progress bar
  setTxtProgressBar(progress_bar, i)
}

close(progress_bar)
View(profile_c_stocks_forest_floor)

# Save output

write.csv2(profile_c_stocks_forest_floor %>%
             filter(c_stock_forest_floor >= 0),
           file = paste0("./output/stocks/",
                         survey_form,
                         "_profile_carbon_stocks_forest_floor.csv"),
           row.names = FALSE,
           na = "")






## 4.3. Aggregate forest floor per plot ----

plot_c_stocks_forest_floor <-
  profile_c_stocks_forest_floor %>%
  filter(c_stock_forest_floor >= 0) %>%
  group_by(partner_short, partner_code, code_country, code_plot,
           plot_id, survey_year) %>%
  summarise(c_stock_forest_floor_avg =
              round(mean(c_stock_forest_floor, na.rm = TRUE), 2),
            # Standard deviation across spatial repetitions
            # (this also accounts for some sample preprocessing and
            # lab analytical uncertainty)
            c_stock_forest_floor_stdev =
              ifelse(length(c_stock_forest_floor) > 1,
                     round(sd(c_stock_forest_floor, na.rm = TRUE), 2),
                     NA),
            nlay_min =
              min(nlay, na.rm = TRUE),
            nlay_max =
              max(nlay, na.rm = TRUE),
            forest_floor_thickness_avg =
              round(mean(forest_floor_thickness, na.rm = TRUE), 2),
            forest_floor_layers_unique =
              ifelse(length(unique(forest_floor_layers)) == 1,
                     unique(forest_floor_layers),
                     NA),
            .groups = "drop") %>%
  rename(c_stock_forest_floor = c_stock_forest_floor_avg) %>%
  arrange(partner_short,
          code_plot,
          survey_year) %>%
  mutate_all(function(x) ifelse(is.nan(x), NA, x))

View(plot_c_stocks_forest_floor)

# Save output

write.csv2(plot_c_stocks_forest_floor,
           file = paste0("./output/stocks/",
                         survey_form,
                         "_plot_carbon_stocks_forest_floor.csv"),
           row.names = FALSE,
           na = "")






# 5. Combine below-ground and forest floor ----
## 5.1. Join below-ground and forest floor ----

profile_c_stocks <-
  profile_c_stocks_below_ground %>%
  rename(nlay_below_ground = nlay) %>%
  left_join(profile_c_stocks_forest_floor %>%
              select(-partner_short,
                     -plot_id,
                     -survey_year,
                     -partner_code,
                     -code_country,
                     -code_plot,
                     -repetition) %>%
              rename(nlay_forest_floor = nlay),
            by = "profile_id") %>%
  mutate(c_stock =
           rowSums(select(., c_stock_below_ground, c_stock_forest_floor),
                          na.rm = TRUE),
         nlay = rowSums(select(., nlay_below_ground, nlay_forest_floor),
                         na.rm = TRUE))

# Save output

write.csv2(profile_c_stocks %>%
             filter(c_stock_10 > 0),
           file = paste0("./output/stocks/",
                         survey_form,
                         "_profile_carbon_stocks.csv"),
           row.names = FALSE,
           na = "")

## 5.2. Aggregate per plot ----

plot_c_stocks <- profile_c_stocks %>%
  filter(c_stock_10 > 0) %>%
  group_by(partner_short, partner_code, code_country, code_plot,
           plot_id, survey_year) %>%
  summarise(c_stock_avg =
              round(mean(c_stock, na.rm = TRUE), 2),
            # Standard deviation across spatial repetitions
            # (this also accounts for some sample preprocessing and
            # lab analytical uncertainty)
            c_stock_stdev =
              ifelse(length(c_stock) > 1,
                     round(sd(c_stock, na.rm = TRUE), 2),
                     NA),
            c_stock_below_ground_avg =
              round(mean(c_stock_below_ground, na.rm = TRUE), 2),
            c_stock_below_ground_stdev =
              ifelse(length(c_stock_below_ground) > 1,
                     round(sd(c_stock_below_ground, na.rm = TRUE), 2),
                     NA),
            c_stock_forest_floor_avg =
              round(mean(c_stock_forest_floor, na.rm = TRUE), 2),
            c_stock_forest_floor_stdev =
              ifelse(length(c_stock_forest_floor) > 1,
                     round(sd(c_stock_forest_floor, na.rm = TRUE), 2),
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
  rename(c_stock = c_stock_avg) %>%
  rename(c_stock_forest_floor = c_stock_forest_floor_avg) %>%
  rename(c_stock_below_ground = c_stock_below_ground_avg) %>%
  # Some columns contain "NaN" instead of NA: replace
  mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%
  arrange(partner_short,
          code_plot,
          survey_year)

View(plot_c_stocks)

# Save output

write.csv2(plot_c_stocks,
           file = paste0("./output/stocks/",
                         survey_form,
                         "_plot_carbon_stocks.csv"),
           row.names = FALSE,
           na = "")




# 6. Visual check per plot





