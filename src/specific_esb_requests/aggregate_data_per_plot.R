

# Aggregate soil data into one value per plot
# -------------------------------------------

# Question: Deliver soil data aggregated in one value per plot. This is
# relevant for:
# - Analysis of residuals in validation of Yasso model
#   (within HE PathFinder project)
# - Nutrient availability metrics
#   (within Ecological Studies Book of ICP Forests)

# Critical question:
# Until which depth do we integrate data? And do we include the forest floor?

# Possible variables:

# - Base saturation class
#   → available in so_strat
# - WRB
#   → available in so_strat
# - Some measure for the water content: code_water (so_prf)
#   → available in so_strat
# - Clay particle size fractions
#   → using profiles compiled for water budget modelling.
#     Weighted using soil mass (i.e. amount of fine earth) until 100 cm
#     in this R script.
# - Silt particle size fractions
#   → using profiles compiled for water budget modelling.
#     Weighted using soil mass until 100 cm in this R script.
# - Sand particle size fractions
#   → using profiles compiled for water budget modelling.
#     Weighted using soil mass until 100 cm in this R script.
# - Texture class
#   → Gap-fill using particle size fractions
# - n_total → available as stocks until 30 and 100 cm in stock script
# - extrac_p (aqua regia; ~total)
#   → available as stocks until 30 and 100 cm in stock script
# - extrac_s (aqua regia; ~total)
#   → available as stocks until 30 and 100 cm in stock script
# - rea_fe (acid ammonium oxalate-extractable; ~poorly crystalline)
#   → available as stocks until 30 and 100 cm in stock script
# - rea_al (acid ammonium oxalate-extractable; ~poorly crystalline)
#   → available as stocks until 30 and 100 cm in stock script
# - exch_ca (barium chloride-extractable)
#   → available as stocks until 30 and 100 cm in stock script
#     Original data were reported in cmol(+) kg-1, but the stock dataset
#     contains mass-based data, i.e. in kg Ca ha-1.
#     (1 cmol(+) (from Ca2+) kg-1 = 200.39 mg Ca2+ kg-1)
# - exch_cec
#   → Weighted using soil mass until 100 cm.
# - ph_cacl2
#   → Weighted using soil volume (i.e. depth) until 100 cm.
# - C:N ratio
#   → Using C and N stocks until 30 and 100 cm.
# - bulk_density (as a measure for porosity)
#   → remove pedotransfer-estimated values
#     Weighted using soil volume (excl. stones) until 100 cm.






# Script initiation: December 2024

# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O



# Prepare packages and functions ----

# Define required packages
stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"),
          require("soiltexture"))

# Source functions

source("./src/functions/harmonise_layer_to_depths.R")
source("./src/functions/depth_join.R")




# Import ----

## Profiles compiled for water budget modelling ----
# (one profile with mostly physical data per plot):

# see: "./src/specific_esb_requests/soil_data_for_water_budget_modelling.R"

path <- "./output/specific_esb_requests/"
files <- list.files(path)[which(grepl("so_data_for_wbm.csv", list.files(path)))]
dates <- as.Date(sapply(strsplit(files, "_"), `[`, 1),
                 format = "%Y%m%d")

so_data_for_wbm <- read.csv(paste0(path, files[which.max(dates)]),
                            sep = ";") %>%
  as_tibble %>%
  # Use layers above effective soil depth
  filter(above_rock == TRUE) %>%
  # Remove plots without any original observations
  filter(all_na != "Plot without original observations")



## Stocks ----

# see: "./src/stock_calculations/"

path_root <- "./output/stocks/"
folders <- list.dirs(path_root, full.names = FALSE, recursive = FALSE)

parameters_stocks <- c("c",
                       "n",
                       "extrac_p",
                       "extrac_s",
                       "rea_fe",
                       "rea_al",
                       "exch_ca")

for (par_i in parameters_stocks) {

  paths_i <- folders[grepl(paste0(par_i, "_stocks"), folders)]

  # Select most recent path

  dates <- as.Date(sapply(strsplit(paths_i, "_"), `[`, 1),
                   format = "%Y%m%d")

  path_i <- paste0(path_root,
                   paths_i[which.max(dates)], "/")

  path_i <- list.files(path_i, full.names = TRUE)[which(
    grepl(paste0("_plot_", par_i, "_stocks.csv"), list.files(path_i)))]

  data_i <- read.csv(path_i,
                     sep = ";") %>%
    as_tibble

  assign_env(paste0("so", "_plot_",
                    par_i, "_stocks"),
             data_i)

}




## Layer1+ data

source("./src/functions/read_processed.R")
read_processed(path_name = "./data/layer1_data/",
               save_to_env = TRUE)


## Stratifiers

source("./src/functions/get_stratifiers.R")
so_strat <- get_stratifiers(level = "LII")




# Add ph_cacl2 ----
# Most recent pH-CaCl2 data to dataframe with one harmonised profile per plot

df <- depth_join(df1 = so_data_for_wbm,
                 df2 = so_som,
                 parameters = "ph_cacl2",
                 mode = "most_recent")

df1 <- df

# Add cation exchange capacity ----
# Most recent CEC data to dataframe with one harmonised profile per plot

df <- depth_join(df1 = df,
                 df2 = so_som,
                 parameters = "exch_cec",
                 mode = "most_recent")

df2 <- df


# Aggregate over depth ----
# (into one value per plot)

df_plot <- df %>%
  # Add soil depths
  left_join(so_strat %>%
              select(plot_id, eff_soil_depth),
            by = "plot_id") %>%
  mutate(coarse_fragment_vol = coalesce(coarse_fragment_vol,
                                        0),
         eff_soil_depth = pmin(eff_soil_depth, 100)) %>%
  # Add a column with original bulk density values, excluding values
  # gap-filled with pedotransfer functions
  mutate(
    bulk_density_meas = ifelse(
      !grepl("PTF", bulk_density_source),
      bulk_density,
      NA_real_)) %>%
  # Summarise per per plot_id over depth
  group_by(country, plot_id, code_country, partner_code, code_plot,
           survey_years, eff_soil_depth) %>%
  reframe(part_size_clay =
            harmonise_layer_to_depths(
              limit_sup = 0,
              limit_inf = round(unique(.data$eff_soil_depth)),
              bulk_density = .data$bulk_density,
              coarse_fragment_vol_frac = 0.01 * .data$coarse_fragment_vol,
              upper_depths = .data$layer_limit_superior,
              lower_depths = .data$layer_limit_inferior,
              variab = .data$part_size_clay,
              parameter_name = "part_size_clay",
              weighting_mode = "by_fine_earth_mass"),
          part_size_silt =
            harmonise_layer_to_depths(
              limit_sup = 0,
              limit_inf = round(unique(.data$eff_soil_depth)),
              bulk_density = .data$bulk_density,
              coarse_fragment_vol_frac = 0.01 * .data$coarse_fragment_vol,
              upper_depths = .data$layer_limit_superior,
              lower_depths = .data$layer_limit_inferior,
              variab = .data$part_size_silt,
              parameter_name = "part_size_silt",
              weighting_mode = "by_fine_earth_mass"),
          part_size_sand =
            harmonise_layer_to_depths(
              limit_sup = 0,
              limit_inf = round(unique(.data$eff_soil_depth)),
              bulk_density = .data$bulk_density,
              coarse_fragment_vol_frac = 0.01 * .data$coarse_fragment_vol,
              upper_depths = .data$layer_limit_superior,
              lower_depths = .data$layer_limit_inferior,
              variab = .data$part_size_sand,
              parameter_name = "part_size_sand",
              weighting_mode = "by_fine_earth_mass"),
          texture_class =
            harmonise_layer_to_depths(
              limit_sup = 0,
              limit_inf = round(unique(.data$eff_soil_depth)),
              bulk_density = .data$bulk_density,
              coarse_fragment_vol_frac = 0.01 * .data$coarse_fragment_vol,
              upper_depths = .data$layer_limit_superior,
              lower_depths = .data$layer_limit_inferior,
              variab = .data$texture_class,
              parameter_name = "texture_class",
              mode = "categorical",
              weighting_mode = "by_fine_earth_mass"),
          texture_class_esd =
            harmonise_layer_to_depths(
              limit_sup = 0,
              limit_inf = round(unique(.data$eff_soil_depth)),
              bulk_density = .data$bulk_density,
              coarse_fragment_vol_frac = 0.01 * .data$coarse_fragment_vol,
              upper_depths = .data$layer_limit_superior,
              lower_depths = .data$layer_limit_inferior,
              variab = .data$texture_class_esd,
              parameter_name = "texture_class_esd",
              mode = "categorical",
              weighting_mode = "by_fine_earth_mass"),
          ph_cacl2 =
            harmonise_layer_to_depths(
              limit_sup = 0,
              limit_inf = round(unique(.data$eff_soil_depth)),
              bulk_density = .data$bulk_density,
              coarse_fragment_vol_frac = 0.01 * .data$coarse_fragment_vol,
              upper_depths = .data$layer_limit_superior,
              lower_depths = .data$layer_limit_inferior,
              variab = .data$ph_cacl2,
              parameter_name = "ph_cacl2",
              weighting_mode = "by_vol"),
          exch_cec =
            harmonise_layer_to_depths(
              limit_sup = 0,
              limit_inf = round(unique(.data$eff_soil_depth)),
              bulk_density = .data$bulk_density,
              coarse_fragment_vol_frac = 0.01 * .data$coarse_fragment_vol,
              upper_depths = .data$layer_limit_superior,
              lower_depths = .data$layer_limit_inferior,
              variab = .data$exch_cec,
              parameter_name = "exch_cec",
              weighting_mode = "by_fine_earth_mass"),
          bulk_density =
            harmonise_layer_to_depths(
              limit_sup = 0,
              limit_inf = round(unique(.data$eff_soil_depth)),
              bulk_density = .data$bulk_density,
              coarse_fragment_vol_frac = 0.01 * .data$coarse_fragment_vol,
              upper_depths = .data$layer_limit_superior,
              lower_depths = .data$layer_limit_inferior,
              variab = .data$bulk_density_meas,
              parameter_name = "bulk_density",
              weighting_mode = "by_fine_earth_vol")) %>%
  mutate_all(function(x) ifelse((x) == "", NA, x)) %>%
  rowwise() %>%
  # Calculate the texture sum to make sure this is ~100 %
  mutate(sum_texture = ifelse(
    any(!is.na(c_across(c(part_size_clay,
                          part_size_silt,
                          part_size_sand)))),
    sum(c_across(c(part_size_clay,
                   part_size_silt,
                   part_size_sand)), na.rm = TRUE),
    NA_real_)) %>%
  ungroup()

df_plot2 <- df_plot

assertthat::assert_that(
  all(na.omit(df_plot$sum_texture) > 99.5) &&
    all(na.omit(df_plot$sum_texture) < 100.5))



# Classify texture fractions according to USDA ----

df_plot <- df_plot %>%
  left_join(
    data.frame(
      # plot_id
      plot_id = df_plot$plot_id[which(!is.na(df_plot$sum_texture))],
      # Texture classes USDA
      texture_class_frac = TT.points.in.classes(
        tri.data = as.data.frame(df_plot[which(!is.na(df_plot$sum_texture)), ]),
        class.sys = "USDA.TT",
        css.names = c("part_size_clay", "part_size_silt", "part_size_sand"),
        PiC.type = "t",
        collapse = "_")) %>%
      as_tibble(),
    by = "plot_id") %>%
  mutate(
    texture_class_frac = case_when(
      texture_class_frac == "Cl" ~ "Clay",
      texture_class_frac == "SiCl" ~ "Silty clay",
      texture_class_frac == "SaCl" ~ "Sandy clay",
      texture_class_frac == "ClLo" ~ "Clay loam",
      texture_class_frac == "SiClLo" ~ "Silty clay loam",
      texture_class_frac == "SaClLo" ~ "Sandy clay loam",
      texture_class_frac == "Lo" ~ "Loam",
      texture_class_frac == "SiLo" ~ "Silt loam", #"Silty loam",
      texture_class_frac == "SaLo" ~ "Sandy loam",
      texture_class_frac == "Si" ~ "Silt",
      texture_class_frac == "LoSa" ~ "Loamy sand",
      texture_class_frac == "Sa" ~ "Sand")) %>%
  select(-sum_texture) %>%
  relocate(texture_class_frac, .after = texture_class)

df_plot3 <- df_plot


# Add stocks ----

## Combine stocks for different variables in one dataform ----

for (par_i in parameters_stocks) {

  data_i <- get_env(paste0("so_plot_",
                           par_i, "_stocks")) %>%
    as_tibble %>%
    # Add the variable name (e.g. "c_" for TOC) to the stock names if needed
    rename_with(
      ~ case_when(
        startsWith(., "stock") ~ paste(par_i, ., sep = "_"),
        TRUE ~ .)) %>%
    rename_with(
      ~ paste(par_i, ., sep = "_"),
      obs_depth_avg) %>%
    filter(survey_form == "so_som") %>%
    mutate(
      unique_survey = paste0(code_country, "_",
                             survey_year, "_",
                             code_plot)) %>%
    relocate(unique_survey, .after = plot_id)

  if (par_i == "c") {

    stocks_combined <- data_i %>%
      select(partner_short, code_plot, survey_year,
             plot_id, unique_survey, depth_stock_avg,
             ends_with("stock_below_ground"),
             ends_with("stock_below_ground_plaus"),
             ends_with("stock_below_ground_topsoil"),
             ends_with("stock_below_ground_topsoil_plaus"),
             ends_with("obs_depth_avg"))

  } else {

    stocks_combined <- stocks_combined %>%
      left_join(
        data_i %>%
          select(unique_survey,
                 ends_with("stock_below_ground"),
                 ends_with("stock_below_ground_plaus"),
                 ends_with("stock_below_ground_topsoil"),
                 ends_with("stock_below_ground_topsoil_plaus"),
                 ends_with("obs_depth_avg")),
        by = "unique_survey")
  }

}

stocks_combined2 <- stocks_combined


## Select the survey_year with the overall best set of stock data ----

stocks_combined <- stocks_combined %>%
  # Add columns to use as selection criteria
  mutate(
    # Number of parameters for which stocks are calculated
    data_avail_count = rowSums(!is.na(select(., ends_with("_topsoil")))),
    # Number of parameters for which plausible stocks are calculated
    plausible_count = rowSums(select(., ends_with("_plaus")) == TRUE,
                              na.rm = TRUE),
    # Number of plausible C and N stocks
    plausible_count_cn = rowSums(
      select(., matches("^(c|n).*_plaus")) == TRUE,
      na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(
    # Unique obs_depth
    obs_depth_avg_unique = {
      # Get the unique non-NA values from columns ending with "_obs_depth_avg"
      unique_values <- unique(c_across(ends_with("_obs_depth_avg")))
      unique_values <- unique_values[which(!is.na(unique_values))]
      # If there's exactly one unique value, use it; otherwise, assign -1
      if (length(unique_values) == 1) unique_values else -1
    },
    # Max obs_depth (since data are not always available until the same depth
    # for a certain plot_id x survey_year)
    obs_depth_avg =
      max(c_across(ends_with("_obs_depth_avg")), na.rm = TRUE)) %>%
  ungroup() %>%
  # Apply filters to obtain the best survey_year per plot_id
  group_by(plot_id) %>%
  # Criterium 1: survey_year with stock data for highest amount of variables
  slice_max(order_by = data_avail_count) %>%
  # Criterium 2: survey_year with deepest observations
  slice_max(order_by = obs_depth_avg) %>%
  # Criterium 3: survey_year with highest amount of plausible stock data
  slice_max(order_by = plausible_count) %>%
  # Criterium 4: most recent survey_year
  slice_max(order_by = survey_year) %>%
  ungroup() %>%
  arrange(partner_short, code_plot) %>%
  rename(obs_depth_max = obs_depth_avg) %>%
  rename(survey_year_stocks = survey_year) %>%
  select(-data_avail_count, -plausible_count, -plausible_count_cn,
         -obs_depth_avg_unique,
         -partner_short, -code_plot, -unique_survey,
         -ends_with("_plaus"), -ends_with("_obs_depth_avg")) %>%
  mutate(
    use_stock_topsoil =
      (.data$obs_depth_max < 30 &
         .data$obs_depth_max < 0.7 * .data$depth_stock_avg)) %>%
  select(-depth_stock_avg) %>%
  mutate(
    c_to_n_ratio_below_ground = ifelse(!is.na(c_stock_below_ground) &
                            !is.na(n_stock_below_ground),
                          round(c_stock_below_ground / n_stock_below_ground,
                                2),
                          NA_real_),
    c_to_n_ratio_below_ground_topsoil =
      ifelse(!is.na(c_stock_below_ground_topsoil) &
               !is.na(n_stock_below_ground_topsoil),
             round(c_stock_below_ground_topsoil / n_stock_below_ground_topsoil,
                   2),
             NA_real_))

stocks_combined3 <- stocks_combined


## Add stocks and so_strat to data ----

df_plot <- df_plot %>%
  left_join(stocks_combined,
            by = "plot_id") %>%
  # Remove C stocks
  select(-starts_with("c_stock_")) %>%
  # Consider texture classes based on fractions as the final texture class
  # To do: check inconsistencies between reported classes and classes based
  #        on fractions!
  mutate(
    texture_class2 = coalesce(texture_class_frac,
                              texture_class)) %>%
  select(-texture_class, -texture_class_frac, -texture_class_esd) %>%
  rename(texture_class = texture_class2) %>%
  relocate(texture_class, .after = part_size_sand) %>%
  left_join(so_strat %>%
              select(-country, -eff_soil_depth, -eff_soil_depth_source,
                     -rooting_depth),
            by = "plot_id")



# Save data ----

so_plot_agg_data <- df_plot

write.table(so_plot_agg_data,
            file = paste0("./output/pathfinder/plot_agg_data/",
                          as.character(format(Sys.Date(), format = "%Y%m%d")),
                          "_so_plot_agg_data.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")


source("./src/functions/create_attribute_catalogue.R")

create_attribute_catalogue(data_frame = so_plot_agg_data,
                           path_to_save = paste0(
                             "./output/pathfinder/plot_agg_data/",
                             "so_plot_agg_data_"))











