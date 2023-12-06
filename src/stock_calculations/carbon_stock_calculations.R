
# Calculate carbon stocks until 100 cm of depth
# Using a mass-preserving spline function of soil layers/horizons

# Input data: "./data/layer1_data/"
# Output data: "./output/stocks/"

# 1. Load packages and functions ----

stopifnot(require("tidyverse"),
          require("assertthat"),
          require("aqp"),
          require("mpspline2"),
          require("ggplot2"))

source("./src/stock_calculations/functions/soilspline.R")
source("./src/stock_calculations/functions/spline2stock.R")

source("./src/functions/get_env.R")

# 2. Specify level ----

level <- "LII"

# 3. Calculate stocks ----


if (level == "LI") {

  source("./src/stock_calculations/functions/get_stocks.R")

  get_stocks(survey_form = "s1_som",
             data_frame = s1_som,
             graph = TRUE,
             density_per_three_cm = TRUE)


  get_stocks(survey_form = "s1_pfh",
             data_frame = s1_pfh,
             graph = TRUE,
             density_per_three_cm = TRUE)

}


if (level == "LII") {

  source("./src/stock_calculations/functions/get_stocks.R")

get_stocks(survey_form = "so_som",
           data_frame = so_som,
           graph = TRUE,
           density_per_three_cm = TRUE)


get_stocks(survey_form = "so_pfh",
           data_frame = so_pfh,
           graph = TRUE,
           density_per_three_cm = TRUE)

}

# 4. Compile data per level ----

if (level == "LII") {


df_layer <-
  bind_rows(so_som_below_ground %>%
              mutate(survey_form = "so_som") %>%
              relocate(survey_form, .before = partner_short) %>%
              mutate(repetition = as.character(repetition)) %>%
              select(-avail_thick,
                     -avail_toc,
                     -avail_bd,
                     -avail_cf),
            so_pfh_below_ground %>%
              mutate(survey_form = "so_pfh") %>%
              relocate(survey_form, .before = partner_short) %>%
              select(-avail_thick,
                     -avail_toc,
                     -avail_bd,
                     -avail_cf),
            so_som_forest_floor %>%
              mutate(survey_form = "so_som") %>%
              relocate(survey_form, .before = partner_short) %>%
              relocate(density, .before = stock_layer) %>%
              mutate(repetition = as.character(repetition)) %>%
              select(-avail_thick,
                     -avail_toc,
                     -avail_bd,
                     -avail_org_layer_weight),
            so_pfh_forest_floor %>%
              mutate(survey_form = "so_pfh") %>%
              relocate(survey_form, .before = partner_short) %>%
              relocate(density, .before = stock_layer) %>%
              select(-avail_thick,
                     -avail_toc,
                     -avail_bd,
                     -avail_org_layer_weight)) %>%
  rename(profile_id_in_form = profile_id) %>%
  mutate(profile_id = paste0(survey_form, "_",
                             profile_id_in_form)) %>%
  arrange(partner_short,
          code_plot,
          survey_year,
          profile_id,
          layer_number)


df_stocks <-
  bind_rows(so_som_profile_c_stocks %>%
              mutate(survey_form = "so_som") %>%
              relocate(survey_form, .before = partner_short) %>%
              mutate(repetition = as.character(repetition)),
            so_pfh_profile_c_stocks %>%
              mutate(survey_form = "so_pfh") %>%
              relocate(survey_form, .before = partner_short)) %>%
  rename(profile_id_in_form = profile_id) %>%
  mutate(profile_id = paste0(survey_form, "_",
                             profile_id_in_form)) %>%
  arrange(partner_short,
          code_plot,
          survey_year,
          profile_id)

}

# 5. Add stratifiers ----

if (level == "LI") {

  source("./src/functions/get_stratifiers.R")
  s1_strat <- get_stratifiers(level = "LI") %>%
    select(plot_id, longitude_dec, latitude_dec,
           wrb_soil_group, forest_type, humus_type, biogeo,
           main_tree_species, bs_class)

  source("./src/functions/save_to_google_drive.R")
  save_to_google_drive(objects_to_save = "s1_strat",
                       path_name = "layer1_data")

  s1_strat <- s1_strat %>%
    select(plot_id, longitude_dec, latitude_dec,
           wrb_soil_group, forest_type, humus_type, biogeo,
           main_tree_species, bs_class)



}


if (level == "LII") {

  source("./src/functions/get_stratifiers.R")
  so_strat <- get_stratifiers(level = "LII")

  source("./src/functions/save_to_google_drive.R")
  save_to_google_drive(objects_to_save = "so_strat",
                       path_name = "layer1_data")


  so_strat <- so_strat %>%
    select(plot_id, longitude_dec, latitude_dec,
           wrb_soil_group, forest_type, humus_type, biogeo,
           main_tree_species, bs_class)

  df_layer <- df_layer %>%
    left_join(so_strat,
              by = "plot_id")

  df_stocks <- df_stocks %>%
    left_join(so_strat,
              by = "plot_id")

  # Add colours

  df_layer <- df_layer %>%
    mutate(unique_layer_repetition =
             paste0(plot_id, "_",
                    survey_year, "_",
                    repetition, "_",
                    code_layer)) %>%
    left_join(so_pfh %>%
                mutate(unique_layer_repetition =
                         paste0(plot_id, "_",
                                survey_year, "_",
                                profile_pit_id, "_",
                                horizon_master)) %>%
                select(unique_layer_repetition,
                       colour_moist_hex),
              by = "unique_layer_repetition") %>%
    select(-unique_layer_repetition) %>%
    mutate(colour_moist_hex =
             ifelse(grepl("som", .data$survey_form) &
                      !is.na(.data$colour_moist_hex),
                    NA,
                    .data$colour_moist_hex))

}


# 6. Make a graph per plot_id ----

plot_ids <- unique(df_layer$plot_ids)

# Evaluate for each of the plot surveys

for (i in seq_along(plot_ids)) {

  profiles_i <- df_layer %>%
    filter(plot_id == plot_ids[i]) %>%
    distinct(profile_id) %>%
    pull(profile_id)

  for (j in seq_along(profiles_i)) {

    prof_id_j <- profiles_i[j]

    df_prof_j <- df_layer %>%
      filter(profile_id == prof_id_i)

    df_stock_j <- df_stocks %>%
      filter(profile_id == prof_id_i)



  }
  }













# Create a data frame for plotting
plot_data <- data.frame(
  depth_average = (-1) * (depth_top + depth_bottom) / 2,
  variab = variab
)

# Create a ggplot object
p <- ggplot(plot_data, aes(x = variab, y = depth_average)) +
  geom_point(col = "black", size = 3) +
  geom_rect(
    aes(xmin = 0, xmax = variab, ymin = -depth_bottom, ymax = -depth_top),
    fill = "grey80", color = "white", size = 2
  ) +
  geom_abline(h = -max_soil_depth, linetype = "dashed", color = "red", size = 2) +
  geom_line(data = data.frame(x = spline_output, y = seq_along(spline_output)),
            aes(x = x, y = -y), col = "blue", size = 3) +
  labs(
    x = expression('Carbon density (t C ha'^-1*'cm'^-1*')'),
    y = "Depth (cm)",
    title = id
  ) +
  theme_minimal()

# Save the ggplot as an image file
ggsave(paste0(path, id, ".png"), p)









