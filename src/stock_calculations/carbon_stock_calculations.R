
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
             graph = FALSE,
             density_per_three_cm = TRUE)


  get_stocks(survey_form = "s1_pfh",
             data_frame = s1_pfh,
             graph = FALSE,
             density_per_three_cm = TRUE)

}


if (level == "LII") {

  source("./src/stock_calculations/functions/get_stocks.R")

get_stocks(survey_form = "so_som",
           data_frame = so_som,
           graph = FALSE,
           density_per_three_cm = TRUE)


get_stocks(survey_form = "so_pfh",
           data_frame = so_pfh,
           graph = FALSE,
           density_per_three_cm = TRUE)

}

# 4. Compile data per level ----

if (level == "LI") {

  df_layer_li <-
    bind_rows(s1_som_below_ground %>%
                mutate(survey_form = "s1_som") %>%
                relocate(survey_form, .before = partner_short) %>%
                mutate(repetition = as.character(repetition)) %>%
                select(-avail_thick,
                       -avail_toc,
                       -avail_bd,
                       -avail_cf),
              s1_pfh_below_ground %>%
                mutate(survey_form = "s1_pfh") %>%
                relocate(survey_form, .before = partner_short) %>%
                select(-avail_thick,
                       -avail_toc,
                       -avail_bd,
                       -avail_cf),
              s1_som_forest_floor %>%
                mutate(survey_form = "s1_som") %>%
                relocate(survey_form, .before = partner_short) %>%
                relocate(density, .before = stock_layer) %>%
                mutate(repetition = as.character(repetition)) %>%
                select(-avail_thick,
                       -avail_toc,
                       -avail_bd,
                       -avail_org_layer_weight),
              s1_pfh_forest_floor %>%
                mutate(survey_form = "s1_pfh") %>%
                relocate(survey_form, .before = partner_short) %>%
                relocate(density, .before = stock_layer) %>%
                select(-avail_thick,
                       -avail_toc,
                       -avail_bd,
                       -avail_org_layer_weight)) %>%
    mutate(profile_id_form = paste0(survey_form, "_",
                                    profile_id)) %>%
    relocate(profile_id_form, .after = profile_id) %>%
    arrange(partner_short,
            code_plot,
            survey_year,
            profile_id,
            layer_number)


  df_stocks_li <-
    bind_rows(s1_som_profile_c_stocks %>%
                mutate(survey_form = "s1_som") %>%
                relocate(survey_form, .before = partner_short) %>%
                mutate(repetition = as.character(repetition)),
              s1_pfh_profile_c_stocks %>%
                mutate(survey_form = "s1_pfh") %>%
                relocate(survey_form, .before = partner_short)) %>%
    as_tibble() %>%
    mutate(profile_id_form = paste0(survey_form, "_",
                                    profile_id)) %>%
    relocate(profile_id_form, .after = profile_id) %>%
    rename(use_stock_topsoil = use_stock_until_30) %>%
    # select(-matches("^stock_[1-9][0-9]*0$")) %>%
    # select(-stock_ol, -stock_ofh) %>%
    arrange(partner_short,
            code_plot,
            survey_year,
            profile_id)

  df_stocks_plot_li <-
    bind_rows(s1_som_plot_c_stocks %>%
                mutate(survey_form = "s1_som") %>%
                relocate(survey_form, .before = partner_short),
              s1_pfh_plot_c_stocks %>%
                mutate(survey_form = "s1_pfh") %>%
                relocate(survey_form, .before = partner_short)) %>%
    rename(use_stock_topsoil = use_stock_until_30) %>%
    select(-matches("^stock_[1-9][0-9]*0$")) %>%
    arrange(partner_short,
            code_plot,
            survey_year)

}





if (level == "LII") {

df_layer_lii <-
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
  mutate(profile_id_form = paste0(survey_form, "_",
                                  profile_id)) %>%
  relocate(profile_id_form, .after = profile_id) %>%
  arrange(partner_short,
          code_plot,
          survey_year,
          profile_id,
          layer_number)


df_stocks_lii <-
  bind_rows(so_som_profile_c_stocks %>%
              mutate(survey_form = "so_som") %>%
              relocate(survey_form, .before = partner_short) %>%
              mutate(repetition = as.character(repetition)),
            so_pfh_profile_c_stocks %>%
              mutate(survey_form = "so_pfh") %>%
              relocate(survey_form, .before = partner_short)) %>%
  as_tibble() %>%
  mutate(profile_id_form = paste0(survey_form, "_",
                                  profile_id)) %>%
  relocate(profile_id_form, .after = profile_id) %>%
  rename(use_stock_topsoil = use_stock_until_30) %>%
  # select(-matches("^stock_[1-9][0-9]*0$")) %>%
  # select(-stock_ol, -stock_ofh) %>%
  arrange(partner_short,
          code_plot,
          survey_year,
          profile_id)



df_stocks_plot_lii <-
  bind_rows(so_som_plot_c_stocks %>%
              mutate(survey_form = "so_som") %>%
              relocate(survey_form, .before = partner_short),
            so_pfh_plot_c_stocks %>%
              mutate(survey_form = "so_pfh") %>%
              relocate(survey_form, .before = partner_short)) %>%
  rename(use_stock_topsoil = use_stock_until_30) %>%
  select(-matches("^stock_[1-9][0-9]*0$")) %>%
  arrange(partner_short,
          code_plot,
          survey_year)

}

# 5. Add stratifiers ----

if (level == "LI") {

  source("./src/functions/get_stratifiers.R")
  s1_strat <- get_stratifiers(level = "LI")

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






# 6. Prepare files to share with partners ----

dir <- "./output/stocks/20231207_to_share_with_partners/"

# Some German plot_ids need to be excluded in LI because
# they concern different plots

german_plot_ids_exclude <-
  c("4_259", "4_260", "4_266", "4_267", "4_283", "4_284", "4_285",
    "4_286", "4_287", "4_288", "4_300", "4_306", "4_307" )

# Some columns need to be excluded because not informative

cols_to_exclude <- c("origin",
                     "q_flag",
                     "line_nr",
                     "qif_key",
                     "unique_survey",
                     "unique_survey_repetition",
                     "unique_survey_layer",
                     "unique_layer_repetition",
                     "unique_layer",
                     "origin_merged",
                     "origin_merge_info",
                     "layer_number_bg",
                     "layer_number_ff",
                     "organic_layer_weight_wrong_unit",
                     "sum_texture",
                     "bulk_density_layer_weight",
                     "horizon_number",
                     "unique_survey_profile",
                     "colour_moist_hex")

## 6.1. s1_som ----

s1_som_output <- s1_som %>%
  filter(!plot_id %in% german_plot_ids_exclude) %>%
  select(-any_of(cols_to_exclude)) %>%
  relocate(plot_id, .after = code_plot) %>%
  relocate(layer_thickness, .after = layer_limit_inferior) %>%
  mutate(profile_id = paste0(survey_year, "_",
                             plot_id, "_",
                             repetition)) %>%
  relocate(profile_id, .after = repetition) %>%
  relocate(date_labor_analyses, .before = change_date) %>%
  relocate(download_date, .after = change_date) %>%
  rename(code_layer_orig = code_layer_original) %>%
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
    contains("_orig"),
    contains("_loq"),
    contains("_rt"))

cat(names(s1_som_output), sep = "\n")

write.table(s1_som_output,
            file = paste0(dir, "s1_som_layer1.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")



## 6.2. so_som ----

so_som_output <- so_som %>%
  filter(!plot_id %in% german_plot_ids_exclude) %>%
  select(-any_of(cols_to_exclude)) %>%
  relocate(plot_id, .after = code_plot) %>%
  relocate(layer_thickness, .after = layer_limit_inferior) %>%
  mutate(profile_id = paste0(survey_year, "_",
                             plot_id, "_",
                             repetition)) %>%
  relocate(profile_id, .after = repetition) %>%
  relocate(date_labor_analyses, .before = change_date) %>%
  relocate(download_date, .after = change_date) %>%
  rename(code_layer_orig = code_layer_original) %>%
  relocate(sum_acid_cations, .after = base_saturation) %>%
  relocate(sum_base_cations, .after = base_saturation) %>%
  relocate(c_to_n_ratio, .after = base_saturation) %>%
  relocate(organic_layer_weight, .after = bulk_density) %>%
  relocate(partner_code, .after = code_country) %>%
  select(
    # Selecting columns without the specified patterns
    which(!grepl("_rt|_loq|_orig|_source", names(.))),
    # Selecting columns with the specified patterns in the desired order
    contains("_source"),
    contains("_orig"),
    contains("_loq"),
    contains("_rt"))

cat(names(so_som_output), sep = "\n")

write.table(so_som_output,
            file = paste0(dir, "so_som_layer1.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")



## 6.3. s1_pfh ----

s1_pfh_output <- s1_pfh %>%
  filter(!plot_id %in% german_plot_ids_exclude) %>%
  select(-any_of(cols_to_exclude)) %>%
  relocate(plot_id, .after = code_plot) %>%
  relocate(bulk_density, .before = horizon_bulk_dens_measure) %>%
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
  relocate(coarse_fragment_vol, .after = horizon_bulk_dens_est) %>%
  relocate(code_horizon_coarse_vol, .after = coarse_fragment_vol) %>%
  relocate(horizon_coarse_weight, .after = coarse_fragment_vol) %>%
  select(
    # Selecting columns without the specified patterns
    which(!grepl("_rt|_loq|_orig|_source", names(.))),
    # Selecting columns with the specified patterns in the desired order
    contains("_source"),
    contains("_orig"),
    contains("_loq"),
    contains("_rt"))

cat(names(s1_pfh_output), sep = "\n")

write.table(s1_pfh_output,
            file = paste0(dir, "s1_pfh_layer1.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")





## 6.4. so_pfh ----

so_pfh_output <- so_pfh %>%
  filter(!plot_id %in% german_plot_ids_exclude) %>%
  select(-any_of(cols_to_exclude)) %>%
  relocate(plot_id, .after = code_plot) %>%
  relocate(bulk_density, .before = horizon_bulk_dens_measure) %>%
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
  relocate(coarse_fragment_vol, .after = horizon_bulk_dens_est) %>%
  relocate(code_horizon_coarse_vol, .after = coarse_fragment_vol) %>%
  relocate(horizon_coarse_weight, .after = coarse_fragment_vol) %>%
  select(
    # Selecting columns without the specified patterns
    which(!grepl("_rt|_loq|_orig|_source", names(.))),
    # Selecting columns with the specified patterns in the desired order
    contains("_source"),
    contains("_orig"),
    contains("_loq"),
    contains("_rt"))

cat(names(so_pfh_output), sep = "\n")

write.table(so_pfh_output,
            file = paste0(dir, "so_pfh_layer1.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")



## 6.5. s1 profile stocks ----

df_stocks_li_output <- df_stocks_li %>%
  filter(!plot_id %in% german_plot_ids_exclude) %>%
  mutate(feedback_experts = NA) %>%
  format_stocks()

source("./src/functions/save_excel.R")
save_excel(df_stocks_li_output,
           file = paste0(dir, "s1_profile_soc_stocks.xlsx"))


## 6.6. s1 plot stocks ----

df_stocks_plot_li_output <- df_stocks_plot_li %>%
  filter(!plot_id %in% german_plot_ids_exclude) %>%
  select(-partner_short) %>%
  left_join(d_partner %>%
              select(code, desc_short),
            by = join_by("partner_code" == "code")) %>%
  rename(partner_short = desc_short) %>%
  relocate(partner_short, .after = survey_form) %>%
  mutate(feedback_experts = NA) %>%
  format_stocks()

source("./src/functions/save_excel.R")
save_excel(df_stocks_plot_li_output,
           file = paste0(dir, "s1_plot_soc_stocks.xlsx"))



## 6.7. so profile stocks ----

df_stocks_lii_output <- df_stocks_lii %>%
  filter(!plot_id %in% german_plot_ids_exclude) %>%
  mutate(feedback_experts = NA) %>%
  format_stocks()

source("./src/functions/save_excel.R")
save_excel(df_stocks_lii_output,
           file = paste0(dir, "so_profile_soc_stocks.xlsx"))


## 6.8. so plot stocks ----

df_stocks_plot_lii_output <- df_stocks_plot_lii %>%
  filter(!plot_id %in% german_plot_ids_exclude) %>%
  select(-partner_short) %>%
  left_join(d_partner %>%
              select(code, desc_short),
            by = join_by("partner_code" == "code")) %>%
  rename(partner_short = desc_short) %>%
  relocate(partner_short, .after = survey_form) %>%
  mutate(feedback_experts = NA) %>%
  format_stocks()

source("./src/functions/save_excel.R")
save_excel(df_stocks_plot_lii_output,
           file = paste0(dir, "so_plot_soc_stocks.xlsx"))


## 6.9. Additional manual corrections ----

path <- "./data/additional_data/additional_manual_corrections_fscc.xlsx"

assertthat::assert_that(file.exists(path))

df_to_correct <-
  openxlsx::read.xlsx(path,
                      sheet = 1) %>%
  mutate(code_line = as.character(code_line)) %>%
  rename(observation_date = change_date) %>%
  # Assuming the Excel epoch is 1899-12-30
  mutate(observation_date = as.Date(.data$observation_date - 1,
                                    origin = "1899-12-30")) %>%
  mutate(observation_date =
           as.Date(parsedate::parse_iso_8601(.data$observation_date)))


source("./src/functions/save_excel.R")
save_excel(df_to_correct %>%
             filter(str_starts(survey_form, "so")),
           file = paste0(dir, "so_additional_manual_corrections.xlsx"))

save_excel(df_to_correct %>%
             filter(str_starts(survey_form, "s1")),
           file = paste0(dir, "s1_additional_manual_corrections.xlsx"))






# 7. Make a graph per plot_id ----

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









