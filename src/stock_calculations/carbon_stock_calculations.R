
# Calculate carbon stocks until 100 cm (or until the effective soil depth
# if shallower)
# ---------------------------------------------------------------------------

# Details: In this script, validated and gap-filled "layer 1+" solid soil data
# are processed to calculate soil carbon stocks using different functions.
# The detailed steps taken in each function can be found within the "Roxygen"
# documentation of the function, i.e. on top of the function scripts
# (in "./src/stock_calculations/functions/" folder).

# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O

# Input data: "./data/layer1_data/"
# Output data: "./output/stocks/"



# 1. Load packages and functions ----

stopifnot(require("tidyverse"),
          require("assertthat"),
          require("mpspline2"),
          require("ggplot2"))

source("./src/functions/get_env.R")


# 2. Specify level ----

# Only run the line of the ICP Forests level for which you would like
# to run the script

level <- "LI"
level <- "LII"


# 3. Calculate stocks ----

# Import data

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)



if (level == "LI") {

  source("./src/stock_calculations/functions/get_stocks.R")

  get_stocks(survey_form = "s1",
             data_frame = NULL,
             parameter = "organic_carbon_total",
             constant_subsoil = TRUE,
             exclude_ol = TRUE,
             graph = TRUE,
             add_stratifiers = TRUE,
             density_per_three_cm = TRUE,
             save_to_env = TRUE,
             save_to_gdrive = TRUE)

}


if (level == "LII") {

  source("./src/stock_calculations/functions/get_stocks.R")

  get_stocks(survey_form = "so",
             data_frame = NULL,
             parameter = "organic_carbon_total",
             constant_subsoil = TRUE,
             exclude_ol = TRUE,
             graph = TRUE,
             add_stratifiers = TRUE,
             density_per_three_cm = TRUE,
             save_to_env = TRUE,
             save_to_gdrive = TRUE)

}





# 4. Compile data per level ----

if (level == "LI") {

  # df_layer_li <-
  #   bind_rows(s1_som_below_ground %>%
  #               mutate(survey_form = "s1_som") %>%
  #               relocate(survey_form, .before = partner_short) %>%
  #               mutate(repetition = as.character(repetition)) %>%
  #               select(-avail_thick,
  #                      -avail_toc,
  #                      -avail_bd,
  #                      -avail_cf),
  #             s1_pfh_below_ground %>%
  #               mutate(survey_form = "s1_pfh") %>%
  #               relocate(survey_form, .before = partner_short) %>%
  #               select(-avail_thick,
  #                      -avail_toc,
  #                      -avail_bd,
  #                      -avail_cf),
  #             s1_som_forest_floor %>%
  #               mutate(survey_form = "s1_som") %>%
  #               relocate(survey_form, .before = partner_short) %>%
  #               relocate(density, .before = stock_layer) %>%
  #               mutate(repetition = as.character(repetition)) %>%
  #               select(-avail_thick,
  #                      -avail_toc,
  #                      -avail_bd,
  #                      -avail_org_layer_weight),
  #             s1_pfh_forest_floor %>%
  #               mutate(survey_form = "s1_pfh") %>%
  #               relocate(survey_form, .before = partner_short) %>%
  #               relocate(density, .before = stock_layer) %>%
  #               select(-avail_thick,
  #                      -avail_toc,
  #                      -avail_bd,
  #                      -avail_org_layer_weight)) %>%
  #   mutate(profile_id_form = paste0(survey_form, "_",
  #                                   profile_id)) %>%
  #   relocate(profile_id_form, .after = profile_id) %>%
  #   arrange(partner_short,
  #           code_plot,
  #           survey_year,
  #           profile_id,
  #           layer_number)
  #
  #
  # df_stocks_li <-
  #   bind_rows(s1_som_profile_c_stocks %>%
  #               mutate(survey_form = "s1_som") %>%
  #               relocate(survey_form, .before = partner_short) %>%
  #               mutate(repetition = as.character(repetition)),
  #             s1_pfh_profile_c_stocks %>%
  #               mutate(survey_form = "s1_pfh") %>%
  #               relocate(survey_form, .before = partner_short)) %>%
  #   as_tibble() %>%
  #   mutate(profile_id_form = paste0(survey_form, "_",
  #                                   profile_id)) %>%
  #   relocate(profile_id_form, .after = profile_id) %>%
  #   rename(use_stock_topsoil = use_stock_until_30) %>%
  #   # select(-matches("^stock_[1-9][0-9]*0$")) %>%
  #   # select(-stock_ol, -stock_ofh) %>%
  #   arrange(partner_short,
  #           code_plot,
  #           survey_year,
  #           profile_id)
  #
  # df_stocks_plot_li <-
  #   bind_rows(s1_som_plot_c_stocks %>%
  #               mutate(survey_form = "s1_som") %>%
  #               relocate(survey_form, .before = partner_short),
  #             s1_pfh_plot_c_stocks %>%
  #               mutate(survey_form = "s1_pfh") %>%
  #               relocate(survey_form, .before = partner_short)) %>%
  #   rename(use_stock_topsoil = use_stock_until_30) %>%
  #   select(-matches("^stock_[1-9][0-9]*0$")) %>%
  #   arrange(partner_short,
  #           code_plot,
  #           survey_year)

}





if (level == "LII") {

# df_layer_lii <-
#   bind_rows(so_som_below_ground %>%
#               mutate(survey_form = "so_som") %>%
#               relocate(survey_form, .before = partner_short) %>%
#               mutate(repetition = as.character(repetition)) %>%
#               select(-avail_thick,
#                      -avail_toc,
#                      -avail_bd,
#                      -avail_cf),
#             so_pfh_below_ground %>%
#               mutate(survey_form = "so_pfh") %>%
#               relocate(survey_form, .before = partner_short) %>%
#               select(-avail_thick,
#                      -avail_toc,
#                      -avail_bd,
#                      -avail_cf),
#             so_som_forest_floor %>%
#               mutate(survey_form = "so_som") %>%
#               relocate(survey_form, .before = partner_short) %>%
#               relocate(density, .before = stock_layer) %>%
#               mutate(repetition = as.character(repetition)) %>%
#               select(-avail_thick,
#                      -avail_toc,
#                      -avail_bd,
#                      -avail_org_layer_weight),
#             so_pfh_forest_floor %>%
#               mutate(survey_form = "so_pfh") %>%
#               relocate(survey_form, .before = partner_short) %>%
#               relocate(density, .before = stock_layer) %>%
#               select(-avail_thick,
#                      -avail_toc,
#                      -avail_bd,
#                      -avail_org_layer_weight)) %>%
#   mutate(profile_id_form = paste0(survey_form, "_",
#                                   profile_id)) %>%
#   relocate(profile_id_form, .after = profile_id) %>%
#   arrange(partner_short,
#           code_plot,
#           survey_year,
#           profile_id,
#           layer_number)
#
#
# df_stocks_lii <-
#   bind_rows(so_som_profile_c_stocks %>%
#               mutate(survey_form = "so_som") %>%
#               relocate(survey_form, .before = partner_short) %>%
#               mutate(repetition = as.character(repetition)),
#             so_pfh_profile_c_stocks %>%
#               mutate(survey_form = "so_pfh") %>%
#               relocate(survey_form, .before = partner_short)) %>%
#   as_tibble() %>%
#   mutate(profile_id_form = paste0(survey_form, "_",
#                                   profile_id)) %>%
#   relocate(profile_id_form, .after = profile_id) %>%
#   rename(use_stock_topsoil = use_stock_until_30) %>%
#   # select(-matches("^stock_[1-9][0-9]*0$")) %>%
#   # select(-stock_ol, -stock_ofh) %>%
#   arrange(partner_short,
#           code_plot,
#           survey_year,
#           profile_id)



# df_stocks_plot_lii <-
#   bind_rows(so_som_plot_c_stocks %>%
#               mutate(survey_form = "so_som") %>%
#               relocate(survey_form, .before = partner_short),
#             so_pfh_plot_c_stocks %>%
#               mutate(survey_form = "so_pfh") %>%
#               relocate(survey_form, .before = partner_short)) %>%
#   rename(use_stock_topsoil = use_stock_until_30) %>%
#   select(-matches("^stock_[1-9][0-9]*0$")) %>%
#   arrange(partner_short,
#           code_plot,
#           survey_year)

}


# x. Add national carbon stock estimates ----

national_carbon_stocks_harmonised <-
  read.csv(paste0("./data/additional_data/national_coauthor_carbon_stocks/",
                  "national_carbon_stocks_harmonised.csv"),
           sep = ";")

# ...(see sandbox)





# 5. Add stratifiers ----
# (For statistics/visualisation purposes)

if (level == "LI") {

  # source("./src/functions/get_stratifiers.R")
  # s1_strat <- get_stratifiers(level = "LI")
  #
  # source("./src/functions/save_to_google_drive.R")
  # save_to_google_drive(objects_to_save = "s1_strat",
  #                      path_name = "layer1_data")
  #
  # s1_strat <- s1_strat %>%
  #   select(plot_id, longitude_dec, latitude_dec, slope, eff_soil_depth,
  #          wrb_soil_group, forest_type, humus_type,
  #          parent_material, biogeo,
  #          main_tree_species, bs_class)
  #
  # colSums(!is.na(s1_strat))
  #
  #
  # # Add stratifiers to the data
  #
  # df_stocks_plot_li <- df_stocks_plot_li %>%
  #   left_join(s1_strat,
  #             by = "plot_id")
  #
  #
  # df_stocks_plot_li <- df_stocks_plot_li %>%
  #   mutate(humus_type = case_when(
  #     humus_type == "Amphi (or Amphihumus)" ~ "Amphi",
  #     humus_type %in% c("Histomull", "Histomoder") ~ "Peat",
  #     TRUE ~ humus_type))
}


if (level == "LII") {

  # source("./src/functions/get_stratifiers.R")
  # so_strat <- get_stratifiers(level = "LII")
  #
  # source("./src/functions/save_to_google_drive.R")
  # save_to_google_drive(objects_to_save = "so_strat",
  #                      path_name = "layer1_data")
  #
  # so_strat <- so_strat %>%
  #   select(plot_id, longitude_dec, latitude_dec, slope, eff_soil_depth,
  #          rooting_depth,
  #          wrb_soil_group, forest_type, humus_type,
  #          parent_material, biogeo,
  #          main_tree_species, bs_class)
  #
  # colSums(!is.na(so_strat))
  #
  #
  # # Add stratifiers to the data
  #
  # df_stocks_plot_lii <- df_stocks_plot_lii %>%
  #   left_join(so_strat,
  #             by = "plot_id")
  #
  #
  #
  # # Format and print the names for attribute catalogue:
  #
  # names_list <-
  #   sort(unique(c(names(so_strat))))
  #
  # cat(paste0("-   ", names_list, ": \n "), sep = "\n")
  #
  #
  # df_layer_lii <- df_layer_lii %>%
  #   left_join(so_strat,
  #             by = "plot_id")
  #
  # df_stocks_lii <- df_stocks_lii %>%
  #   left_join(so_strat,
  #             by = "plot_id")

  # Add colours (for visualisation)

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
# they concern different non-unique plots across different Länder

# german_plot_ids_exclude <-
#   c("4_259", "4_260", "4_266", "4_267", "4_283", "4_284", "4_285",
#     "4_286", "4_287", "4_288", "4_300", "4_306", "4_307" )

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
 # filter(!plot_id %in% german_plot_ids_exclude) %>%
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


# Format and print the names for attribute catalogue:

names_list <-
  sort(unique(c(names(s1_som_output), names(so_som_output),
                names(s1_pfh_output), names(so_pfh_output))))

cat(paste0("-   **", names_list, "** - "), sep = "\n")






## 6.3. s1_pfh ----

s1_pfh_output <- s1_pfh %>%
 # filter(!plot_id %in% german_plot_ids_exclude) %>%
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


# Format and print the names for attribute catalogue:

names_list <-
  sort(unique(c(names(df_stocks_li_output), names(df_stocks_plot_li_output),
                names(df_stocks_lii_output), names(df_stocks_plot_lii_output))))

cat(paste0("-   **", names_list, "** - "), sep = "\n")




## 6.9. Additional manual corrections ----

path <- "./data/additional_data/additional_manual_corrections_fscc.xlsx"

assertthat::assert_that(file.exists(path))

df_to_correct <-
  openxlsx::read.xlsx(path,
                      sheet = 1) %>%
  mutate(code_line = as.character(code_line)) %>%
  rename(observation_date = change_date) %>%
  # Assuming the Excel epoch is 1899-12-30
  mutate(observation_date = as.Date(.data$observation_date,
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






# 7. Statistics ----

# C stocks until 100

data_frame <-
  bind_rows(df_stocks_li %>%
              filter(use_stock_topsoil == FALSE) %>%
              select(plot_id, c_stock),
            df_stocks_lii %>%
              filter(use_stock_topsoil == FALSE) %>%
              select(plot_id, c_stock)) %>%
  mutate(all = "All")

response <- "c_stock"

group <- "all"

rcompanion_groupwiseMean(
  group = group,
  var = response,
  data = data_frame,
  conf = 0.95,
  digits = 5,
  R = 9100,
  traditional = FALSE,
  bca = TRUE,
  na.rm = TRUE)

#    n   Mean Conf.level Bca.lower Bca.upper
# 9097 134.26       0.95     131.5    137.34


# Below-ground mineral C stocks until 100

min_below_ground <-
  bind_rows(s1_plot_c_stocks %>%
              filter(use_stock_topsoil == FALSE) %>%
              filter(contains_peat == FALSE) %>%
              select(plot_id, stock_below_ground),
            so_plot_c_stocks %>%
              filter(use_stock_topsoil == FALSE) %>%
              filter(contains_peat == FALSE) %>%
              select(plot_id, stock_below_ground)) %>%
  pull(stock_below_ground) %>%
  median

# Below-ground mineral C stocks until 30

min_below_ground_topsoil <-
  bind_rows(s1_plot_c_stocks %>%
              filter(contains_peat == FALSE) %>%
              select(plot_id, stock_below_ground_topsoil),
            so_plot_c_stocks %>%
              filter(contains_peat == FALSE) %>%
              select(plot_id, stock_below_ground_topsoil)) %>%
  pull(stock_below_ground_topsoil) %>%
  median

min_below_ground_topsoil / min_below_ground * 100

# Below-ground peat C stocks until 100

peat_below_ground <-
  bind_rows(s1_plot_c_stocks %>%
              filter(use_stock_topsoil == FALSE) %>%
              filter(contains_peat == TRUE) %>%
              select(plot_id, stock_below_ground),
            so_plot_c_stocks %>%
              filter(use_stock_topsoil == FALSE) %>%
              filter(contains_peat == TRUE) %>%
              select(plot_id, stock_below_ground)) %>%
  pull(stock_below_ground) %>%
  median

# Below-ground peat C stocks until 30

peat_below_ground_topsoil <-
  bind_rows(s1_plot_c_stocks %>%
              filter(contains_peat == TRUE) %>%
              select(plot_id, stock_below_ground_topsoil),
            so_plot_c_stocks %>%
              filter(contains_peat == TRUE) %>%
              select(plot_id, stock_below_ground_topsoil)) %>%
  pull(stock_below_ground_topsoil) %>%
  median

peat_below_ground_topsoil / peat_below_ground * 100



# Forest floor

  bind_rows(s1_plot_c_stocks %>%
              # mutate(c_stock_forest_floor =
              #          ifelse(is.na(c_stock_forest_floor),
              #                 0,
              #                 .data$c_stock_forest_floor)) %>%
              select(plot_id, stock_forest_floor),
            so_plot_c_stocks %>%
                # mutate(c_stock_forest_floor =
                #          ifelse(is.na(c_stock_forest_floor),
                #                 0,
                #                 .data$c_stock_forest_floor)) %>%
              select(plot_id, stock_forest_floor)) %>%
    filter(!is.na(stock_forest_floor)) %>%
    pull(stock_forest_floor) %>%
    median







# To do: NA c stocks forest floor should be 0!







# 8. Data visualisation ----

## 8.1. Make a graph per plot_id ----

# Input

df_layer <- df_layer_lii
df_stocks <- df_stocks_lii

# plot_ids <- unique(df_stocks_lii$plot_id)
# Arrange data

plot_ids <- unique(df_layer$plot_id)

# Evaluate for each of the plot surveys

for (i in seq_along(plot_ids)) {

    df_stocks_lii %>%
    filter(plot_id == plot_ids[22]) %>%
    distinct(profile_id, .keep_all = TRUE) %>%
    mutate(survey_id = paste0(survey_form, "_",
                              survey_year)) %>%
    distinct(survey_id, .keep_all = TRUE)



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



## 8.2. Violin plots per stratifier ----

stopifnot(require("tidyverse"),
          require("assertthat"),
          require("aqp"),
          require("ggplot2"),
          require("boot"),
          require("rempsyc"),
          # require("INBOtheme"),
          require("ggtext"))

assert_that(all(c("wrb_soil_group",
                  "forest_type",
                  "humus_type",
                  "biogeo") %in% names(so_plot_c_stocks)))


source("./src/functions/graph_violin.R")

# graph_violin(data_frame = so_plot_c_stocks,
#              response = "stock",
#              group = "forest_type",
#              path_export = "./output/stocks/20240324_carbon_stocks/")
#
# graph_violin(data_frame = so_plot_c_stocks,
#              response = "c_stock",
#              group = "wrb_soil_group",
#              path_export = "./output/stocks/20240324_carbon_stocks/")



graph_violin(data_frame = s1_plot_c_stocks %>%
               filter(!is.na(stock_forest_floor)),
             data_frame_2 = so_plot_c_stocks %>%
               filter(!is.na(stock_forest_floor)),
             response = "stock_forest_floor",
             group = "humus_type",
             path_export = "./output/stocks/20240324_carbon_stocks/")

graph_violin(data_frame = s1_plot_c_stocks %>%
               filter(use_stock_topsoil == FALSE) %>%
               filter(!is.na(biogeo)) %>%
               rename(biogeographical_region = biogeo),
             data_frame_2 = so_plot_c_stocks %>%
               filter(use_stock_topsoil == FALSE) %>%
               filter(biogeo != "Black Sea") %>%
               rename(biogeographical_region = biogeo),
             response = "stock",
             group = "biogeographical_region",
             path_export = "./output/stocks/20240324_carbon_stocks/")

graph_violin(data_frame = s1_plot_c_stocks %>%
               filter(use_stock_topsoil == FALSE),
             data_frame_2 = so_plot_c_stocks %>%
               filter(use_stock_topsoil == FALSE),
             response = "stock",
             group = "wrb_soil_group",
             path_export = "./output/stocks/20240324_carbon_stocks/")

graph_violin(data_frame = s1_plot_c_stocks %>%
               filter(use_stock_topsoil == FALSE),
             data_frame_2 = so_plot_c_stocks %>%
               filter(use_stock_topsoil == FALSE),
             response = "stock",
             group = "forest_type",
             path_export = "./output/stocks/20240324_carbon_stocks/")






## 8.3. Make a map ----

source("./src/functions/map_icpf.R")

data_s1 <- s1_plot_c_stocks %>%
  filter(grepl("som", survey_form)) %>%
  # filter(use_stock_topsoil == FALSE) %>%
  filter(stock_topsoil <= 400) %>%
  group_by(plot_id) %>%
  slice_max(order_by = survey_year) %>%
  filter(!is.na(latitude_dec) &
           !is.na(longitude_dec)) %>%
  as_sf

# source("./src/sandbox/map_icpf_continuous2.R")
#
# map_icpf2(layers = "data_s1",
#          title = "Forest soil carbon stock (most recent) · **Level I**",
#          legend_title = paste0("**Organic carbon stock**<br>",
#                                "(forest floor + topsoil)<br>",
#                                "t C ha<sup>-1</sup>"),
#          biogeo_palette = NULL,
#          legend_classes = NULL,
#          point_size = 0.3,
#          with_logo = FALSE,
#          count_plots_legend = FALSE,
#          offset_x = 0.18, #1.3,
#          offset_x2 = 0.01, # 0.075,
#          export_name = "c_stock_most_recent_s1",
#          export_folder = "stocks/20240324_carbon_stocks")


data_so <- so_plot_c_stocks %>%
  filter(grepl("som", survey_form)) %>%
  # filter(use_stock_topsoil == FALSE) %>%
  filter(stock_topsoil <= 400) %>%
  group_by(plot_id) %>%
  slice_max(order_by = survey_year) %>%
  filter(!is.na(latitude_dec) &
           !is.na(longitude_dec)) %>%
  as_sf

# source("./src/sandbox/map_icpf_continuous2.R")
#
# map_icpf2(layers = "data_so",
#           title = "Forest soil carbon stock (most recent) · **Level II**",
#           legend_title = paste0("**Organic carbon stock**<br>",
#                                 "(forest floor + topsoil)<br>",
#                                 "t C ha<sup>-1</sup>"),
#           biogeo_palette = NULL,
#           legend_classes = NULL,
#           point_size = 0.6,
#           with_logo = FALSE,
#           count_plots_legend = FALSE,
#           offset_x = 0.18,
#           offset_x2 = 0.01,
#           export_name = "c_stock_most_recent_so",
#           export_folder = "stocks/20240324_carbon_stocks")


map_icpf(layers = "data_so",
         title = "Forest soil carbon stock (most recent) · **Level II**",
         legend_title = paste0("**Organic carbon stock**<br>",
                               "(forest floor + topsoil)<br>",
                               "t C ha<sup>-1</sup>"),
         biogeo_palette = NULL,
         legend_classes = NULL,
         variable_continuous = "stock_topsoil",
         point_size = 0.6,
         with_logo = FALSE,
         count_plots_legend = FALSE,
         inset_maps_offset_x = 0.18,
         export_name = "test",
         export_folder = "stocks/20240324_carbon_stocks")






















