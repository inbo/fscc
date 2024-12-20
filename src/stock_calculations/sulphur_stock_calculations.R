


# Calculate sulphur stocks until 100 cm (or until the effective soil depth
# if shallower)
# ---------------------------------------------------------------------------

# Details: In this script, validated and gap-filled "layer 1+" solid soil data
# are processed to calculate soil sulphur stocks using different functions.
# The detailed steps taken in each function can be found within the "Roxygen"
# documentation of the function, i.e. on top of the function scripts
# (in "./src/stock_calculations/functions/" folder).

# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O

# Input data: "./data/layer1_data/"
# Output data: "./output/stocks/"



# 1. Load packages and functions ----

stopifnot(require("tidyverse"),
          require("sf"),
          require("assertthat"),
          require("mpspline2"),
          require("ggplot2"),
          require("rempsyc"),
          require("patchwork"),
          require("xgboost"),
          require("boot"),
          require("bootstrap"),
          require("triangle"),
          require("broom"))

source("./src/functions/get_env.R")


# 2. Specify level ----

# Only run the line of the ICP Forests level for which you would like
# to run the script

level <- "LI"
level <- "LII"
calculate_new_stocks <- FALSE # FALSE if you prefer to import
# the most recently calculated stock data;
# TRUE if you prefer to recalculate the stocks
shorter_var_name <- "extrac_s"


# Import data

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)


# Get stocks

if (calculate_new_stocks == TRUE) {

  # 3. Calculate P stocks ----

  if (level == "LI") {

    source("./src/stock_calculations/functions/get_stocks.R")

    get_stocks(survey_form = "s1",
               data_frame = NULL,
               parameter = "extrac_s",
               constant_subsoil = TRUE,
               exclude_ol = TRUE,
               graph = TRUE,
               add_stratifiers = TRUE,
               density_per_three_cm = TRUE,
               add_plausible_fscc = TRUE,
               save_to_env = TRUE,
               save_to_gdrive = TRUE)

  }


  if (level == "LII") {

    source("./src/stock_calculations/functions/get_stocks.R")

    get_stocks(survey_form = "so",
               data_frame = NULL,
               parameter = "extrac_s",
               constant_subsoil = TRUE,
               exclude_ol = TRUE,
               graph = TRUE,
               add_stratifiers = TRUE,
               density_per_three_cm = TRUE,
               add_plausible_fscc = TRUE,
               save_to_env = TRUE,
               save_to_gdrive = TRUE)

  }

  # If calculate_new_stocks is FALSE

} else {


  # Import most recent stocks

  dir <- paste0(list.dirs("./output/stocks", recursive = FALSE)[
    grepl("_extrac_s_stocks",
          list.dirs("./output/stocks", recursive = FALSE))], "/")

  if (length(dir) > 1) {

    # Check if there are csv files inside

    # Function to check if CSV files exist in the top-level folder

    check_csv_files <- function(folder) {

      csv_files <- list.files(folder,
                              pattern = "\\.csv$",
                              full.names = TRUE,
                              recursive = FALSE)

      # TRUE if there are any CSV files, FALSE otherwise
      return(length(csv_files) > 0)
    }

    # Apply the function to all folders and create a vector of TRUE/FALSE

    csv_check <- sapply(dir, check_csv_files)

    dir <- dir[which(csv_check)]

    assertthat::assert_that(length(dir) >= 1)

    # Get the dates from the folder names

    dates <- as.Date(sapply(strsplit(basename(dir), "_"), `[`, 1),
                     format = "%Y%m%d")

    dir <- dir[which.max(dates)]

  }




  for (survey_form_i in c("s1", "so")) {

    # Layers

    data <- read.csv(paste0(dir,
                            survey_form_i,
                            "_layers.csv"),
                     sep = ";")

    assign_env(paste0(survey_form_i, "_layers"),
               data)

    # Profile stocks

    data <- read.csv(paste0(dir,
                            survey_form_i,
                            "_profile_extrac_s_stocks.csv"),
                     sep = ";")

    assign_env(paste0(survey_form_i, "_profile_extrac_s_stocks"),
               data)

    # Plot stocks

    data <- read.csv(paste0(dir,
                            survey_form_i,
                            "_plot_extrac_s_stocks.csv"),
                     sep = ";")

    assign_env(paste0(survey_form_i, "_plot_extrac_s_stocks"),
               data)

  }

}







plot_stocks <- get_env(paste0("so_som", "_plot_",
                              shorter_var_name, "_stocks"))



# 8. Data visualisation ----

## 8.2. Plots per stratifier ----

plot_extrac_s_stocks <-
  # bind_rows(
  #   s1_plot_c_stocks,
  #   so_plot_c_stocks
  # ) %>%
  plot_stocks %>%
  filter(grepl("som", survey_form)) %>%
  mutate(
    stock = ifelse(
      stock_plaus == TRUE,
      stock,
      NA_real_),
    stock_below_ground = ifelse(
      stock_below_ground_plaus == TRUE,
      stock_below_ground,
      NA_real_),
    stock_topsoil = ifelse(
      stock_topsoil_plaus == TRUE,
      stock_topsoil,
      NA_real_),
    stock_below_ground_topsoil = ifelse(
      stock_below_ground_topsoil_plaus == TRUE,
      stock_below_ground_topsoil,
      NA_real_),
    stock_forest_floor = ifelse(
      stock_forest_floor_plaus == TRUE,
      coalesce(stock_forest_floor,
               0),
      NA_real_)) %>%
  group_by(plot_id, survey_form,
           # partner_short, partner_code,
           code_country, code_plot,
           latitude_dec, longitude_dec, mat, map, slope, altitude,
           wrb_ref_soil_group, eftc, humus_form, parent_material,
           biogeographical_region, main_tree_species, bs_class) %>%
  reframe(
    stock_change = calculate_slope(survey_year, stock),
    stock_change_min = calculate_slope(survey_year, stock_min),
    stock_change_max = calculate_slope(survey_year, stock_max),
    int_stock = calculate_intercept(survey_year, stock),
    int_stock_min = calculate_intercept(survey_year, stock_min),
    int_stock_max = calculate_intercept(survey_year, stock_max),
    survey_year_earliest = min(survey_year),
    survey_year_last = max(survey_year),
    survey_year_difference = max(survey_year) - min(survey_year),
    survey_year_count = n_distinct(survey_year),
    stock_earliest = ifelse(
      any(!is.na(stock)),
      stock[which.min(survey_year[!is.na(stock)])],
      NA_real_),
    stock_last = ifelse(
      any(!is.na(stock)),
      stock[which.max(survey_year[!is.na(stock)])],
      NA_real_),
    stock = ifelse(
      any(!is.na(stock)),
      mean(stock, na.rm = TRUE),
      NA_real_),
    stock_below_ground = ifelse(
      any(!is.na(stock_below_ground)),
      mean(stock_below_ground, na.rm = TRUE),
      NA_real_),
    stock_topsoil = ifelse(
      any(!is.na(stock_topsoil)),
      mean(stock_topsoil, na.rm = TRUE),
      NA_real_),
    stock_below_ground_topsoil = ifelse(
      any(!is.na(stock_below_ground_topsoil)),
      mean(stock_below_ground_topsoil, na.rm = TRUE),
      NA_real_),
    stock_forest_floor = ifelse(
      any(!is.na(stock_forest_floor)),
      mean(stock_forest_floor, na.rm = TRUE),
      NA_real_),
    contains_peat =
      any(contains_peat == TRUE)) %>%
  # Since data from the '90s are often methodologically incomparible
  # filter(survey_year_last >= 2000)
  ungroup() %>%
  rowwise() %>%
  mutate(
    di_ff_top = ifelse(
      !is.na(stock_forest_floor) & !is.na(stock_below_ground_topsoil),
      stock_forest_floor / stock_below_ground_topsoil,
      NA_real_),
    di_top_soil = ifelse(
      !is.na(stock_below_ground_topsoil) & !is.na(stock_below_ground),
      stock_below_ground_topsoil / stock_below_ground,
      NA_real_)) %>%
  ungroup()



assert_that(all(c("wrb_ref_soil_group",
                  "eftc",
                  "humus_form",
                  "biogeographical_region") %in% names(plot_n_stocks)))

source("./src/functions/graph_interval.R")


### Humus form ----

graph_interval(data = plot_extrac_s_stocks %>%
                 filter(!is.na(stock_forest_floor)) %>%
                 mutate(humus_form = case_when(
                   grepl("Histo", humus_form) |
                     humus_form %in% c("Peat", "Anmoor") ~
                     "Semi-terr.",
                   .default = humus_form)) %>%
                 filter(humus_form %in% c("Mull", "Moder", "Mor",
                                          "Amphi", "Semi-terr.")),
               response = "stock_forest_floor",
               group = "humus_form",
               shorter_var_name = shorter_var_name,
               src_survey = "so",
               path_export = paste0(dir, "graphs/"),
               x_max = 130,
               aspect.ratio = 0.35)


### WRB soil group ----

graph_interval(data = plot_extrac_s_stocks %>%
                 filter(!is.na(stock)),
               response = "stock",
               group = "wrb_ref_soil_group",
               shorter_var_name = shorter_var_name,
               src_survey = "so",
               path_export = paste0(dir, "graphs/"),
               x_max = 5000,
               number_of_groups = 10,
               aspect.ratio = 0.7)

### EFTC ----

d_forest_type <- read.csv("./data/additional_data/d_forest_type.csv",
                          sep = ";")

graph_interval(data = plot_extrac_s_stocks %>%
                 filter(!is.na(stock)) %>%
                 left_join(d_forest_type %>%
                             select(code,
                                    short_descr, very_short_descr) %>%
                             rename(eftc = short_descr) %>%
                             rename(eftc_short = very_short_descr) %>%
                             rename(eftc_code = code),
                           by = "eftc") %>%
                 mutate(eftc = paste0(eftc_code, ". ",
                                      eftc_short, "<br>")),
               response = "stock",
               group = "eftc",
               shorter_var_name = shorter_var_name,
               src_survey = "so",
               path_export = paste0(dir, "graphs/"),
               x_max = 30,
               number_of_groups = 13,
               aspect.ratio = 1,
               return = TRUE)








## 8.3. Make a map ----

### 8.3.1. Mean ----

source("./src/functions/map_icpf2.R")
source("./src/functions/as_sf.R")

# data_s1_full <- plot_n_stocks %>%
#   filter(grepl("s1_", survey_form)) %>%
#   filter(!is.na(latitude_dec) &
#            !is.na(longitude_dec)) %>%
#   as_sf

data_so_full <- plot_extrac_s_stocks %>%
  filter(grepl("so_", survey_form)) %>%
  filter(!is.na(latitude_dec) &
           !is.na(longitude_dec)) %>%
  as_sf

assertthat::assert_that(n_distinct(data_so$plot_id) == nrow(data_so))

plot_extrac_s_stocks %>%
  filter(!is.na(stock)) %>%
  filter(grepl("so_", survey_form)) %>%
  mutate(code_country = as.factor(code_country)) %>%
  select(survey_year_earliest, survey_year_last, survey_year_difference,
         survey_year_count, code_country) %>%
  summary


# Level II

data_so <- data_so_full %>%
  filter(!is.na(stock_topsoil))

map_icpf2(layers = "data_so",
          title = paste0("**Forest soil phosphorus stock**",
                         " · ",
                         "Level II<br>",
                         n_distinct(data_so$plot_id), " plots ",
                         "(forest floor + topsoil)<br>",
                         "Mean over survey period (1990 - 2023)"
          ),
          legend_title = paste0("**Extractable<br>phosphorus stock**<br>",
                                "kg P ha<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_topsoil",
          point_size = 0.8,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 1.1,
          y_max = 5000,
          sigma = 200,
          export_name = "so_mean",
          export_folder = paste0(dir, "graphs/"))


data_so <- data_so_full %>%
  filter(!is.na(stock))

map_icpf2(layers = "data_so",
          title = paste0("**Forest soil phosphorus stock**",
                         " · ",
                         "Level II<br>",
                         n_distinct(data_so$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "Mean over survey period (1990 - 2023)"
          ),
          legend_title = paste0("**Extractable<br>phosphorus stock**<br>",
                                "kg N ha<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock",
          point_size = 0.8,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 1.1,
          y_max = 5000,
          sigma = 200,
          export_name = "so_soildepth_mean",
          export_folder = paste0(dir, "graphs/"))




### 8.3.2. Change ----

data_so <- data_so_full %>%
  filter(!is.na(stock_change))

map_icpf2(layers = "data_so",
          title = paste0("**Annual increase** in **forest ",
                         "soil phosphorus stock** ",
                         " · Level II<br>",
                         n_distinct(data_so$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "1991 - 2023 (time span per plot: 9 - 29 years)"),
          legend_title = paste0("**Annual increase**<br>",
                                "extractable<br>phosphorus stock<br>",
                                "kg N ha<sup>-1</sup> year<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_change",
          point_size = 1.5,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 0.9,
          y_change_max = 100,
          export_name = "so_change",
          export_folder = paste0(dir, "graphs/"))



### 8.3.3. Phosphorus Distribution Index (PDI) ----

source("./src/functions/graph_interval.R")

# Forest floor to below-ground topsoil

graph_interval(data = plot_extrac_s_stocks %>%
                 filter(!is.na(di_ff_top)),
               response = "di_ff_top",
               group = "wrb_ref_soil_group",
               shorter_var_name = shorter_var_name,
               src_survey = "so",
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 2,
               number_of_groups = 10,
               aspect.ratio = 0.7)

graph_interval(data = plot_extrac_s_stocks %>%
                 filter(!is.na(di_ff_top)) %>%
                 mutate(humus_form = case_when(
                   grepl("Histo", humus_form) |
                     humus_form %in% c("Peat", "Anmoor") ~
                     "Semi-terr.",
                   .default = humus_form)) %>%
                 filter(humus_form %in% c("Mull", "Moder", "Mor",
                                          "Amphi", "Semi-terr.")),
               response = "di_ff_top",
               group = "humus_form",
               shorter_var_name = shorter_var_name,
               src_survey = "so",
               path_export = paste0(dir, "graphs/"),
               x_max = 2,
               aspect.ratio = 0.35)


# Below-ground topsoil to below-ground

graph_interval(data = plot_extrac_s_stocks %>%
                 filter(!is.na(di_top_soil)),
               response = "di_top_soil",
               group = "wrb_ref_soil_group",
               shorter_var_name = shorter_var_name,
               src_survey = "so",
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 1,
               number_of_groups = 10,
               aspect.ratio = 0.7)


graph_interval(data = plot_extrac_s_stocks %>%
                 filter(!is.na(di_top_soil)) %>%
                 filter(!is.na(eftc)) %>%
                 left_join(d_forest_type %>%
                             select(code,
                                    short_descr, very_short_descr) %>%
                             rename(eftc = short_descr) %>%
                             rename(eftc_short = very_short_descr) %>%
                             rename(eftc_code = code),
                           by = "eftc") %>%
                 mutate(eftc = paste0(eftc_code, ". ",
                                      eftc_short, "<br>")),
               response = "di_top_soil",
               group = "eftc",
               shorter_var_name = shorter_var_name,
               src_survey = "so",
               path_export = paste0(dir, "graphs/"),
               x_max = 1,
               number_of_groups = 13,
               aspect.ratio = 1,
               return = TRUE)






