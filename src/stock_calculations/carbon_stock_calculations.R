
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
          require("ggplot2"),
          require("rempsyc"),
          require("patchwork"),
          require("xgboost"))

source("./src/functions/get_env.R")


# 2. Specify level ----

# Only run the line of the ICP Forests level for which you would like
# to run the script

level <- "LI"
level <- "LII"
calculate_new_stocks <- FALSE

# Import data

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)


# Get stocks

if (calculate_new_stocks == TRUE) {

# 3. Calculate stocks ----

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

} else {

  # Import most recent stocks

  dir <- paste0(list.dirs("./output/stocks", recursive = FALSE)[
    grepl("_carbon_stocks",
          list.dirs("./output/stocks", recursive = FALSE))], "/")

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
                            "_profile_c_stocks.csv"),
                     sep = ";")

    assign_env(paste0(survey_form_i, "_profile_c_stocks"),
               data)

    # Plot stocks

    data <- read.csv(paste0(dir,
                            survey_form_i,
                            "_plot_c_stocks.csv"),
                     sep = ";")

    assign_env(paste0(survey_form_i, "_plot_c_stocks"),
               data)

  }

}


# 4. Calculate variables per plot_id ----

# Define a function to calculate slope of linear regression
calculate_slope <- function(x, y) {
  if(length(unique(x)) > 1 &
     # Spanning more than five years
     ((max(x) - min(x)) > 8)) {
    model <- lm(y ~ x)
    coef(model)[[2]]  # Slope coefficient
  } else {
    NA_real_
  }
}

calculate_intercept <- function(x, y) {
  if(length(unique(x)) > 1 &
     # Spanning more than five years
     ((max(x) - min(x)) > 8)) {
    model <- lm(y ~ x)
    coef(model)[[1]]  # Slope coefficient
  } else {
    NA_real_
  }
}

plot_c_stocks_summ <-
  bind_rows(s1_plot_c_stocks,
            so_plot_c_stocks) %>%
  mutate(stock_forest_floor = ifelse(
    is.na(unknown_forest_floor) | unknown_forest_floor == FALSE,
    coalesce(stock_forest_floor,
             0),
    NA_real_)) %>%
  as_tibble() %>%
  # Since data from the '90s are often methodologically incomparible
  # filter(survey_year >= 2000) %>%
  # Since fixed-depth layer data are supposed to be more complete,
  # and because it is better to use data from one sampling approach
  filter(grepl("som", survey_form)) %>%
  filter(use_stock_topsoil == FALSE) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  group_by(plot_id,
           survey_form, partner_short, partner_code,
           code_country, code_plot,
           latitude_dec, longitude_dec, mat, map, slope, altitude,
           wrb_ref_soil_group, eftc, humus_form, parent_material,
           biogeographical_region, main_tree_species, bs_class) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
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
    stock_earliest = stock[which(survey_year == survey_year_earliest)],
    stock_min = min(stock_min),
    stock_max = max(stock_max),
    stock = mean(stock),
    use_stock_topsoil =
      any(use_stock_topsoil == TRUE),
    contains_peat =
      any(contains_peat == TRUE)) %>%
  relocate(stock, .before = stock_min) %>%
  mutate(
    stock_start = (survey_year_earliest * stock_change + int_stock),
    # Net C influx - relative (% ha-1 year-1)
    stock_change_rel = 100 * stock_change /
      # Predicted stock at the start year
      (stock_start),
    stock_change_rel_min = 100 * stock_change_min /
      (survey_year_earliest * stock_change_min + int_stock_min),
    stock_change_rel_max = 100 * stock_change_max /
      (survey_year_earliest * stock_change_max + int_stock_max))








# 5. Add national carbon stock estimates ----

national_carbon_stocks_harmonised <-
  read.csv(paste0("./data/additional_data/national_coauthor_carbon_stocks/",
                  "national_carbon_stocks_harmonised.csv"),
           sep = ";")

# ...(see sandbox)







# 6. Statistics ----

# C stocks until 100

data_frame <-
  bind_rows(so_plot_c_stocks %>%
              filter(use_stock_topsoil == FALSE) %>%
              filter(is.na(unknown_forest_floor) |
                       unknown_forest_floor == FALSE) %>%
              select(plot_id, stock),
            s1_plot_c_stocks %>%
              filter(use_stock_topsoil == FALSE) %>%
              filter(is.na(unknown_forest_floor) |
                       unknown_forest_floor == FALSE) %>%
              select(plot_id, stock)) %>%
  mutate(all = "All")

response <- "stock"

group <- "all"

# Too time-consuming

# rcompanion_groupwiseMean(
#   group = group,
#   var = response,
#   data = data_frame,
#   conf = 0.95,
#   digits = 5,
#   R = 9100,
#   traditional = FALSE,
#   bca = TRUE,
#   na.rm = TRUE)

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
  summary

min_below_ground

# Below-ground mineral C stocks until 30

min_below_ground_topsoil <-
  bind_rows(s1_plot_c_stocks %>%
              filter(contains_peat == FALSE) %>%
              select(plot_id, stock_below_ground_topsoil),
            so_plot_c_stocks %>%
              filter(contains_peat == FALSE) %>%
              select(plot_id, stock_below_ground_topsoil)) %>%
  pull(stock_below_ground_topsoil) %>%
  summary

as.numeric(min_below_ground_topsoil["Median"] /
             min_below_ground["Median"] * 100)

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
  summary

peat_below_ground

# Below-ground peat C stocks until 30

peat_below_ground_topsoil <-
  bind_rows(s1_plot_c_stocks %>%
              filter(contains_peat == TRUE) %>%
              select(plot_id, stock_below_ground_topsoil),
            so_plot_c_stocks %>%
              filter(contains_peat == TRUE) %>%
              select(plot_id, stock_below_ground_topsoil)) %>%
  pull(stock_below_ground_topsoil) %>%
  summary

as.numeric(peat_below_ground_topsoil["Median"] /
             peat_below_ground["Median"] * 100)



# Forest floor

bind_rows(s1_plot_c_stocks %>%
            filter(is.na(unknown_forest_floor) |
                     unknown_forest_floor == FALSE) %>%
            mutate(stock_forest_floor =
                     coalesce(stock_forest_floor, 0)) %>%
            select(plot_id, stock_forest_floor),
          so_plot_c_stocks %>%
            filter(is.na(unknown_forest_floor) |
                     unknown_forest_floor == FALSE) %>%
              mutate(stock_forest_floor =
                       coalesce(stock_forest_floor, 0)) %>%
            select(plot_id, stock_forest_floor)) %>%
  filter(!is.na(stock_forest_floor)) %>%
  pull(stock_forest_floor) %>%
  summary





# Annual changes

plot_c_stocks_summ %>%
  filter(!is.na(stock_change_rel)) %>%
  pull(survey_year_last) %>%
  max

plot_c_stocks_summ %>%
  filter(!is.na(stock_change_rel)) %>%
  filter(contains_peat == FALSE) %>%
  pull(stock_change) %>%
  summary

plot_c_stocks_summ %>%
  filter(!is.na(stock_change_rel)) %>%
  filter(contains_peat == FALSE) %>%
  pull(stock_change) %>%
  quantile(c(0.05, 0.95))

plot_c_stocks_summ %>%
  filter(!is.na(stock_change_rel)) %>%
  filter(contains_peat == FALSE) %>%
  pull(stock_change_rel) %>%
  summary






## Boosted regression tree

plot_c_stocks <-
  bind_rows(s1_plot_c_stocks,
            so_plot_c_stocks) %>%
  mutate(stock_forest_floor = ifelse(
    is.na(unknown_forest_floor) | unknown_forest_floor == FALSE,
    coalesce(stock_forest_floor,
             0),
    NA_real_)) %>%
  filter(!is.na(stock_forest_floor)) %>%
    group_by(plot_id,
             survey_form, partner_short, partner_code,
             code_country, country, code_plot,
             latitude_dec, longitude_dec, mat, map, slope, altitude,
             wrb_ref_soil_group, eftc, humus_form, parent_material,
             biogeographical_region, main_tree_species, bs_class) %>%
    reframe(
      stock_min = min(stock_min),
      stock_max = max(stock_max),
      stock = mean(stock),
      stock_topsoil = mean(stock_topsoil)) %>%
    relocate(stock, .before = stock_min) %>%
    select(stock_topsoil,
           country, latitude_dec, mat, map, altitude, wrb_ref_soil_group,
           eftc, humus_form, parent_material, biogeographical_region,
           main_tree_species, bs_class) %>%
    na.omit()




  # Split the data into training and testing sets
  set.seed(123)  # For reproducibility
  train_idx <- sample(nrow(plot_c_stocks), nrow(plot_c_stocks) * 0.8)
  train_data <- plot_c_stocks[train_idx, ] %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.integer)
  test_data <- plot_c_stocks[-train_idx, ] %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.integer)


  sparse_matrix <- Matrix::sparse.model.matrix(humus_form ~ ., data = test_data)[,-1]


  # Target
  y_train <- as.numeric(train_data$stock_topsoil)
  y_test <- as.numeric(test_data$stock_topsoil)

  # Features
  x_train <- train_data %>% select(-stock_topsoil)
  x_test <- test_data %>% select(-stock_topsoil)

  xgb_train <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
  xgb_test <- xgb.DMatrix(data = as.matrix(x_test), label = y_test)

  xgb_params <- list(
    booster = "gbtree",
    eta = 0.01,
    max_depth = 8,
    gamma = 4,
    subsample = 0.75,
    colsample_bytree = 1,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = length(levels(iris$Species))
  )


  xgb_model <- xgb.train(params = )







# 8. Data visualisation ----

## 8.2. Plots per stratifier ----

plot_c_stocks <-
  bind_rows(s1_plot_c_stocks,
            so_plot_c_stocks) %>%
  mutate(stock_forest_floor = ifelse(
    is.na(unknown_forest_floor) | unknown_forest_floor == FALSE,
    coalesce(stock_forest_floor,
             0),
    NA_real_)) %>%
  group_by(plot_id,
           partner_short, partner_code,
           code_country, country, code_plot,
           latitude_dec, longitude_dec, mat, map, slope, altitude,
           wrb_ref_soil_group, eftc, humus_form, parent_material,
           biogeographical_region, main_tree_species, bs_class) %>%
  reframe(
    stock_min = min(stock_min),
    stock_max = max(stock_max),
    stock = mean(stock),
    stock_topsoil = mean(stock_topsoil),
    stock_below_ground = mean(stock_below_ground),
    stock_below_ground_topsoil = mean(stock_below_ground_topsoil),
    stock_forest_floor = mean(stock_forest_floor),
    use_stock_topsoil =
      any(use_stock_topsoil == TRUE))

assert_that(all(c("wrb_ref_soil_group",
                  "eftc",
                  "humus_form",
                  "biogeographical_region") %in% names(plot_c_stocks)))

source("./src/functions/graph_interval.R")

dir <- paste0(list.dirs("./output/stocks", recursive = FALSE)[
  grepl("_carbon_stocks",
        list.dirs("./output/stocks", recursive = FALSE))], "/")


### Humus form ----

p1 <- graph_interval(data = plot_c_stocks %>%
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
               path_export = paste0(dir, "graphs/"),
               mode = "dark",
               version = "dark",
               x_max = 150,
               width = 5,
               aspect.ratio = 0.4,
               return = TRUE)

graph_interval(data = plot_c_stocks %>%
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
               path_export = paste0(dir, "graphs/"),
               x_max = 88,
               aspect.ratio = 0.35)


### WRB soil group ----

plot_c_stocks <-
  bind_rows(s1_plot_c_stocks,
            so_plot_c_stocks) %>%
  mutate(stock_forest_floor = ifelse(
    is.na(unknown_forest_floor) | unknown_forest_floor == FALSE,
    coalesce(stock_forest_floor,
             0),
    NA_real_)) %>%
  filter(use_stock_topsoil == FALSE) %>%
  group_by(plot_id,
           partner_short, partner_code,
           code_country, country, code_plot,
           latitude_dec, longitude_dec, mat, map, slope, altitude,
           wrb_ref_soil_group, eftc, humus_form, parent_material,
           biogeographical_region, main_tree_species, bs_class) %>%
  reframe(
    stock_min = min(stock_min),
    stock_max = max(stock_max),
    stock = mean(stock),
    stock_topsoil = mean(stock_topsoil),
    stock_below_ground = mean(stock_below_ground),
    stock_below_ground_topsoil = mean(stock_below_ground_topsoil),
    stock_forest_floor = mean(stock_forest_floor),
    use_stock_topsoil =
      any(use_stock_topsoil == TRUE))

p2 <- graph_interval(data = plot_c_stocks %>%
                 filter(!is.na(stock_forest_floor)),
               response = "stock",
               group = "wrb_ref_soil_group",
               path_export = paste0(dir, "graphs/"),
               mode = "dark",
               version = "dark",
               x_max = 1000,
               number_of_groups = 10,
               width = 5,
               aspect.ratio = 0.8,
               return = TRUE)

graph_interval(data = plot_c_stocks %>%
                 filter(!is.na(stock_forest_floor)) %>%
                 filter(use_stock_topsoil == FALSE),
               response = "stock",
               group = "wrb_ref_soil_group",
               path_export = paste0(dir, "graphs/"),
               x_max = 1000,
               number_of_groups = 10,
               aspect.ratio = 0.7)

### EFTC ----

d_forest_type <- read.csv("./data/additional_data/d_forest_type.csv",
                          sep = ";")

p3 <- graph_interval(data = plot_c_stocks %>%
                       filter(!is.na(stock_forest_floor)) %>%
                       filter(use_stock_topsoil == FALSE) %>%
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
                     path_export = paste0(dir, "graphs/"),
                     mode = "dark",
                     version = "dark",
                     x_max = 1000,
                     number_of_groups = 14,
                     width = 5,
                     aspect.ratio = 1.4,
                     return = TRUE)

graph_interval(data = plot_c_stocks %>%
                 filter(!is.na(stock_forest_floor)) %>%
                 filter(use_stock_topsoil == FALSE) %>%
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
               path_export = paste0(dir, "graphs/"),
               x_max = 1000,
               number_of_groups = 14,
               aspect.ratio = 1.2,
               return = TRUE)



p <- p2 / p1 / p3 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.justification = c("left", "top"))


ggsave(filename = paste0("patchwork", ".png"),
       plot = p,
       path = paste0(dir, "graphs/"),
       dpi = 500,
       height = 14,
       width = 5)








## 8.3. Make a map ----

### 8.3.1. Mean ----

source("./src/functions/map_icpf.R")
source("./src/functions/as_sf.R")

data_s1 <- s1_plot_c_stocks %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  # filter(survey_year >= 2000) %>%
  group_by(plot_id,
           survey_form, partner_short, partner_code,
           code_country, code_plot,
           latitude_dec, longitude_dec, mat, map, slope, altitude,
           wrb_ref_soil_group, eftc, humus_form, parent_material,
           biogeographical_region, main_tree_species, bs_class) %>%
  reframe(
    stock_min = min(stock_min),
    stock_max = max(stock_max),
    stock = mean(stock),
    stock_topsoil = mean(stock_topsoil)) %>%
  relocate(stock, .before = stock_min) %>%
  filter(!is.na(latitude_dec) &
           !is.na(longitude_dec)) %>%
  as_sf



data_so <- so_plot_c_stocks %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  # filter(survey_year >= 2000) %>%
  group_by(plot_id,
           survey_form, partner_short, partner_code,
           code_country, code_plot,
           latitude_dec, longitude_dec, mat, map, slope, altitude,
           wrb_ref_soil_group, eftc, humus_form, parent_material,
           biogeographical_region, main_tree_species, bs_class) %>%
  reframe(
    stock_min = min(stock_min),
    stock_max = max(stock_max),
    stock = mean(stock),
    stock_topsoil = mean(stock_topsoil)) %>%
  relocate(stock, .before = stock_min) %>%
  filter(!is.na(latitude_dec) &
           !is.na(longitude_dec)) %>%
  as_sf

# Level I

p1 <- map_icpf2(layers = "data_s1",
          title = paste0("**Forest soil carbon stock**",
                         " · ",
                         "Level I<br>",
                         n_distinct(data_s1$plot_id), " plots ",
                         "(forest floor + topsoil)<br>",
                         "Mean over survey period (1985 - 2022)"
          ),
          legend_title = paste0("**Carbon stock**<br>",
                                "t C ha<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_topsoil",
          point_size = 0.1,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark",
          inset_maps_offset_x = 1.5,
          export_name = "s1_mean_dark",
          export_folder = paste0(dir, "graphs/"),
          return = TRUE)

map_icpf2(layers = "data_s1",
          title = paste0("**Forest soil carbon stock**",
                         " · ",
                         "Level I<br>",
                         n_distinct(data_s1$plot_id), " plots ",
                         "(forest floor + topsoil)<br>",
                         "Mean over time (1985 - 2022)"
          ),
          legend_title = paste0("**Carbon stock**<br>",
                                "t C ha<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_topsoil",
          point_size = 0.1,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          inset_maps_offset_x = 1.5,
          export_name = "s1_mean",
          export_folder = paste0(dir, "graphs/"))

# Level II

map_icpf2(layers = "data_so",
          title = paste0("**Forest soil carbon stock**",
                         " · ",
                         "Level II<br>",
                         n_distinct(data_so$plot_id), " plots ",
                         "(forest floor + topsoil)<br>",
                         "Mean over survey period (1990 - 2023)"
          ),
         legend_title = paste0("**Carbon stock**<br>",
                               "t C ha<sup>-1</sup>"),
         biogeo_palette = NULL,
         legend_classes = NULL,
         variable_continuous = "stock_topsoil",
         point_size = 0.8,
         with_logo = FALSE,
         count_plots_legend = FALSE,
         mode = "dark",
         inset_maps_offset_x = 1.5,
         export_name = "so_mean_dark",
         export_folder = paste0(dir, "graphs/"),
         return = TRUE)


map_icpf2(layers = "data_so",
          title = paste0("**Forest soil carbon stock**",
                         " · ",
                         "Level II<br>",
                         n_distinct(data_so$plot_id), " plots ",
                         "(forest floor + topsoil)<br>",
                         "Mean over survey period (1990 - 2023)"
          ),
          legend_title = paste0("**Carbon stock**<br>",
                                "t C ha<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_topsoil",
          point_size = 0.8,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          inset_maps_offset_x = 1.5,
          export_name = "so_mean",
          export_folder = paste0(dir, "graphs/"))


### 8.3.2. Change ----

plot_c_stocks_summ %>%
  filter(!is.na(stock_change)) %>%
  filter(grepl("s1_", survey_form)) %>%
  mutate(partner_short = as.factor(partner_short)) %>%
  select(survey_year_earliest, survey_year_last, survey_year_difference,
         survey_year_count, partner_short) %>%
  summary

plot_c_stocks_summ %>%
  filter(!is.na(stock_change)) %>%
  filter(grepl("so_", survey_form)) %>%
  mutate(partner_short = as.factor(partner_short)) %>%
  select(survey_year_earliest, survey_year_last, survey_year_difference,
         survey_year_count, partner_short) %>%
  summary

data_change_so <- plot_c_stocks_summ %>%
  filter(!is.na(stock_change)) %>%
  filter(grepl("so_", survey_form)) %>%
  as_sf

map_icpf2(layers = "data_change_so",
          title = paste0("**Annual increase** in **forest soil carbon stock** ",
                         " · Level II<br>",
                         n_distinct(data_change_so$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "1991 - 2023 (time span per plot: 9 - 29 years)"),
          legend_title = paste0("**Annual increase**<br>",
                                "carbon stock<br>",
                                "t C ha<sup>-1</sup> year<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_change",
          point_size = 1.5,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          inset_maps_offset_x = 0.9,
          export_name = "so_change",
          export_folder = paste0(dir, "graphs/"))



p2 <- map_icpf2(layers = "data_change_so",
          title = paste0("**Annual increase** in **forest soil carbon stock** ",
                         " · Level II<br>",
                         n_distinct(data_change_so$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "1991 - 2023 (time span per plot: 9 - 29 years)"),
          legend_title = paste0("**Annual increase**<br>",
                                "carbon stock<br>",
                                "t C ha<sup>-1</sup> year<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_change",
          point_size = 1.5,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark",
          inset_maps_offset_x = 0.9,
          export_name = "so_change_dark",
          export_folder = paste0(dir, "graphs/"),
          return = TRUE)






p <- p1 / p2


ggsave(filename = paste0("patchwork_map", ".png"),
       plot = p2,
       path = paste0(dir, "graphs/"),
       dpi = 500,
       height = 14,
       width = 5.5)









