
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
             add_plausible_fscc = TRUE,
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
             add_plausible_fscc = TRUE,
             save_to_env = TRUE,
             save_to_gdrive = TRUE)

}

  # If calculate_new_stocks is FALSE

} else {


  # Import most recent stocks

  dir <- paste0(list.dirs("./output/stocks", recursive = FALSE)[
    grepl("_c_stocks",
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
# e.g. annual change in stock


# Define a function to calculate slope of linear regression
calculate_slope <- function(x, y) {
  if (length(unique(x)) > 1 &&
      length(x[!is.na(y)]) > 1 &&
     # Spanning more than eight years
     ((max(x) - min(x)) > 8)) {
    model <- lm(y ~ x)
    coef(model)[[2]]  # Slope coefficient
  } else {
    NA_real_
  }
}

calculate_slope <- function(x, y) {
  if (length(unique(x)) > 1 &&
      length(x[!is.na(y)]) > 1 &&
      # Spanning more than eight years
      ((max(x) - min(x)) > 8)) {
    model <- lm(y ~ x)
    coef(model)[[2]]  # Slope coefficient
  } else {
    NA_real_
  }
}

# Define a function to calculate intercept of linear regression
calculate_intercept <- function(x, y) {
  if (length(unique(x)) > 1 &&
      length(x[!is.na(y)]) > 1 &&
     # Spanning more than eight years
     ((max(x) - min(x)) > 8)) {
    model <- lm(y ~ x)
    coef(model)[[1]]  # Slope coefficient
  } else {
    NA_real_
  }
}


# Calculate annual stock change, etc
# Compile "s1" and "so" for the time being

# plot_c_stocks_summ <-
#   bind_rows(s1_plot_c_stocks,
#             so_plot_c_stocks) %>%
#   as_tibble() %>%
#   # Since data from the '90s are often methodologically incomparible
#   # filter(survey_year >= 2000) %>%
#   # Since fixed-depth layer data are supposed to be more complete,
#   # and because it is better to use data from one sampling approach
#   filter(grepl("som", survey_form)) %>%
#   filter(stock_plaus == TRUE) %>%
#   group_by(plot_id,
#            survey_form, partner_short, partner_code,
#            code_country, code_plot,
#            latitude_dec, longitude_dec, mat, map, slope, altitude,
#            wrb_ref_soil_group, eftc, humus_form, parent_material,
#            biogeographical_region, main_tree_species, bs_class) %>%
#   reframe(
#     # Net C influx - absolute (t C ha-1 year-1)
#     stock_change = calculate_slope(survey_year, stock),
#     stock_change_min = calculate_slope(survey_year, stock_min),
#     stock_change_max = calculate_slope(survey_year, stock_max),
#     int_stock = calculate_intercept(survey_year, stock),
#     int_stock_min = calculate_intercept(survey_year, stock_min),
#     int_stock_max = calculate_intercept(survey_year, stock_max),
#     survey_year_earliest = min(survey_year),
#     survey_year_last = max(survey_year),
#     survey_year_difference = max(survey_year) - min(survey_year),
#     survey_year_count = n_distinct(survey_year),
#     stock_earliest = stock[which(survey_year == survey_year_earliest)],
#     stock_last = stock[which(survey_year == survey_year_last)],
#     stock_min = min(stock_min),
#     stock_max = max(stock_max),
#     stock = mean(stock),
#     contains_peat =
#       any(contains_peat == TRUE)) %>%
#   relocate(stock, .before = stock_min) %>%
#   mutate(
#     stock_start = (survey_year_earliest * stock_change + int_stock),
#     # Net C influx - relative (% ha-1 year-1)
#     stock_change_rel = 100 * stock_change /
#       # Predicted stock at the start year
#       (stock_start),
#     stock_change_rel_min = 100 * stock_change_min /
#       (survey_year_earliest * stock_change_min + int_stock_min),
#     stock_change_rel_max = 100 * stock_change_max /
#       (survey_year_earliest * stock_change_max + int_stock_max)) %>%
#   # Since data from the '90s are often methodologically incomparible
#   filter(survey_year_last >= 2000)








# 5. Add national carbon stock estimates ----
# (reported by countries, to detect any systematic issues)

national_carbon_stocks_harmonised <-
  read.csv(paste0("./data/additional_data/national_coauthor_carbon_stocks/",
                  "national_carbon_stocks_harmonised.csv"),
           sep = ";")

# ...(see sandbox)







# 6. Statistics ----

# Bias-corrected and accelerated (BCa) confidence intervals, which adjust
# for both bias and skewness in the bootstrapped samples

boot_icpf <- function(data,
                      column_name,
                      stat = "mean_median",
                      nboot = 5000,
                      conf = 0.95) {

  alpha <- c(0.5 * (1 - conf),
             1 - 0.5 * (1 - conf))

  if (grepl("mean", stat)) {

    bca_ci <- bcanon(data %>%
                       pull(.data[[column_name]]),
                     nboot = nboot,
                     function(x) {mean(x, na.rm = TRUE)},
                     alpha = alpha)

    output_mean <- c(
      mean = round(mean(data[[column_name]]), 2),
      n = round(length(na.omit(data[[column_name]]))),
      mean_bca_ci_min = round(as.numeric(bca_ci$confpoints[1, 2]), 2),
      mean_bca_ci_max = round(as.numeric(bca_ci$confpoints[2, 2]), 2))
  }

  if (grepl("median", stat)) {

    bca_ci <- bcanon(data %>%
                       pull(.data[[column_name]]),
                     nboot = nboot,
                     function(x) {median(x, na.rm = TRUE)},
                     alpha = alpha)

    output_median <- c(
      median = round(median(data[[column_name]]), 2),
      n = round(length(na.omit(data[[column_name]]))),
      median_bca_ci_min = round(as.numeric(bca_ci$confpoints[1, 2]), 2),
      median_bca_ci_max = round(as.numeric(bca_ci$confpoints[2, 2]), 2))

  }

  if (stat == "mean") {
    output <- output_mean
  } else if (stat == "median") {
    output <- output_median
  } else {
    output <- c(output_mean, output_median)
  }

  return(output)

}





# Monte Carlo sampling function to estimate slope (sequestration rate)
# with uncertainty using triangular distribution.
# The triangular distribution is useful when you know the minimum, maximum,
# and the most likely value but lack information about the full distribution
# shape.

monte_carlo_slope <- function(data, column_name,
                              nsamples = 1000) {

  slopes <- numeric(nsamples)
  column_name_min <- paste0(column_name, "_min")
  column_name_max <- paste0(column_name, "_max")

  for (i in seq_len(nsamples)) {

    sampled_stock <- mapply(rtriangle,
                            # lower limit of triangle distribution
                            a = data[[column_name_min]],
                            # upper limit of triangle distribution
                            b = data[[column_name_max]],
                            # mode (maximum) of triangle distribution
                            c = data[[column_name]])

    slopes[i] <- calculate_slope(data$survey_year, sampled_stock)

  }
  return(slopes)
}







# Bias-corrected and accelerated (BCa) confidence intervals of sequestration
# rates
# This function bootstraps the entire set of Monte Carlo-sampled slopes
# (sequestration rates) from all plots, this way directly reflecting
# the propagated uncertainties in the final bootstrapping process.

boot_mc_change <- function(data,
                           column_name,
                           stat = "mean_median",
                           nsamples_mc = 50,
                           nboot = 200,
                           conf = 0.95) {

  assertthat::assert_that(is_grouped_df(data))

  stock_changes <- data %>%
    # Combine direct slope calculation and Monte Carlo sampling
    do({
      # Direct slope calculation
      direct_slope <- calculate_slope(.$survey_year,
                                      .data[[column_name]])
      # Monte Carlo slope sampling
      mc_slopes <- monte_carlo_slope(.,
                                     column_name = column_name,
                                     nsamples = nsamples_mc)
      # Create tibble with both results
      tibble(
        # plot_id = unique(.$plot_id),
        stock_change = direct_slope,
        mean_slope_mc = mean(mc_slopes, na.rm = TRUE),
        median_slope_mc = median(mc_slopes, na.rm = TRUE),
        slopes_mc = list(mc_slopes)
      )
    }) %>%
    ungroup() %>%
    filter(!is.na(stock_change))

  # Unlist all slopes into a single vector for bootstrapping
  all_slopes <- unlist(stock_changes$slopes_mc)

  # Bootstrapping on all sampled slopes
  slope_results <- boot_icpf(data.frame(all_slopes),
                             column_name = "all_slopes",
                             stat = stat,
                             nboot = nboot,
                             conf = conf)

  slope_results[which(names(slope_results) == "n")] <-
    nrow(stock_changes)

  slope_results[which(names(slope_results) == "mean")] <-
    round(mean(stock_changes$stock_change), 2)

  slope_results[which(names(slope_results) == "median")] <-
    round(median(stock_changes$stock_change), 2)

  return(slope_results)
}






## Level II: mean ----

# Total SOC stock (forest floor + soil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  reframe(stock = mean(stock)) %>%
  boot_icpf(column_name = "stock",
            stat = "mean_median")

# Total SOC stock (forest floor + topsoil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  reframe(stock_topsoil = mean(stock_topsoil)) %>%
  boot_icpf(column_name = "stock_topsoil",
            stat = "mean_median")


# Below-ground SOC stock (soil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  reframe(stock_below_ground = mean(stock_below_ground)) %>%
  boot_icpf(column_name = "stock_below_ground",
            stat = "mean_median")

# Below-ground SOC stock (topsoil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  reframe(stock_below_ground_topsoil = mean(stock_below_ground_topsoil)) %>%
  boot_icpf(column_name = "stock_below_ground_topsoil",
            stat = "mean_median")

# Forest floor SOC stock (forest floor)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  mutate(stock_forest_floor =
           coalesce(stock_forest_floor, 0)) %>%
  group_by(plot_id) %>%
  reframe(stock_forest_floor = mean(stock_forest_floor)) %>%
  boot_icpf(column_name = "stock_forest_floor",
            stat = "mean_median")


# Below-ground SOC stock (mineral soil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  filter(use_stock_topsoil == FALSE) %>%
  filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  reframe(stock_below_ground = mean(stock_below_ground)) %>%
  boot_icpf(column_name = "stock_below_ground",
            stat = "mean_median")

# Below-ground SOC stock (peat soil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  filter(use_stock_topsoil == FALSE) %>%
  filter(contains_peat == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  reframe(stock_below_ground = mean(stock_below_ground)) %>%
  boot_icpf(column_name = "stock_below_ground",
            stat = "mean_median")




## Level II: sequestration rate - mean method ----


# Total SOC stock (forest floor + soil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
    stock_change = calculate_slope(survey_year, stock)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")


# Total SOC stock (forest floor + topsoil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
    stock_change = calculate_slope(survey_year, stock_topsoil)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")





# Below-ground SOC stock (soil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
    stock_change = calculate_slope(survey_year, stock_below_ground)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")



# Below-ground SOC stock (topsoil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
    stock_change = calculate_slope(survey_year, stock_below_ground_topsoil)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")




# Forest floor SOC stock (forest floor)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  mutate(stock_forest_floor =
           coalesce(stock_forest_floor, 0)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
    stock_change = calculate_slope(survey_year, stock_forest_floor)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")





# Below-ground SOC stock (mineral soil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  filter(use_stock_topsoil == FALSE) %>%
  filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
    stock_change = calculate_slope(survey_year, stock_below_ground)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")








# Below-ground SOC stock (peat soil)

so_plot_c_stocks %>%
  filter(plausible_fscc == TRUE) %>%
  filter(use_stock_topsoil == FALSE) %>%
  filter(contains_peat == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
    stock_change = calculate_slope(survey_year, stock_below_ground)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")










## Level II: sequestration rate - MC method ----


# Total SOC stock (forest floor + soil)

so_plot_c_stocks %>%
  filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock")


# Total SOC stock (forest floor + topsoil)

so_plot_c_stocks %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_topsoil")




# Below-ground SOC stock (soil)

so_plot_c_stocks %>%
  filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_below_ground")



# Below-ground SOC stock (topsoil)

so_plot_c_stocks %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_below_ground_topsoil")




# Forest floor SOC stock (forest floor)

so_plot_c_stocks %>%
  # filter(use_stock_topsoil == FALSE) %>%
  # filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(is.na(unknown_forest_floor) |
           unknown_forest_floor == FALSE) %>%
  mutate(stock_forest_floor =
           coalesce(stock_forest_floor, 0)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_forest_floor")



# Below-ground SOC stock (mineral soil)

so_plot_c_stocks %>%
  filter(use_stock_topsoil == FALSE) %>%
  filter(contains_peat == FALSE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_below_ground")


# Below-ground SOC stock (peat soil)

so_plot_c_stocks %>%
  filter(use_stock_topsoil == FALSE) %>%
  filter(contains_peat == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  # filter(is.na(unknown_forest_floor) |
  #          unknown_forest_floor == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_below_ground")









## Level I & Level II ----


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

plot_c_stocks_summ %>%
  filter(!is.na(stock_change_rel)) %>%
  filter(survey_form == "so_som") %>%
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

# (...)










# 8. Data visualisation ----

## 8.2. Plots per stratifier ----

plot_c_stocks <-
  bind_rows(
    s1_plot_c_stocks,
    so_plot_c_stocks
    ) %>%
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
                  "biogeographical_region") %in% names(plot_c_stocks)))

source("./src/functions/graph_interval.R")


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

# plot_c_stocks <-
#   bind_rows(s1_plot_c_stocks,
#             so_plot_c_stocks) %>%
#   mutate(stock_forest_floor = ifelse(
#     is.na(unknown_forest_floor) | unknown_forest_floor == FALSE,
#     coalesce(stock_forest_floor,
#              0),
#     NA_real_)) %>%
#   filter(use_stock_topsoil == FALSE) %>%
#   group_by(plot_id,
#            partner_short, partner_code,
#            code_country, country, code_plot,
#            latitude_dec, longitude_dec, mat, map, slope, altitude,
#            wrb_ref_soil_group, eftc, humus_form, parent_material,
#            biogeographical_region, main_tree_species, bs_class) %>%
#   reframe(
#     stock_min = min(stock_min),
#     stock_max = max(stock_max),
#     stock = mean(stock),
#     stock_topsoil = mean(stock_topsoil),
#     stock_below_ground = mean(stock_below_ground),
#     stock_below_ground_topsoil = mean(stock_below_ground_topsoil),
#     stock_forest_floor = mean(stock_forest_floor),
#     use_stock_topsoil =
#       any(use_stock_topsoil == TRUE))

p2 <- graph_interval(data = plot_c_stocks %>%
                 filter(!is.na(stock)),
               response = "stock",
               group = "wrb_ref_soil_group",
               path_export = paste0(dir, "graphs/"),
               mode = "dark",
               version = "dark",
               x_max = 700,
               number_of_groups = 10,
               width = 5,
               aspect.ratio = 0.8,
               return = TRUE)

graph_interval(data = plot_c_stocks %>%
                 filter(!is.na(stock)),
               response = "stock",
               group = "wrb_ref_soil_group",
               path_export = paste0(dir, "graphs/"),
               x_max = 700,
               number_of_groups = 10,
               aspect.ratio = 0.7)

### EFTC ----

d_forest_type <- read.csv("./data/additional_data/d_forest_type.csv",
                          sep = ";")

p3 <- graph_interval(data = plot_c_stocks %>%
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
                     path_export = paste0(dir, "graphs/"),
                     mode = "dark",
                     version = "dark",
                     x_max = 700,
                     number_of_groups = 14,
                     width = 5,
                     aspect.ratio = 1.4,
                     return = TRUE)

graph_interval(data = plot_c_stocks %>%
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
               path_export = paste0(dir, "graphs/"),
               x_max = 700,
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

source("./src/functions/map_icpf2.R")
source("./src/functions/as_sf.R")

data_s1_full <- plot_c_stocks %>%
  filter(grepl("s1_", survey_form)) %>%
  filter(!is.na(latitude_dec) &
           !is.na(longitude_dec)) %>%
  as_sf

data_so_full <- plot_c_stocks %>%
  filter(grepl("so_", survey_form)) %>%
  filter(!is.na(latitude_dec) &
           !is.na(longitude_dec)) %>%
  as_sf

assertthat::assert_that(n_distinct(data_s1$plot_id) == nrow(data_s1))

# Level I

data_s1 <- data_s1_full %>%
  filter(!is.na(stock_topsoil))

map_icpf2(layers = "data_s1",
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
          export_folder = paste0(dir, "graphs/"))

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
          mode = "dark_light",
          inset_maps_offset_x = 1.5,
          export_name = "s1_mean3",
          export_folder = paste0(dir, "graphs/"))

# Level II

data_so <- data_so_full %>%
  filter(!is.na(stock_topsoil))

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
         export_folder = paste0(dir, "graphs/"))


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
          mode = "dark_light",
          inset_maps_offset_x = 1.5,
          export_name = "so_mean",
          export_folder = paste0(dir, "graphs/"))


data_so <- data_so_full %>%
  filter(!is.na(stock))

map_icpf2(layers = "data_so",
          title = paste0("**Forest soil carbon stock**",
                         " · ",
                         "Level II<br>",
                         n_distinct(data_so$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "Mean over survey period (1990 - 2023)"
          ),
          legend_title = paste0("**Carbon stock**<br>",
                                "t C ha<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock",
          point_size = 0.8,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 1.5,
          export_name = "so_soildepth_mean",
          export_folder = paste0(dir, "graphs/"))




### 8.3.2. Change ----

plot_c_stocks %>%
  filter(!is.na(stock_change)) %>%
  filter(grepl("s1_", survey_form)) %>%
  mutate(code_country = as.factor(code_country)) %>%
  select(survey_year_earliest, survey_year_last, survey_year_difference,
         survey_year_count, code_country) %>%
  summary

plot_c_stocks %>%
  filter(!is.na(stock_change)) %>%
  filter(grepl("so_", survey_form)) %>%
  mutate(code_country = as.factor(code_country)) %>%
  select(survey_year_earliest, survey_year_last, survey_year_difference,
         survey_year_count, code_country) %>%
  summary


data_so <- data_so_full %>%
  filter(!is.na(stock_change))

map_icpf2(layers = "data_so",
          title = paste0("**Annual increase** in **forest soil carbon stock** ",
                         " · Level II<br>",
                         n_distinct(data_so$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "1991 - 2022 (time span per plot: 9 - 27 years)"),
          legend_title = paste0("**Annual increase**<br>",
                                "carbon stock<br>",
                                "t C ha<sup>-1</sup> year<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_change",
          point_size = 1.5,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 0.9,
          export_name = "so_change",
          export_folder = paste0(dir, "graphs/"))



map_icpf2(layers = "data_so",
          title = paste0("**Annual increase** in **forest soil carbon stock** ",
                         " · Level II<br>",
                         n_distinct(data_so$plot_id), " plots ",
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
          export_folder = paste0(dir, "graphs/"))




data_s1 <- data_s1_full %>%
  filter(!is.na(stock_change))

map_icpf2(layers = "data_s1",
          title = paste0("**Annual increase** in **forest soil carbon stock** ",
                         " · Level I<br>",
                         n_distinct(data_s1$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "1986 - 2022 (time span per plot: 10 - 32 years)"),
          legend_title = paste0("**Annual increase**<br>",
                                "carbon stock<br>",
                                "t C ha<sup>-1</sup> year<sup>-1</sup>"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_change",
          point_size = 0.5,
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 0.9,
          export_name = "s1_change",
          export_folder = paste0(dir, "graphs/"))




### 8.3.3. Carbon Distribution Index (CDI) ----


source("./src/functions/graph_interval.R")

# Forest floor to below-ground topsoil

graph_interval(data = plot_c_stocks %>%
                 filter(!is.na(di_ff_top)),
               response = "di_ff_top",
               group = "wrb_ref_soil_group",
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 2,
               number_of_groups = 10,
               aspect.ratio = 0.7)

graph_interval(data = plot_c_stocks %>%
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
               path_export = paste0(dir, "graphs/"),
               x_max = 2,
               aspect.ratio = 0.35)


# Below-ground topsoil to below-ground

graph_interval(data = plot_c_stocks %>%
                 filter(!is.na(di_top_soil)),
               response = "di_top_soil",
               group = "wrb_ref_soil_group",
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 1,
               number_of_groups = 10,
               aspect.ratio = 0.7)


graph_interval(data = plot_c_stocks %>%
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
               path_export = paste0(dir, "graphs/"),
               x_max = 1,
               number_of_groups = 13,
               aspect.ratio = 1,
               return = TRUE)

