
# Calculate carbon stocks until 100 cm (or until the effective soil depth
# if shallower)
# ---------------------------------------------------------------------------

# Details: In this script, validated and gap-filled "layer 1+" solid soil data
# are processed to calculate soil organic carbon stocks using different
# functions. After that, the stock data are summarised using some basic
# stats and visualised (interval graphs and maps)

# The detailed steps taken in each function can be found within the "Roxygen"
# documentation of the function, i.e. on top of the function scripts
# (in "./src/stock_calculations/functions/" folder).

# Remarks:
# · First set the "input parameters" for this script correct in section 2.
# · Do not just run all code of this script. Especially section 5 (stats)
#   contains some lengthy codes that are not really necessary (anymore) but
#   left for completeness. Also, if you are okay with using the most recent
#   version of the stocks, you can upload those instead of rerunning the
#   stock functions in section 3 (since those can take a long time: ~4 h
#   for Level I and ~1 h for Level II)

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
source("./src/stock_calculations/functions/get_path_most_recent_stocks.R")
source("./src/stock_calculations/functions/stock_stats.R")
source("./src/functions/graph_interval.R")
source("./src/functions/map_icpf2.R")
source("./src/functions/map_icpf.R")
source("./src/functions/as_sf.R")
source("./src/functions/harmonise_quantiles.R")
source("./src/sandbox/esb_mapping_template-main/function_esb_map_template.R")


# 2. Specify input parameters ----

# Only run the line of the ICP Forests level for which you would like
# to run the script

level <- "LII" # Level I or Level II

calculate_new_stocks <- FALSE # FALSE if you prefer to import
                              # the most recently calculated stock data;
                              # TRUE if you prefer to recalculate the stocks
add_nat_oc_stock_estimates <- FALSE # TRUE if you want to add the national
                                    # OC stock data reported by co-authors
parameter <- "organic_carbon_total"
parameter <- "n_total"




# Import layer 1+ data


source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)



# Derive information from input

survey_form <- case_when(
  level == "LI" ~ "s1",
  level == "LII" ~ "so")

level_long <- case_when(
  level == "LI" ~ "Level I",
  level == "LII" ~ "Level II")


source("./src/functions/get_par_table.R")
parameter_table <- get_par_table()

ind <- unique(c(which(parameter == parameter_table$som_parameter),
                which(parameter == parameter_table$pfh_parameter)))

assertthat::assert_that(!identical(ind, integer(0)))

shorter_var_name <- parameter_table$shorter_name[ind]
shorter_var_name_cap <- parameter_table$shorter_name_cap[ind]
full_var_name <- parameter_table$full_name[ind]
unit_stock <- parameter_table$unit_stock_markdown[ind]





# Get stocks

if (calculate_new_stocks == TRUE) {

# 3. Calculate stocks ----

if (level == "LI") {

  source("./src/stock_calculations/functions/get_stocks.R")

  get_stocks(survey_form = "s1",
             data_frame = NULL,
             parameter = parameter,
             constant_subsoil = TRUE,
             exclude_ol = TRUE,
             graph = case_when(
               parameter == "organic_carbon_total" ~ TRUE,
               TRUE ~ FALSE), # No graphs for Level I, else too lengthy
             add_stratifiers = TRUE,
             use_splines = TRUE,
             density_per_three_cm = TRUE,
             add_plausible_fscc = TRUE,
             save_to_env = TRUE,
             save_to_gdrive = TRUE)

}


if (level == "LII") {

  source("./src/stock_calculations/functions/get_stocks.R")

  get_stocks(survey_form = "so",
             data_frame = NULL,
             parameter = parameter,
             constant_subsoil = TRUE,
             exclude_ol = TRUE,
             graph = TRUE,
             add_stratifiers = TRUE,
             use_splines = TRUE,
             density_per_three_cm = TRUE,
             add_plausible_fscc = TRUE,
             save_to_env = TRUE,
             save_to_gdrive = TRUE)

}

  # If calculate_new_stocks is FALSE

} else {


  # Import most recent stocks

  dir <- get_path_most_recent_stocks(shorter_var_name = shorter_var_name)


  for (survey_form_i in c("s1", "so")) {

    # Layers

    data <- read.csv(paste0(dir,
                            survey_form_i,
                            "_layers.csv"),
                     sep = ";") %>%
      rename_with(~ sub(paste0("^", shorter_var_name, "_"),
                        "", .x))

    assign_env(paste0(survey_form_i, "_layers"),
               data)

    # Profile stocks

    data <- read.csv(paste0(dir,
                            survey_form_i,
                            "_profile_", shorter_var_name, "_stocks.csv"),
                     sep = ";") %>%
      rename_with(~ sub(paste0("^", shorter_var_name, "_"),
                        "", .x))

    assign_env(paste0(survey_form_i, "_profile_", shorter_var_name, "_stocks"),
               data)

    # Plot stocks

    data <- read.csv(paste0(dir,
                            survey_form_i,
                            "_plot_", shorter_var_name, "_stocks.csv"),
                     sep = ";") %>%
      # Remove any leading "oc_" in the character names
      rename_with(~ sub(paste0("^", shorter_var_name, "_"),
                        "", .x)) %>%
      mutate_all(function(x) ifelse((x) == "", NA, x))

    assign_env(paste0(survey_form_i, "_plot_", shorter_var_name, "_stocks"),
               data)

  }

}





# 4. Add national stock estimates ----
# (reported by countries, to detect any systematic issues)

# Only organic carbon

if (parameter == "organic_carbon_total" &&
    add_nat_oc_stock_estimates == TRUE) {

  national_carbon_stocks_harmonised <-
    read.csv(paste0("./data/additional_data/national_coauthor_carbon_stocks/",
                    "national_carbon_stocks_harmonised.csv"),
             sep = ";")

  if (!exists("d_country", .GlobalEnv)) {

    d_country <- read.csv(paste0("./data/raw_data/", survey_form,
                                 "/adds/dictionaries/d_country.csv"),
                          sep = ";")
  }

  df <-
    get(paste0(survey_form, "_plot_",
               shorter_var_name, "_stocks")) %>%
    mutate(key = ifelse(code_country %in% c(5, 53),
                        code_country,
                        plot_id)) %>%
    # Add national stocks
    left_join(national_carbon_stocks_harmonised %>%
                mutate(key = ifelse(code_country %in% c(5, 53),
                                    code_country,
                                    plot_id)) %>%
                filter(level == "so") %>%
                select(-level, -code_country, -plot_id),
              by = "key") %>%
    mutate(
      inside_uncertainty = ifelse(
        (!is.na(c_stock_nat) | !is.na(c_stock_topsoil_nat)),
        ifelse(!is.na(c_stock_nat),
               case_when(
                 c_stock_nat > stock_max ~ "underestimated",
                 c_stock_nat < stock_min ~ "overestimated",
                 c_stock_nat < stock_max &
                   c_stock_nat > stock_min ~ "in_unc_range"),
               case_when(
                 c_stock_topsoil_nat > stock_topsoil_max ~ "underestimated",
                 c_stock_topsoil_nat < stock_topsoil_min ~ "overestimated",
                 c_stock_topsoil_nat < stock_topsoil_max &
                   c_stock_topsoil_nat > stock_topsoil_min ~ "in_unc_range")),
        NA_character_),
      rel_overestimation = ifelse(
        (!is.na(c_stock_nat) | !is.na(c_stock_topsoil_nat)),
        ifelse(!is.na(c_stock_nat),
               round(stock / c_stock_nat, 2),
               round(stock_topsoil / c_stock_topsoil_nat, 2)),
        NA_real_)) %>%
    left_join(d_country %>%
                rename(code_country = code) %>%
                rename(country = lib_country) %>%
                select(code_country, country),
              by = "code_country")


  summary(as.factor(df$inside_uncertainty))
  summary(df$rel_overestimation)

  df %>%
    filter(!is.na(rel_overestimation)) %>%
    filter(!code_country %in% c(5, 53)) %>%
    group_by(country) %>%
    reframe(rel_overestim_avg = round(mean(rel_overestimation), 1),
            rel_overestim_quantile5 =
              round(quantile(rel_overestimation, 0.05), 1),
            rel_overestim_quantile95 =
              round(quantile(rel_overestimation, 0.95), 1))

  df %>%
    filter(!is.na(rel_overestimation)) %>%
    filter(!code_country %in% c(5, 53)) %>%
    count(country, inside_uncertainty) %>%
    pivot_wider(names_from = inside_uncertainty,
                values_from = n,
                values_fill = 0)


} # End of "if add_nat_oc_stock_estimates == TRUE"























# 5. Stats ----



# Note: not sure if these bootstrapped statistics are still relevant
# (especially those evaluating changes).
# But I just leave the code here for in case...




## Some quick stats ----

# Below-ground MINERAL C stocks until 100

min_below_ground <-
  get(paste0(survey_form, "_plot_",
             shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == FALSE) %>%
  filter(!is.na(stock_below_ground)) %>%
  pull(stock_below_ground) %>%
  summary

min_below_ground


# Below-ground MINERAL C stocks until 30

min_below_ground_topsoil <-
  get(paste0(survey_form, "_plot_",
             shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_topsoil_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == FALSE) %>%
  filter(!is.na(stock_below_ground_topsoil)) %>%
  pull(stock_below_ground_topsoil) %>%
  summary

cat("\n\nPercentage of total stock located in topsoil (mineral soils):\n")

as.numeric(min_below_ground_topsoil["Median"] /
             min_below_ground["Median"] * 100)




# Below-ground peat C stocks until 100

peat_below_ground <-
  get(paste0(survey_form, "_plot_",
             shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == TRUE) %>%
  filter(!is.na(stock_below_ground)) %>%
  pull(stock_below_ground) %>%
  summary

peat_below_ground

# Below-ground peat C stocks until 30

peat_below_ground_topsoil <-
  get(paste0(survey_form, "_plot_",
             shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_topsoil_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == TRUE) %>%
  filter(!is.na(stock_below_ground_topsoil)) %>%
  pull(stock_below_ground_topsoil) %>%
  summary

cat("\n\nPercentage of total stock located in topsoil (peat soils):\n")

as.numeric(peat_below_ground_topsoil["Median"] /
             peat_below_ground["Median"] * 100)


# Forest floor

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_forest_floor_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(!is.na(stock_forest_floor)) %>%
  pull(stock_forest_floor) %>%
  summary







## Mean (bootstrapped) ----

# Bias-corrected and accelerated (BCa) confidence intervals, which adjust
# for both bias and skewness in the bootstrapped samples

# Total SOC stock (forest floor + soil)

cat(paste0("\n\nTotal ", shorter_var_name,
           " stock (forest floor + soil) - mean\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  reframe(stock = mean(stock)) %>%
  boot_icpf(column_name = "stock",
            stat = "mean_median")

# Total SOC topsoil stock (forest floor + topsoil)

cat(paste0("\n\nTotal ", shorter_var_name,
           " topsoil stock (forest floor + topsoil) - mean\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_topsoil_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  reframe(stock_topsoil = mean(stock_topsoil)) %>%
  boot_icpf(column_name = "stock_topsoil",
            stat = "mean_median")


# Below-ground SOC stock (soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (soil) - mean\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  reframe(stock_below_ground = mean(stock_below_ground)) %>%
  boot_icpf(column_name = "stock_below_ground",
            stat = "mean_median")

# Below-ground SOC stock (MINERAL soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (MINERAL soil) - mean\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == FALSE) %>%
  group_by(plot_id) %>%
  reframe(stock_below_ground = mean(stock_below_ground)) %>%
  boot_icpf(column_name = "stock_below_ground",
            stat = "mean_median")

# Below-ground SOC stock (PEAT soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (PEAT soil) - mean\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == TRUE) %>%
  group_by(plot_id) %>%
  reframe(stock_below_ground = mean(stock_below_ground)) %>%
  boot_icpf(column_name = "stock_below_ground",
            stat = "mean_median")

# Below-ground topsoil SOC stock

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " topsoil stock - mean\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_topsoil_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  reframe(stock_below_ground_topsoil = mean(stock_below_ground_topsoil)) %>%
  boot_icpf(column_name = "stock_below_ground_topsoil",
            stat = "mean_median")

# Forest floor SOC stock (forest floor)

cat(paste0("\n\nForest floor ", shorter_var_name,
           " stock - mean\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_forest_floor_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  reframe(stock_forest_floor = mean(stock_forest_floor)) %>%
  boot_icpf(column_name = "stock_forest_floor",
            stat = "mean_median")







## Sequestration rate - mean (bootstrapped) method ----

# Bias-corrected and accelerated (BCa) confidence intervals, which adjust
# for both bias and skewness in the bootstrapped samples

# Note: it is no longer the recommended statistical approach to evaluate
# and summarise changes for individual plots

# Total SOC stock (forest floor + soil)

cat(paste0("\n\nTotal ", shorter_var_name,
           " stock (forest floor + soil) - change (mean method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  # Filter for plots with last survey after 2000
  filter(max(survey_year) >= 2000) %>%
  reframe(
    # Net C influx - absolute (t C ha-1 year-1)
    # Only for plot surveys spanning > eight years
    stock_change = calculate_slope(survey_year, stock)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")


# Total SOC topsoil stock (forest floor + topsoil)

cat(paste0("\n\nTotal ", shorter_var_name,
           " topsoil stock (forest floor + topsoil) - change (mean method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_topsoil_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    stock_change = calculate_slope(survey_year, stock_topsoil)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")


# Below-ground SOC stock (soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (soil) - change (mean method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    stock_change = calculate_slope(survey_year, stock_below_ground)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")


# Below-ground SOC stock (MINERAL soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (MINERAL soil) - change (mean method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    stock_change = calculate_slope(survey_year, stock_below_ground)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")


# Below-ground SOC stock (PEAT soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (PEAT soil) - change (mean method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == TRUE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    stock_change = calculate_slope(survey_year, stock_below_ground)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")


# Below-ground topsoil SOC stock

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " topsoil stock - change (mean method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_topsoil_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    stock_change = calculate_slope(survey_year, stock_below_ground_topsoil)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")


# Forest floor SOC stock (forest floor)

cat(paste0("\n\nForest floor ", shorter_var_name,
           " stock - change (mean method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_forest_floor_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  reframe(
    stock_change = calculate_slope(survey_year, stock_forest_floor)) %>%
  filter(!is.na(stock_change)) %>%
  boot_icpf(column_name = "stock_change",
            stat = "mean_median")




















## Sequestration rate - Monte Carlo (bootstrapped) method ----

# Bias-corrected and accelerated (BCa) confidence intervals of sequestration
# rates
# This function bootstraps the entire set of Monte Carlo-sampled slopes
# (sequestration rates) from all plots, this way directly reflecting
# the propagated uncertainties in the final bootstrapping process.

# Monte Carlo sampling function to estimate slope (sequestration rate)
# with uncertainty using triangular distribution.
# The triangular distribution is useful when you know the minimum, maximum,
# and the most likely value but lack information about the full distribution
# shape.


# Note: this may take a long time, especially for Level I!
# Moreover, it is no longer the recommended statistical approach to evaluate
# and summarise changes for individual plots

# Total SOC stock (forest floor + soil)

cat(paste0("\n\nTotal ", shorter_var_name,
           " stock (forest floor + soil) - change (MC method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  # Filter for plots with last survey after 2000
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock")



# Total SOC topsoil stock (forest floor + topsoil)

cat(paste0("\n\nTotal ", shorter_var_name,
           " topsoil stock (forest floor + topsoil) - change (MC method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_topsoil_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_topsoil")


# Below-ground SOC stock (soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (soil) - change (MC method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_below_ground")


# Below-ground SOC stock (MINERAL soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (MINERAL soil) - change (MC method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == FALSE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_below_ground")


# Below-ground SOC stock (PEAT soil)

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " stock (PEAT soil) - change (MC method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  filter(contains_peat == TRUE) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_below_ground")


# Below-ground topsoil SOC stock

cat(paste0("\n\nBelow-ground ", shorter_var_name,
           " topsoil stock - change (MC method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_below_ground_topsoil_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_below_ground_topsoil")


# Forest floor SOC stock (forest floor)

cat(paste0("\n\nForest floor ", shorter_var_name,
           " stock - change (MC method)\n"))

get(paste0(survey_form, "_plot_",
           shorter_var_name, "_stocks")) %>%
  filter(stock_forest_floor_plaus == TRUE) %>%
  filter(grepl("som", survey_form)) %>%
  group_by(plot_id) %>%
  filter(max(survey_year) >= 2000) %>%
  boot_mc_change(column_name = "stock_forest_floor")




## Stats per stratifier ----

# e.g. biogeographical region

suppressWarnings({
  rempsyc::rcompanion_groupwiseMean(
    group = "biogeographical_region",
    var = "stock",
    data = get(paste0(survey_form, "_plot_",
                      shorter_var_name, "_stocks")) %>%
      filter(stock_plaus == TRUE) %>%
      filter(grepl("som", survey_form)) %>%
      filter(!is.na(stock)) %>%
      filter(!is.na(biogeographical_region)),
    conf = 0.95,
    digits = 5,
    R = 2000,
    traditional = TRUE,
    bca = FALSE,
    na.rm = TRUE) %>%
    mutate(Trad.lower = ifelse(
      Trad.lower < 0, 0, Trad.lower)) %>%
    arrange(desc(Mean))
})




















# 6. Data visualisation ----

# Folder of most recent stocks to save graphs:
dir <- get_path_most_recent_stocks(shorter_var_name = shorter_var_name)


## 6.1. Plots per stratifier ----

# Better do not combine Level I and Level II as we used to do in the past

# Dataframe where stocks (and changes) are summarised per plot_id

plot_stocks <-
  get(paste0(survey_form, "_plot_",
             shorter_var_name, "_stocks")) %>%
  # Exclude pfh-based stocks
  filter(grepl("som", survey_form)) %>%
  # Remove implausible stocks
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
           code_country, code_plot,
           latitude_dec, longitude_dec, x_etrs89, y_etrs89,
           mat, map, slope, altitude,
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
  ungroup() %>%
  rowwise() %>%
  mutate(
    # (Carbon) Distribution Index - forest floor to below-ground topsoil ratio
    di_ff_top = ifelse(
      !is.na(stock_forest_floor) & !is.na(stock_below_ground_topsoil),
      stock_forest_floor / stock_below_ground_topsoil,
      NA_real_),
    # (Carbon) Distribution Index - below-ground topsoil to below-ground ratio
    di_top_soil = ifelse(
      !is.na(stock_below_ground_topsoil) & !is.na(stock_below_ground),
      stock_below_ground_topsoil / stock_below_ground,
      NA_real_)) %>%
  ungroup()

# Note: an additional filter that could be applied is
# filter(survey_year_last >= 2000)
# Since data from the '90s are often methodologically incomparible

assert_that(all(c("wrb_ref_soil_group",
                  "eftc",
                  "humus_form",
                  "biogeographical_region") %in% names(plot_stocks)))


assertthat::assert_that(n_distinct(plot_stocks$plot_id) == nrow(plot_stocks))

plot_stocks %>%
  filter(!is.na(stock)) %>%
  pull(stock) %>%
  hist




### Humus form ----

graph_interval(data = plot_stocks %>%
                 filter(!is.na(stock_forest_floor)) %>%
                 # Harmonise humus form entries
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
               path_export = paste0(dir, "graphs/"),
               x_max = 100,
               aspect.ratio = 0.35)


### WRB soil group ----

graph_interval(data = plot_stocks %>%
                 filter(!is.na(stock)),
               response = "stock",
               group = "wrb_ref_soil_group",
               shorter_var_name = shorter_var_name,
               path_export = paste0(dir, "graphs/"),
               x_max = 750,
               number_of_groups = 10,
               aspect.ratio = 0.7)

### EFTC ----

d_forest_type <- read.csv("./data/additional_data/d_forest_type.csv",
                          sep = ";")

graph_interval(data = plot_stocks %>%
                 filter(!is.na(stock)) %>%
                 # Harmonise EFTC entries
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
               path_export = paste0(dir, "graphs/"),
               x_max = 750,
               number_of_groups = 14,
               aspect.ratio = 1.2)





### Biogeographical region ----

graph_interval(data = plot_stocks %>%
                 filter(!is.na(stock)) %>%
                 filter(!is.na(biogeographical_region)),
               response = "stock",
               group = "biogeographical_region",
               shorter_var_name = shorter_var_name,
               path_export = paste0(dir, "graphs/"),
               x_max = 500,
               number_of_groups = 6,
               aspect.ratio = 0.5)





# stock_topsoil

graph_interval(data = plot_stocks %>%
                 filter(!is.na(stock_topsoil)) %>%
                 filter(!is.na(biogeographical_region)),
               response = "stock_topsoil",
               group = "biogeographical_region",
               shorter_var_name = shorter_var_name,
               path_export = paste0(dir, "graphs/"),
               x_max = 300,
               number_of_groups = 8,
               aspect.ratio = 0.7)





### Compiled graph ----
#  (was used for poster SOM2024 symposium Morocco 2024)

p1 <- graph_interval(
  data = plot_stocks %>%
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
  mode = "dark",
  version = "dark",
  x_max = 100,
  width = 5,
  aspect.ratio = 0.4,
  return = TRUE)

p2 <- graph_interval(
  data = plot_stocks %>%
    filter(!is.na(stock)),
  response = "stock",
  group = "wrb_ref_soil_group",
  shorter_var_name = shorter_var_name,
  mode = "dark",
  version = "dark",
  x_max = 750,
  number_of_groups = 10,
  width = 5,
  aspect.ratio = 0.8,
  return = TRUE)


p3 <- graph_interval(
  data = plot_stocks %>%
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
  mode = "dark",
  version = "dark",
  x_max = 750,
  number_of_groups = 14,
  width = 5,
  aspect.ratio = 1.4,
  return = TRUE)


p <- p2 / p1 / p3 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.justification = c("left", "top"))


ggsave(filename = paste0(survey_form, "_patchwork", ".png"),
       plot = p,
       path = paste0(dir, "graphs/"),
       dpi = 500,
       height = 14,
       width = 5)







### Parameter Distribution Index (DI) ----



# Forest floor to below-ground topsoil

graph_interval(data = plot_stocks %>%
                 filter(!is.na(di_ff_top)),
               response = "di_ff_top",
               group = "wrb_ref_soil_group",
               shorter_var_name = shorter_var_name,
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 2,
               number_of_groups = 10,
               aspect.ratio = 0.7)

graph_interval(data = plot_stocks %>%
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
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 2,
               aspect.ratio = 0.35)

graph_interval(data = plot_stocks %>%
                 filter(!is.na(di_ff_top)) %>%
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
               response = "di_ff_top",
               group = "eftc",
               shorter_var_name = shorter_var_name,
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 2,
               number_of_groups = 13,
               aspect.ratio = 1)


# Below-ground topsoil to below-ground

graph_interval(data = plot_stocks %>%
                 filter(!is.na(di_top_soil)),
               response = "di_top_soil",
               group = "wrb_ref_soil_group",
               shorter_var_name = shorter_var_name,
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 1,
               number_of_groups = 10,
               aspect.ratio = 0.7)


graph_interval(data = plot_stocks %>%
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
               path_export = paste0(dir, "graphs/"),
               x_min = 0,
               x_max = 1,
               number_of_groups = 13,
               aspect.ratio = 1)







## 6.2. Make a map ----

### 6.2.1. Mean ----

assertthat::assert_that(
  all(!is.na(plot_stocks$latitude_dec)) &&
    all(!is.na(plot_stocks$longitude_dec)))

plot_stocks_sf <- plot_stocks %>%
  filter(!is.na(stock)) %>%
  as_sf

plot_topsoil_stocks_sf <- plot_stocks %>%
  filter(!is.na(stock_topsoil)) %>%
  as_sf


# Total stock - Light mode

map_icpf2(layers = "plot_stocks_sf",
          title = paste0("**Forest soil ", tolower(full_var_name), " stock**",
                         " · ", level_long, "<br>",
                         n_distinct(plot_stocks_sf$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "Mean over survey period (",
                         min(plot_stocks_sf$survey_year_earliest,
                             na.rm = TRUE),
                         " - ",
                         max(plot_stocks_sf$survey_year_last,
                             na.rm = TRUE),
                         ")"),
          legend_title = paste0("**", full_var_name, "<br>stock** (",
                                unit_stock, ")"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_topsoil",
          point_size = case_when(
            survey_form == "s1" ~ 0.1,
            survey_form == "so" ~ 0.8),
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 1.3,
          export_name = paste0(survey_form, "_mean"),
          export_folder = paste0(dir, "graphs/"))

# Total stock - Dark mode

map_icpf2(layers = "plot_stocks_sf",
          title = paste0("**Forest soil ", tolower(full_var_name), " stock**",
                         " · ", level_long, "<br>",
                         n_distinct(plot_stocks_sf$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         "Mean over survey period (",
                         min(plot_stocks_sf$survey_year_earliest,
                             na.rm = TRUE),
                         " - ",
                         max(plot_stocks_sf$survey_year_last,
                             na.rm = TRUE),
                         ")"),
          legend_title = paste0("**", full_var_name, "<br>stock** (",
                                unit_stock, ")"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_topsoil",
          point_size = case_when(
            survey_form == "s1" ~ 0.1,
            survey_form == "so" ~ 0.8),
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark",
          inset_maps_offset_x = 1.3,
          export_name = paste0(survey_form, "_mean_dark"),
          export_folder = paste0(dir, "graphs/"))


# Total stock - ESB template

create_esb_map(data = plot_stocks %>%
                 filter(!is.na(stock)),
               lon_col = "longitude_dec",
               lat_col = "latitude_dec",
               color_col = "stock",
               point_size = case_when(
                 survey_form == "s1" ~ 0.1,
                 survey_form == "so" ~ 0.8),
               save_plot = TRUE,
               legend_title = paste0(full_var_name, " stock \n",
                                     "(forest floor + soil) \n[",
                                     sub("<sup>-1</sup>$", "-1", unit_stock),
                                     "]"),
               filename = paste0(dir, "graphs/", survey_form,
                                 "_mean_esb.png"))





# Topsoil - Light mode

map_icpf2(layers = "plot_topsoil_stocks_sf",
          title = paste0("**Forest soil ", tolower(full_var_name), " stock**",
                         " · ", level_long, "<br>",
                         n_distinct(plot_topsoil_stocks_sf$plot_id), " plots ",
                         "(forest floor + topsoil)<br>",
                         "Mean over survey period (",
                         min(plot_topsoil_stocks_sf$survey_year_earliest,
                             na.rm = TRUE),
                         " - ",
                         max(plot_topsoil_stocks_sf$survey_year_last,
                             na.rm = TRUE),
                         ")"),
          legend_title = paste0("**", full_var_name, "<br>stock** (",
                                unit_stock, ")"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_topsoil",
          point_size = case_when(
            survey_form == "s1" ~ 0.1,
            survey_form == "so" ~ 0.8),
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 1.3,
          export_name = paste0(survey_form, "_topsoil_mean"),
          export_folder = paste0(dir, "graphs/"))

# Topsoil - Dark mode

map_icpf2(layers = "plot_topsoil_stocks_sf",
          title = paste0("**Forest soil ", tolower(full_var_name), " stock**",
                         " · ", level_long, "<br>",
                         n_distinct(plot_topsoil_stocks_sf$plot_id), " plots ",
                         "(forest floor + topsoil)<br>",
                         "Mean over survey period (",
                         min(plot_topsoil_stocks_sf$survey_year_earliest,
                             na.rm = TRUE),
                         " - ",
                         max(plot_topsoil_stocks_sf$survey_year_last,
                             na.rm = TRUE),
                         ")"),
          legend_title = paste0("**", full_var_name, "<br>stock** (",
                                unit_stock, ")"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_topsoil",
          point_size = case_when(
            survey_form == "s1" ~ 0.1,
            survey_form == "so" ~ 0.8),
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark",
          inset_maps_offset_x = 1.3,
          export_name = paste0(survey_form, "_topsoil_mean_dark"),
          export_folder = paste0(dir, "graphs/"))


# Topsoil - ESB template

create_esb_map(data = plot_stocks %>%
                 filter(!is.na(stock_topsoil)),
               lon_col = "longitude_dec",
               lat_col = "latitude_dec",
               color_col = "stock_topsoil",
               point_size = case_when(
                 survey_form == "s1" ~ 0.1,
                 survey_form == "so" ~ 0.8),
               save_plot = TRUE,
               legend_title = paste0(full_var_name, " stock \n",
                                     "(forest floor + topsoil) \n[",
                                     sub("<sup>-1</sup>$", "-1", unit_stock),
                                     "]"),
               filename = paste0(dir, "graphs/", survey_form,
                                 "_topsoil_mean_esb.png"))


















### 6.2.2. Map stock classes ----


#### Total soil ----

# Get boundaries from Level I, in the assumption that this is representative
# for Europe

quantiles <- get(paste0("s1_plot_", shorter_var_name, "_stocks")) %>%
  filter(stock_plaus == TRUE) %>%
  group_by(plot_id) %>%
  reframe(stock = mean(stock, na.rm = TRUE)) %>%
  pull(stock) %>%
  quantile(c(0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE) %>%
  harmonise_quantiles

if (parameter == "organic_carbon_total") {

  quantiles <- c(50, 75, 100, 150, 850)
}

data_sf <- plot_stocks %>%
  filter(!is.na(stock)) %>%
  mutate(
    stock_cat = cut(
      stock,
      breaks = c(0, quantiles),
      labels = paste0(c(0, quantiles[-length(quantiles)]), " - ", quantiles),
      include.lowest = TRUE,
      right = FALSE)) %>%
  arrange(stock) %>%
  as_sf


# Light mode with biogeographical regions

map_icpf(layers = "data_sf",
         title = paste0("**Forest soil ", tolower(full_var_name)," stock** · ",
                        level_long, "<br>",
                        n_distinct(data_sf$plot_id),
                        " plots ",
                        "(forest floor + soil)<br>",
                        "Mean over survey period (",
                        min(data_sf$survey_year_earliest,
                            na.rm = TRUE),
                        " - ",
                        max(data_sf$survey_year_last,
                            na.rm = TRUE),
                        ")"),
         legend_title = paste0("**", full_var_name, "<br>stock** (",
                               unit_stock, ")"),
         legend_classes = TRUE,
         variable_cat = "stock_cat",
         export_name = paste0(survey_form, "_mean_biogeo_cat"),
         export_folder = paste0(dir, "graphs/"),
         point_col = NULL,
         point_size = case_when(
           survey_form == "s1" ~ 0.1,
           survey_form == "so" ~ 0.8),
         biogeo_palette = "biogeo_col",
         count_plots_legend = FALSE,
         inset_maps_offset_x = 0.4)



# Light mode

map_icpf(layers = "data_sf",
         title = paste0("**Forest soil ", tolower(full_var_name)," stock** · ",
                        level_long, "<br>",
                        n_distinct(data_sf$plot_id),
                        " plots ",
                        "(forest floor + soil)<br>",
                        "Mean over survey period (",
                        min(data_sf$survey_year_earliest,
                            na.rm = TRUE),
                        " - ",
                        max(data_sf$survey_year_last,
                            na.rm = TRUE),
                        ")"),
         legend_title = paste0("**", full_var_name, "<br>stock** (",
                               unit_stock, ")"),
         legend_classes = TRUE,
         variable_cat = "stock_cat",
         export_name = paste0(survey_form, "_mean_cat"),
         export_folder = paste0(dir, "graphs/"),
         point_col = NULL,
         point_size = case_when(
           survey_form == "s1" ~ 0.1,
           survey_form == "so" ~ 0.8),
         biogeo_palette = NULL,
         count_plots_legend = FALSE,
         inset_maps_offset_x = 1.1)


# ESB template

# Create a colour palette
categories <- levels(data_sf$stock_cat)
point_col <- viridisLite::viridis(length(categories), direction = -1)
color_palette <- setNames(point_col, categories)

create_esb_map(data = data_sf,
               lon_col = "longitude_dec",
               lat_col = "latitude_dec",
               color_col = "stock_cat",
               color_palette = color_palette,
               point_size = case_when(
                 survey_form == "s1" ~ 0.1,
                 survey_form == "so" ~ 0.8),
               save_plot = TRUE,
               legend_title = paste0(full_var_name, " stock \n",
                                     "(forest floor + soil) \n[",
                                     sub("<sup>-1</sup>$", "-1", unit_stock),
                                     "]"),
               filename = paste0(dir, "graphs/", survey_form,
                                 "_mean_esb_cat.png"))







#### Topsoil ----

# Get boundaries from Level I, in the assumption that this is representative
# for Europe

quantiles <- get(paste0("s1_plot_", shorter_var_name, "_stocks")) %>%
  filter(stock_topsoil_plaus == TRUE) %>%
  group_by(plot_id) %>%
  reframe(stock_topsoil = mean(stock_topsoil, na.rm = TRUE)) %>%
  pull(stock_topsoil) %>%
  quantile(c(0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE) %>%
  harmonise_quantiles

if (parameter == "organic_carbon_total") {

  quantiles <- c(50, 75, 100, 150, 400)

}

data_sf <- plot_stocks %>%
  filter(!is.na(stock_topsoil)) %>%
  mutate(
    stock_topsoil_cat = cut(
      stock_topsoil,
      breaks = c(0, quantiles),
      labels = paste0(c(0, quantiles[-length(quantiles)]), " - ", quantiles),
      include.lowest = TRUE,
      right = FALSE)) %>%
  arrange(stock_topsoil) %>%
  as_sf


# Light mode with biogeographical regions

map_icpf(layers = "data_sf",
         title = paste0("**Forest soil ", tolower(full_var_name)," stock** · ",
                        level_long, "<br>",
                        n_distinct(data_sf$plot_id),
                        " plots ",
                        "(forest floor + topsoil)<br>",
                        "Mean over survey period (",
                        min(data_sf$survey_year_earliest,
                            na.rm = TRUE),
                        " - ",
                        max(data_sf$survey_year_last,
                            na.rm = TRUE),
                        ")"),
         legend_title = paste0("**", full_var_name, "<br>stock** (",
                               unit_stock, ")"),
         legend_classes = TRUE,
         variable_cat = "stock_topsoil_cat",
         export_name = paste0(survey_form, "_topsoil_mean_biogeo_cat"),
         export_folder = paste0(dir, "graphs/"),
         point_col = NULL,
         point_size = case_when(
           survey_form == "s1" ~ 0.1,
           survey_form == "so" ~ 0.8),
         biogeo_palette = "biogeo_col",
         count_plots_legend = FALSE,
         inset_maps_offset_x = 0.4)

# Light mode

map_icpf(layers = "data_sf",
         title = paste0("**Forest soil ", tolower(full_var_name)," stock** · ",
                        level_long, "<br>",
                        n_distinct(data_sf$plot_id),
                        " plots ",
                        "(forest floor + topsoil)<br>",
                        "Mean over survey period (",
                        min(data_sf$survey_year_earliest,
                            na.rm = TRUE),
                        " - ",
                        max(data_sf$survey_year_last,
                            na.rm = TRUE),
                        ")"),
         legend_title = paste0("**", full_var_name, "<br>stock** (",
                               unit_stock, ")"),
         legend_classes = TRUE,
         variable_cat = "stock_topsoil_cat",
         export_name = paste0(survey_form, "_topsoil_mean_cat"),
         export_folder = paste0(dir, "graphs/"),
         point_col = NULL,
         point_size = case_when(
           survey_form == "s1" ~ 0.1,
           survey_form == "so" ~ 0.8),
         biogeo_palette = NULL,
         count_plots_legend = FALSE,
         inset_maps_offset_x = 1.1)



# ESB template

# Create a colour palette
categories <- levels(data_sf$stock_topsoil_cat)
point_col <- viridisLite::viridis(length(categories), direction = -1)
color_palette <- setNames(point_col, categories)

create_esb_map(data = data_sf,
               lon_col = "longitude_dec",
               lat_col = "latitude_dec",
               color_col = "stock_topsoil_cat",
               color_palette = color_palette,
               point_size = case_when(
                 survey_form == "s1" ~ 0.1,
                 survey_form == "so" ~ 0.8),
               save_plot = TRUE,
               legend_title = paste0(full_var_name, " stock \n",
                                     "(forest floor + topsoil) \n[",
                                     sub("<sup>-1</sup>$", "-1", unit_stock),
                                     "]"),
               filename = paste0(dir, "graphs/", survey_form,
                                 "_topsoil_mean_esb_cat.png"))















### 6.2.3. Change ----

# (Not sure if these maps are still relevant...)

plot_stocks_ch_sf <- plot_stocks %>%
  filter(!is.na(stock_change)) %>%
  as_sf

map_icpf2(layers = "plot_stocks_ch_sf",
          title = paste0("**Annual increase** in **forest soil ",
                         tolower(full_var_name), " stock** ",
                         " · Level II<br>",
                         n_distinct(plot_stocks_ch_sf$plot_id), " plots ",
                         "(forest floor + soil)<br>",
                         min(plot_stocks_ch_sf$survey_year_earliest,
                             na.rm = TRUE),
                         " - ",
                         max(plot_stocks_ch_sf$survey_year_last,
                             na.rm = TRUE),
                         " (time span per plot: ",
                         min(plot_stocks_ch_sf$survey_year_difference,
                             na.rm = TRUE),
                         " - ",
                         max(plot_stocks_ch_sf$survey_year_difference,
                             na.rm = TRUE),
                         " years)"),
          legend_title = paste0("**Annual increase**<br>",
                                tolower(full_var_name),
                                "<br>stock<br>",
                                "(", unit_stock, " year<sup>-1</sup>)"),
          biogeo_palette = NULL,
          legend_classes = NULL,
          variable_continuous = "stock_change",
          point_size = case_when(
            survey_form == "s1" ~ 0.1,
            survey_form == "so" ~ 1.5),
          with_logo = FALSE,
          count_plots_legend = FALSE,
          mode = "dark_light",
          inset_maps_offset_x = 0.9,
          export_name = paste0(survey_form, "_change"),
          export_folder = paste0(dir, "graphs/"))








