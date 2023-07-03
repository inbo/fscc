
# Identify plots for which repeated SOC assessments are available
# (Preliminary assessment, since the final objective is to identify plots
# for which repeated below-ground C stocks are available)

# Preliminary assumption: if no data on bulk density or the coarse fragments
# content anyway is reported, it should be possible to gap-fill this info
# from other survey forms, other survey years, or through pedotransfer
# estimations.

# PathFinder
# Script initiation date: 2023-06-29
# (before meeting with Claudia and Markus 2023-06-30)


get_repeated_c_plots <- function(survey = c("so", "s1"),
                                 download_date) {

# Define required packages ----
stopifnot(require("sf"),
          require("tidyverse"),
          require("assertthat"))

# Import data ----

survey <- match.arg(survey)

if (survey == "so") {
  required_forms <- c("so_som", "coordinates_so")
} else {
  required_forms <- c("s1_som", "coordinates_s1")
}

# If any of these does not yet exist in the global environment

if (any(!(unlist(map(required_forms, exists))))) {

  # Source the required functions
  source("./src/functions/read_icpforests_csv.R")
  source("./src/functions/merge_duplicate_records.R")
  source("./src/functions/get_layer_inconsistencies.R")

  if (survey == "so") {

    # Import all data forms as well as the most common coordinates
    # using the read_icpforests_csv() function
    # "si" needs to be downloaded first, since "coordinates_so" of "so"
    # uses the coordinates of "si"
    # (This makes a difference for some Polish plots)

    read_icpforests_csv("si", download_date)
    read_icpforests_csv("so", download_date)
    
    # Merge duplicate records in "so_som"
    merge_duplicate_records("so_som", merge = TRUE)
    
    # Solve obvious layer (limit) inconsistencies
    get_layer_inconsistencies("so_som", solve = TRUE)
    

  } else if (survey == "s1") {

    read_icpforests_csv("y1", download_date)
    read_icpforests_csv("s1", download_date)
    
    # Solve obvious layer (limit) inconsistencies
    get_layer_inconsistencies("s1_som", solve = TRUE)
    

  }
}

if (survey == "so") {
  df <- so_som
  df_coordinates <- coordinates_so
  min_depth <- 40
} else if (survey == "s1") {
  df <- s1_som
  df_coordinates <- coordinates_s1
  min_depth <- 20
}

# Identify unique surveys with TOC data between 0 and 40 cm ----
# "unique surveys" (plot_id x survey_year)

df_oc <- df %>%
  # Filter for records (layers) with TOC and layer limit data
  filter(!is.na(.data$organic_carbon_total)) %>%
  filter(!is.na(.data$layer_limit_inferior)) %>%
  filter(!is.na(.data$layer_limit_superior)) %>%
  # Summarise per unique survey (plot_id x survey_year)
  # with the maximum and minimum depths for each of the unique surveys
  group_by(.data$country,
           .data$partner_code,
           .data$code_plot,
           .data$plot_id,
           .data$survey_year,
           .data$unique_survey) %>%
  summarise(max_depth_oc = max(.data$layer_limit_inferior),
            min_depth_oc = min(.data$layer_limit_superior),
            .groups = "drop") %>%
  # Only keep plots for which TOC data go until a depth of at least 40 cm
  filter(.data$max_depth_oc >= min_depth) %>%
  # From a minimum depth of 0 cm
  filter(.data$min_depth_oc <= 0)

nrow(df_oc)

# Identify "unique surveys" with all forest floor information ----
# i.e. TOC as well as organic layer weight data

# Assumption: every unique_survey has at least one forest floor layer...

df_ff <- df %>%
  # Filter for forest floor layers with organic_layer_weight (for stocks)
  # and C contents
  filter(.data$layer_type == "forest_floor") %>%
  filter((!is.na(.data$organic_layer_weight)) |
           # Organic layer weight can also be derived from layer thickness
           # and bulk density
           (is.na(.data$organic_layer_weight) &
              !is.na(.data$layer_limit_superior) &
              !is.na(.data$layer_limit_inferior) &
              !is.na(.data$bulk_density))) %>%
  # No need to filter for TOC data availability
  # Derive their unique plot_ids
  distinct(unique_survey, .keep_all = TRUE)

nrow(df_ff)

# Identify "unique surveys" with below-ground + forest floor data ----
# For stock calculations

df_joint <- inner_join(df_oc,
                       as.data.frame(df_ff$unique_survey),
                       by = join_by("unique_survey" ==
                                      "df_ff$unique_survey"))

# Identify plots with at least one survey with the required C data ----

df_per_plot_id <- df_joint %>%
  group_by(.data$country,
           .data$partner_code,
           .data$code_plot,
           .data$plot_id) %>%
  summarise(
  # How many repeated SOC assessments (surveys, i.e. different survey years)
  # occurred for the given plot_id?
  repeated_survey_count = n(),
  # Did any survey occur since survey_year 2000 for the given plot_id?
  any_after_2000 = any(survey_year >= 2000, na.rm = TRUE),
  # Did any survey occur before + since survey_year 2000 for the given plot_id?
  before_and_after_2000 = any(survey_year < 2000, na.rm = TRUE) &
    any(survey_year >= 2000, na.rm = TRUE),
  .groups = "drop") %>%
  # Add coordinates of plots
  left_join(df_coordinates, by = "plot_id") %>%
  # Filter out plots without coordinates
  # No rows should be filtered out
  filter((!is.na(.data$longitude_dec)) & (!is.na(.data$longitude_dec))) %>%
  relocate(.data$longitude_dec, .before = "repeated_survey_count") %>%
  relocate(.data$latitude_dec, .after = "longitude_dec")

nrow(df_per_plot_id)

return(df_per_plot_id)

}

# Apply function for "so" survey and save ----

so_som_plots_with_c <- get_repeated_c_plots(survey = "so",
                                            download_date = "20230605")

nrow(filter(so_som_plots_with_c, repeated_survey_count > 1))
nrow(filter(so_som_plots_with_c, any_after_2000 == TRUE))
nrow(filter(so_som_plots_with_c, before_and_after_2000 == TRUE))

write.csv2(so_som_plots_with_c,
           "./output/pathfinder/so_som_plots_with_c_prelim.csv",
           row.names = FALSE)

# Convert results "so" to spatial dataframe and save map ----

source("./src/functions/map_icpf.R")

so_som_plots_with_c_spat <- so_som_plots_with_c %>%
  filter(repeated_survey_count > 1) %>%
  st_as_sf(coords = c("longitude_dec", "latitude_dec"),
           crs = 4326) %>%
  st_transform(crs = 3035)

map_icpf(layers = "so_som_plots_with_c_spat",
         title = "Soil plots for C stocks with repeated surveys (Level II)",
         legend_title = "Plot location",
         legend_classes = "Plot location",
         export_name = "map_LII_som_plots_with_c_prelim",
         export_folder = "pathfinder",
         point_col = "red",
         point_size = 0.6)


# Apply function for "s1" survey and save ----

s1_som_plots_with_c <- get_repeated_c_plots(survey = "s1",
                                            download_date = "20230605")

nrow(filter(s1_som_plots_with_c, repeated_survey_count > 1))
nrow(filter(s1_som_plots_with_c, any_after_2000 == TRUE))
nrow(filter(s1_som_plots_with_c, before_and_after_2000 == TRUE))

write.csv2(s1_som_plots_with_c,
           "./output/pathfinder/s1_som_plots_with_c_prelim.csv",
           row.names = FALSE)

# Convert results "s1" to spatial dataframe and save map ----

source("./src/functions/map_icpf.R")

s1_som_plots_with_c_spat <- s1_som_plots_with_c %>%
  filter(repeated_survey_count > 1) %>%
  st_as_sf(coords = c("longitude_dec", "latitude_dec"),
           crs = 4326) %>%
  st_transform(crs = 3035)

map_icpf(layers = "s1_som_plots_with_c_spat",
         title = "Soil plots for C stocks with repeated surveys (Level I)",
         legend_title = "Plot location",
         legend_classes = "Plot location",
         export_name = "map_LI_som_plots_with_c_prelim",
         export_folder = "pathfinder",
         point_col = "red",
         point_size = 0.6)
