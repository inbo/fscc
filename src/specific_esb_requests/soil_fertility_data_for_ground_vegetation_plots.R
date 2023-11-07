
# Question:
# Deliver C/N ratios of 0 - 10 cm depth layer and forest floor + pH
# (LII; starting from 1996) as a measure for soil fertility
# in LII plots with ground vegetation data

# Script initiation date: 31 Oct 2023


# Prepare required packages ----

stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"))

# Import "layer 1" data for soil (Level II) ----

if (!exists("so_som")) {
source("./src/functions/read_processed.R")
read_processed(survey_forms = c("si", "so"),
               save_to_env = TRUE)
}

so_som_working <- so_som %>%
  # Filter for records starting from 1996
  filter(.data$survey_year >= 1996) %>%
  # Filter for records not deeper than 20 cm
  # (10 cm is requested, but some plots only have a layer M02, e.g. "6_715")
  filter(is.na(.data$layer_limit_inferior) |
           .data$layer_limit_inferior <= 20)


# Import list of plots with ground vegetation data ----

plots_vegetation <-
  read.csv(paste0("./data/additional_data/",
                  "ground_vegetation_plots_for_soil_data_esb/",
                  "plots_vegetation.csv")) %>%
  # Create a column "plot_id"
  mutate(plot_id = paste0(.data$code_country, "_",
                          .data$code_plot)) %>%
  # Create new columns for the soil data
  mutate(c_to_n_0_10 = NA,
         ph_cacl2_0_10 = NA,
         ph_h2o_0_10 = NA,
         c_to_n_forest_floor = NA,
         ph_cacl2_forest_floor = NA,
         ph_h2o_forest_floor = NA)

# Check: No issue with Polish plot codes
assertthat::assert_that(
  min(plots_vegetation$code_plot[which(plots_vegetation$code_country == 53)]) >
    100)



# Evaluate for each of the plot_ids ----

# Set progress bar
progress_bar <- txtProgressBar(min = 0,
                               max = nrow(plots_vegetation), style = 3)

for (i in seq_len(nrow(plots_vegetation))) {
  
  plot_id_i <- plots_vegetation$plot_id[i]
  
  # If the plot_id appears in "so"
  
  if (!identical(which(so_som_working$plot_id == plot_id_i),
                 integer(0))) {
  
  ## 0 - 10 cm depth layer ----
  
  # First: focus on layers M05/H05, M51/H51, M01/H01
    
  suppressWarnings({
    
  df_0_10 <- so_som_working %>%
    filter(.data$plot_id == plot_id_i) %>%
    filter(.data$layer_type %in% c("peat", "mineral")) %>%
    filter(.data$layer_limit_inferior <= 10) %>%
    # Aggregate per unique_survey_repetition (by taking a weighted average)
    # In theory, there are two options:
    # - Only M01/H01 appears: no weighted average needed
    # - Both M05/H05 and M51/H51 appear: the two layers have the same
    #   layer thickness (i.e. 5 cm), so weighting can happen based on
    #   bulk density
    group_by(unique_survey_repetition, plot_id) %>%
    summarise(organic_carbon_total_avg =
                ifelse(!is.na(.data$bulk_density),
                       weighted.mean(organic_carbon_total,
                                     w = bulk_density,
                                     na.rm = TRUE),
                       mean(organic_carbon_total, na.rm = TRUE)),
              n_total_avg =
                ifelse(!is.na(.data$bulk_density),
                       weighted.mean(n_total,
                                     w = bulk_density,
                                     na.rm = TRUE),
                       mean(n_total, na.rm = TRUE)),
              ph_cacl2_avg =
                ifelse(!is.na(.data$bulk_density),
                       weighted.mean(ph_cacl2,
                                     w = bulk_density,
                                     na.rm = TRUE),
                       mean(ph_cacl2, na.rm = TRUE)),
              ph_h2o_avg =
                ifelse(!is.na(.data$bulk_density),
                       weighted.mean(ph_h2o,
                                     w = bulk_density,
                                     na.rm = TRUE),
                       mean(ph_h2o, na.rm = TRUE)),
              # max LOQs
              .groups = "drop")
  })
  
  # If any of the originally filtered profiles does not appear in the
  # table above (df_0_10),
  # which means that there are profiles with a layer M02/H02,
  # add their info
  
  prof_orig <- so_som_working %>%
    filter(.data$plot_id == plot_id_i) %>%
    filter(.data$layer_type %in% c("peat", "mineral")) %>%
    distinct(unique_survey_repetition)
  
  if (any(!prof_orig %in% df_0_10$unique_survey_repetition)) {
    
    df_0_20 <- so_som_working %>%
      filter(.data$plot_id == plot_id_i) %>%
      filter(.data$layer_type %in% c("peat", "mineral")) %>%
      # only profiles that did not yet appear in df_0_10
      filter(!unique_survey_repetition %in%
               df_0_10$unique_survey_repetition) %>%
      # Aggregate per unique_survey_repetition
      group_by(unique_survey_repetition, plot_id) %>%
      summarise(organic_carbon_total_avg =
                  ifelse(!is.na(.data$bulk_density),
                         weighted.mean(organic_carbon_total,
                                       w = bulk_density,
                                       na.rm = TRUE),
                         mean(organic_carbon_total, na.rm = TRUE)),
                n_total_avg =
                  ifelse(!is.na(.data$bulk_density),
                         weighted.mean(n_total,
                                       w = bulk_density,
                                       na.rm = TRUE),
                         mean(n_total, na.rm = TRUE)),
                ph_cacl2_avg =
                  ifelse(!is.na(.data$bulk_density),
                         weighted.mean(ph_cacl2,
                                       w = bulk_density,
                                       na.rm = TRUE),
                         mean(ph_cacl2, na.rm = TRUE)),
                ph_h2o_avg =
                  ifelse(!is.na(.data$bulk_density),
                         weighted.mean(ph_h2o,
                                       w = bulk_density,
                                       na.rm = TRUE),
                         mean(ph_h2o, na.rm = TRUE)),
                .groups = "drop")
    
    # Add the aggregated data of these profiles to the 0 - 10 cm profiles
    
    df_0_10 <- rbind(df_0_10,
                     df_0_20)
    
  }
  
  
  suppressWarnings({
  
  df_0_10 <- df_0_10 %>%
    # Aggregate per plot_id
    group_by(plot_id) %>%
    summarise(organic_carbon_total_avg =
                mean(organic_carbon_total_avg, na.rm = TRUE),
              n_total_avg =
                mean(n_total_avg, na.rm = TRUE),
              ph_cacl2_avg =
                mean(ph_cacl2_avg, na.rm = TRUE),
              ph_h2o_avg =
                mean(ph_h2o_avg, na.rm = TRUE))
  })
  
  # If any 0 - 10 cm layers were reported for the given plot_id
  
  if (nrow(df_0_10) > 0) {
    
  plots_vegetation$c_to_n_0_10[i] <-
    ifelse(is.na(df_0_10$organic_carbon_total_avg) |
             is.na(df_0_10$n_total_avg),
           NA,
           df_0_10$organic_carbon_total_avg / df_0_10$n_total_avg)
  
  plots_vegetation$ph_cacl2_0_10[i] <- df_0_10$ph_cacl2_avg
  
  plots_vegetation$ph_h2o_0_10[i] <- df_0_10$ph_h2o_avg
  
  }
  
  
  ## Forest floor ----
  
  suppressWarnings({
  
  df_ff <- so_som_working %>%
    filter(.data$plot_id == plot_id_i) %>%
    filter(.data$layer_type == "forest_floor") %>%
    # Aggregate per unique_survey_repetition (by taking a weighted average)
    group_by(unique_survey_repetition, plot_id) %>%
    summarise(organic_carbon_total_avg =
                ifelse(!is.na(.data$organic_layer_weight),
                       weighted.mean(organic_carbon_total,
                                     w = organic_layer_weight,
                                     na.rm = TRUE),
                       mean(organic_carbon_total, na.rm = TRUE)),
              n_total_avg =
                ifelse(!is.na(.data$organic_layer_weight),
                       weighted.mean(n_total,
                                     w = organic_layer_weight,
                                     na.rm = TRUE),
                       mean(n_total, na.rm = TRUE)),
              ph_cacl2_avg =
                ifelse(!is.na(.data$organic_layer_weight),
                       weighted.mean(ph_cacl2,
                                     w = organic_layer_weight,
                                     na.rm = TRUE),
                       mean(ph_cacl2, na.rm = TRUE)),
              ph_h2o_avg =
                ifelse(!is.na(.data$organic_layer_weight),
                       weighted.mean(ph_h2o,
                                     w = organic_layer_weight,
                                     na.rm = TRUE),
                       mean(ph_h2o, na.rm = TRUE)),
              .groups = "drop") %>%
    # Aggregate per plot_id
    group_by(plot_id) %>%
    summarise(organic_carbon_total_avg =
                mean(organic_carbon_total_avg, na.rm = TRUE),
              n_total_avg =
                mean(n_total_avg, na.rm = TRUE),
              ph_cacl2_avg =
                mean(ph_cacl2_avg, na.rm = TRUE),
              ph_h2o_avg =
                mean(ph_h2o_avg, na.rm = TRUE))
  
  })
  
  # If any forest floor layers were reported for the given plot_id
  
  if (nrow(df_ff) > 0) {
  
  plots_vegetation$c_to_n_forest_floor[i] <-
    ifelse(is.na(df_ff$organic_carbon_total_avg) |
             is.na(df_ff$n_total_avg),
           NA,
           df_ff$organic_carbon_total_avg / df_ff$n_total_avg)
  
  plots_vegetation$ph_cacl2_forest_floor[i] <- df_ff$ph_cacl2_avg
  
  plots_vegetation$ph_h2o_forest_floor[i] <- df_ff$ph_h2o_avg
  
  }
  
  }
  
  # Update progress bar
  setTxtProgressBar(progress_bar, i)
}

close(progress_bar)

plots_vegetation <- plots_vegetation %>%
  mutate_all(~ifelse(is.nan(.), NA, .))


# Add eutric/dystric qualifier ----

suppressWarnings({
  
so_prf_adds <-
  openxlsx::read.xlsx(paste0("./data/additional_data/",
                             "SO_PRF_ADDS.xlsx"),
                      sheet = 2) %>%
  rename(base_saturation_qualitative = "BS.(high/low)",
         plot_id = PLOT_ID) %>%
  filter(!is.na(.data$base_saturation_qualitative)) %>%
  # Aggregate per plot_id by taking the most abundant class
  group_by(plot_id) %>%
  # Sometimes there are different options, e.g. plot_id 60_9
  # No good way to solve this - we just have to pick one
  summarise(base_saturation_qualitative =
              names(which.max(table(
                .data$base_saturation_qualitative[!is.na(
                  .data$base_saturation_qualitative)]))))

})

plots_vegetation <- plots_vegetation %>%
  left_join(so_prf_adds,
            by = "plot_id") %>%
  mutate(base_saturation_qualitative =
           as.factor(.data$base_saturation_qualitative))

View(plots_vegetation)


# Export ----

write.csv2(plots_vegetation,
           paste0("./output/specific_esb_requests/",
                  "plots_vegetation_with_soil_fertility_data.csv"),
           row.names = FALSE,
           na = "")








