
# Question:
# Deliver C/N ratios of 0 - 10 cm depth layer and forest floor + pH
# (LII; starting from 1996) as a measure for soil fertility
# in LII plots with ground vegetation data

# Script initiation date: 31 Oct 2023


# 1. Prepare required packages ----

stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"))

# 2. Import "layer 1" data for soil (Level II) ----

if (!exists("so_som")) {
source("./src/functions/read_processed.R")
read_processed(survey_forms = c("si", "so"),
               save_to_env = TRUE)
}

so_som_working <- so_som %>%
  # Filter for records starting from 1996
  filter(.data$survey_year >= 1996) %>%
  # Remove redundant layers
  filter(!is.na(.data$layer_number))


# 3. Import list of plots with ground vegetation data ----

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

# Assert: No issue with Polish plot codes

assertthat::assert_that(
  min(plots_vegetation$code_plot[which(plots_vegetation$code_country == 53)]) >
    100)



# 4. Evaluate for each of the plot_ids ----

# Set progress bar
progress_bar <- txtProgressBar(min = 0,
                               max = nrow(plots_vegetation), style = 3)

for (i in seq_len(nrow(plots_vegetation))) {
  
  plot_id_i <- plots_vegetation$plot_id[i]
  
  # If the plot_id appears in "so"
  
  if (!identical(which(so_som_working$plot_id == plot_id_i),
                 integer(0))) {
  
  
## 4.1. 0 - 10 cm depth layer ----
  
    # Some of the below-ground fixed-depth layers are not "conform"
    # the theoretical depths (e.g. 4_503)
    # Therefore, harmonise the layers covering the 0 - 10 cm depth range
    
    # Select the layers covering the 0 - 10 cm depth range
    
    limit_range <- seq(10, 0)
    
    df_sub <- so_som_working %>%
      filter(.data$plot_id == plot_id_i) %>%
      filter(!is.na(layer_limit_superior) &
               !is.na(layer_limit_inferior)) %>%
      rowwise() %>%
      filter(any(limit_range > .data$layer_limit_superior &
                   limit_range < .data$layer_limit_inferior))
    
    # Apply the function harmonise_layer_to_depths.
    # This function selects layers within the specified depth range
    # and calculates a weighted average based on the soil mass
    # contribution (overlapping depth range x bulk density) of each layer
    # to the specified depth range.
    
    source("./src/functions/harmonise_layer_to_depths.R")
    
    df_0_10 <- df_sub %>%
      group_by(unique_survey_repetition, plot_id) %>%
      reframe(organic_carbon_total_avg =
                  harmonise_layer_to_depths(limit_sup = 0,
                                            limit_inf = 10,
                                            bulk_density =
                                              .data$bulk_density,
                                            upper_depths =
                                              .data$layer_limit_superior,
                                            lower_depths =
                                              .data$layer_limit_inferior,
                                            variab =
                                              .data$organic_carbon_total,
                                            parameter_name =
                                              "organic_carbon_total"),
                n_total_avg =
                  harmonise_layer_to_depths(limit_sup = 0,
                                            limit_inf = 10,
                                            bulk_density =
                                              .data$bulk_density,
                                            upper_depths =
                                              .data$layer_limit_superior,
                                            lower_depths =
                                              .data$layer_limit_inferior,
                                            variab =
                                              .data$n_total,
                                            parameter_name =
                                              "n_total"),
                ph_cacl2_avg =
                  harmonise_layer_to_depths(limit_sup = 0,
                                            limit_inf = 10,
                                            bulk_density =
                                              .data$bulk_density,
                                            upper_depths =
                                              .data$layer_limit_superior,
                                            lower_depths =
                                              .data$layer_limit_inferior,
                                            variab =
                                              .data$ph_cacl2,
                                            parameter_name =
                                              "ph_cacl2"),
                ph_h2o_avg =
                  harmonise_layer_to_depths(limit_sup = 0,
                                            limit_inf = 10,
                                            bulk_density =
                                              .data$bulk_density,
                                            upper_depths =
                                              .data$layer_limit_superior,
                                            lower_depths =
                                              .data$layer_limit_inferior,
                                            variab =
                                              .data$ph_h2o,
                                            parameter_name =
                                              "ph_h2o"),
                # LOQs
                organic_carbon_total_loq =
                  harmonise_layer_to_depths(limit_sup = 0,
                                            limit_inf = 10,
                                            bulk_density =
                                              .data$bulk_density,
                                            upper_depths =
                                              .data$layer_limit_superior,
                                            lower_depths =
                                              .data$layer_limit_inferior,
                                            variab =
                                              .data$organic_carbon_total_loq,
                                            parameter_name =
                                              "organic_carbon_total_loq"),
                n_total_loq =
                  harmonise_layer_to_depths(limit_sup = 0,
                                            limit_inf = 10,
                                            bulk_density =
                                              .data$bulk_density,
                                            upper_depths =
                                              .data$layer_limit_superior,
                                            lower_depths =
                                              .data$layer_limit_inferior,
                                            variab =
                                              .data$n_total_loq,
                                            parameter_name =
                                              "n_total_loq"),
                .groups = "drop") %>%
      # Aggregate per plot_id
      group_by(plot_id) %>%
      reframe(organic_carbon_total_avg =
                  mean(organic_carbon_total_avg, na.rm = TRUE),
                n_total_avg =
                  mean(n_total_avg, na.rm = TRUE),
                ph_cacl2_avg =
                  mean(ph_cacl2_avg, na.rm = TRUE),
                ph_h2o_avg =
                  mean(ph_h2o_avg, na.rm = TRUE),
                # LOQs
                organic_carbon_total_loq =
                  mean(organic_carbon_total_loq, na.rm = TRUE),
                n_total_loq =
                  mean(n_total_loq, na.rm = TRUE))
  
  
  # If any 0 - 10 cm layers were reported for the given plot_id
  
  if (nrow(df_0_10) > 0) {
    
  plots_vegetation$c_to_n_0_10[i] <-
    ifelse(is.na(df_0_10$organic_carbon_total_avg) |
             is.na(df_0_10$n_total_avg),
           NA,
           # If any value is below LOQ
           ifelse((df_0_10$organic_carbon_total_avg <
                     df_0_10$organic_carbon_total_loq) ||
                    (df_0_10$n_total_avg < df_0_10$n_total_loq),
                  -1,
                  round(df_0_10$organic_carbon_total_avg / df_0_10$n_total_avg,
                        2)))
  
  plots_vegetation$ph_cacl2_0_10[i] <- round(df_0_10$ph_cacl2_avg, 2)
  
  plots_vegetation$ph_h2o_0_10[i] <- round(df_0_10$ph_h2o_avg, 2)
  
  }
  
  
## 4.2. Forest floor ----
  
  df_ff <- so_som_working %>%
    filter(.data$plot_id == plot_id_i) %>%
    filter(.data$layer_type == "forest_floor") %>%
    # Aggregate per unique_survey_repetition (by taking a weighted average)
    group_by(unique_survey_repetition, plot_id) %>%
    reframe(organic_carbon_total_avg =
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
              # LOQs
              organic_carbon_total_loq =
                ifelse(!is.na(.data$organic_layer_weight),
                       weighted.mean(organic_carbon_total_loq,
                                     w = organic_layer_weight,
                                     na.rm = TRUE),
                       mean(organic_carbon_total_loq, na.rm = TRUE)),
              n_total_loq =
                ifelse(!is.na(.data$organic_layer_weight),
                       weighted.mean(n_total_loq,
                                     w = organic_layer_weight,
                                     na.rm = TRUE),
                       mean(n_total_loq, na.rm = TRUE)),
              .groups = "drop") %>%
    # Aggregate per plot_id
    group_by(plot_id) %>%
    reframe(organic_carbon_total_avg =
                mean(organic_carbon_total_avg, na.rm = TRUE),
              n_total_avg =
                mean(n_total_avg, na.rm = TRUE),
              ph_cacl2_avg =
                mean(ph_cacl2_avg, na.rm = TRUE),
              ph_h2o_avg =
                mean(ph_h2o_avg, na.rm = TRUE),
              # LOQs
              organic_carbon_total_loq =
                mean(organic_carbon_total_loq, na.rm = TRUE),
              n_total_loq =
                mean(n_total_loq, na.rm = TRUE))
  
  
  # If any forest floor layers were reported for the given plot_id
  
  if (nrow(df_ff) > 0) {
  
  plots_vegetation$c_to_n_forest_floor[i] <-
    ifelse(is.na(df_ff$organic_carbon_total_avg) |
             is.na(df_ff$n_total_avg),
           NA,
           # If any value is below LOQ
           ifelse((df_0_10$organic_carbon_total_avg <
                     df_0_10$organic_carbon_total_loq) ||
                    (df_0_10$n_total_avg < df_0_10$n_total_loq),
                  -1,
                  round(df_ff$organic_carbon_total_avg / df_ff$n_total_avg,
                        2)))
  
  plots_vegetation$ph_cacl2_forest_floor[i] <- round(df_ff$ph_cacl2_avg, 2)
  
  plots_vegetation$ph_h2o_forest_floor[i] <- round(df_ff$ph_h2o_avg, 2)
  
  }
  
  }
  
  # Update progress bar
  setTxtProgressBar(progress_bar, i)
}

close(progress_bar)

plots_vegetation <- plots_vegetation %>%
  mutate_all(~ifelse(is.nan(.), NA, .))


# 5. Add eutric/dystric qualifier ----

# This qualitative indicator is just a rough approximation, since the required
# information is sometimes incomplete.
# Basically, eutric/dystric qualifiers of a profile in WRB represent the
# base saturation in the 20 - 100 cm depth range.

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

plots_vegetation <- plots_vegetation %>%
  left_join(so_prf_adds,
            by = "plot_id") %>%
  mutate(base_saturation_qualitative = case_when(
    .data$base_saturation_qualitative == "low" ~ "< 50 %",
    .data$base_saturation_qualitative == "high" ~ ">= 50 %",
    TRUE ~ NA_character_))

View(plots_vegetation)

plot(plots_vegetation$c_to_n_0_10, plots_vegetation$c_to_n_forest_floor)
colSums(!is.na(plots_vegetation))


# 6. Export ----

write.csv2(plots_vegetation,
           paste0("./output/specific_esb_requests/",
                  "plots_vegetation_with_soil_fertility_data.csv"),
           row.names = FALSE,
           na = "")








