

# Maps showing data coverage of ICP Forests

# Script initiation date: 20240306

source("./src/functions/map_icpf.R")
library(patchwork)

# LI (may 2024) ----

# Extra plots Italy

s1_som_fscdb <- openxlsx::read.xlsx(paste0("data/additional_data/fscdb_LI/",
                                    "20180615_s1_som_incl_fieldnames.xlsx"),
                                    1)

s1_fscdb_access <- read.csv(paste0("./data/additional_data/fscdb_LI/",
                                   "original_access_versions/",
                                   "s1_fscdb_access_harmonised_r.csv"),
                            sep = ";")

unique_surveys_fscdb <- bind_rows(
   # Version FSCDB
   s1_som_fscdb %>%
      mutate(code_country = as.numeric(code_country)) %>%
      mutate(code_country = ifelse(is.na(code_country),
                                   # Manually verified
                                   13,
                                   .data$code_country)) %>%
      mutate(gapfilled_survey_year = (!is.na(date_labor_analyses) &
                                         is.na(survey_year)),
             date_labor_analyses =
                # 1900 date system in Excel
                as.Date(date_labor_analyses, origin = "1899-12-30")) %>%
      mutate(survey_year = ifelse(
         is.na(survey_year),
         format(date_labor_analyses, "%Y"),
         survey_year)) %>%
      mutate(survey_year = as.numeric(survey_year)) %>%
      mutate(plot_id = paste0(code_country, "_", code_plot),
             unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot)) %>%
      mutate(layer_type = case_when(
         startsWith(code_layer, "O") | layer_limit_superior < 0 ~ "forest_floor",
         startsWith(code_layer, "H") & layer_limit_superior >= 0 ~ "peat",
         TRUE ~ "mineral"
      )) %>%
      left_join(d_country %>%
                   rename(code_country = code) %>%
                   rename(country = lib_country) %>%
                   select(code_country, country),
                by = "code_country") %>%
      mutate(layer_limit_inferior = as.numeric(layer_limit_inferior),
             layer_limit_superior = as.numeric(layer_limit_superior)) %>%
      select(unique_survey,
             country, code_country, code_plot, plot_id, survey_year, code_layer,
             layer_type, layer_limit_superior, layer_limit_inferior) %>%
      mutate(source = "FSCDB.LI.1"),
   # Version Access
   s1_fscdb_access %>%
      select(unique_survey,
             country, code_country, code_plot, plot_id, survey_year, code_layer,
             layer_type, layer_limit_superior, layer_limit_inferior) %>%
      mutate(survey_year = as.numeric(survey_year)) %>%
      mutate(source = "FSCDB.LI (Access)")) %>%
   arrange(country, code_plot, layer_limit_superior) %>%
   group_by(unique_survey,
            country, code_country, code_plot, plot_id, survey_year) %>%
   reframe(any_ff = any(layer_type == "forest_floor" &
                           is.na(layer_limit_superior) |
                           (layer_limit_superior < 0)),
           any_20_cm = any(!is.na(layer_limit_inferior) &
                              (layer_limit_inferior >= 20)),
           any_80_cm = any(!is.na(layer_limit_inferior) &
                              (layer_limit_inferior >= 80))) %>%
   ungroup





s1_som_survey_summ <- s1_som %>%
  group_by(unique_survey,
           country, code_country, code_plot, plot_id, survey_year) %>%
  reframe(any_ff = any(layer_type == "forest_floor" &
                         is.na(layer_limit_superior) |
                         (layer_limit_superior < 0)),
          any_20_cm = any(!is.na(layer_limit_inferior) &
                            (layer_limit_inferior >= 20)),
          any_80_cm = any(!is.na(layer_limit_inferior) &
                            (layer_limit_inferior >= 80))) %>%
  ungroup




# unique_surveys

missing_surveys_s1 <- unique_surveys_fscdb %>%
   filter(!is.na(survey_year)) %>%
   filter(!unique_survey %in% s1_som_survey_summ$unique_survey) %>%
   arrange(country, code_plot) %>%
   distinct(unique_survey, .keep_all = TRUE) %>%
   left_join(data_availability_s1 %>%
                select(plot_id, survey_years),
             by = "plot_id") %>%
   mutate(any_close_survey_year = NA)

# All plot_ids are in layer 1

unique_surveys_fscdb %>%
   filter(is.na(survey_year)) %>%
   filter(!plot_id %in% s1_som_survey_summ$plot_id) %>%
   arrange(country, code_plot) %>%
   distinct(plot_id, .keep_all = TRUE)

for (i in seq_len(nrow(missing_surveys_s1))) {

   survey_years_i <-
      as.numeric(unlist(str_split(missing_surveys_s1$survey_years[i], "_")))

   survey_year_fscdb_i <- as.numeric(missing_surveys_s1$survey_year[i])

      missing_surveys_s1$any_close_survey_year[i] <-
         any(abs(survey_year_fscdb_i - survey_years_i) <= 3)

   if (is.na(missing_surveys_s1$any_close_survey_year[i])) {
      missing_surveys_s1$any_close_survey_year[i] <- FALSE
   }
}

write.table(missing_surveys_s1 %>% arrange(any_close_survey_year),
            file = "./data/additional_data/missing_surveys_s1.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

missing_surveys_s1_selected <- missing_surveys_s1 %>%
   filter(any_close_survey_year == FALSE) %>%
   # Manually verified
   filter(!code_country %in% c(14, 15, 58)) %>%
   filter(survey_year < 2000) %>%
   distinct(unique_survey) %>%
   pull(unique_survey)








s1_som_summ <- bind_rows(
   s1_som_survey_summ,
   unique_surveys_fscdb %>%
      filter(unique_survey %in% missing_surveys_s1_selected) %>%
      mutate(survey_year = as.numeric(survey_year))) %>%
  arrange(country, code_plot, survey_year) %>%
  group_by(plot_id,
           country, code_country, code_plot) %>%
  reframe(any_ff = any(any_ff == TRUE),
          any_20_cm = any(any_20_cm == TRUE),
          any_80_cm = any(any_80_cm == TRUE),
          count = n(),
          year_range = max(survey_year) - min(survey_year),
          before_and_after_2000 =
            any(survey_year < 2000) & any(survey_year >= 2000)) %>%
  ungroup %>%
  arrange(country, code_plot) %>%
  left_join(coordinates_s1, by = "plot_id") %>%
  left_join(s1_plot_data %>%
               mutate(plot_id = paste0(COUNTRYID, "_", PLOTNR)) %>%
               rename(latitude_dec2 = latitude_dec) %>%
               rename(longitude_dec2 = longitude_dec) %>%
               select(plot_id, latitude_dec2, longitude_dec2),
            by = "plot_id") %>%
   mutate(latitude_dec = coalesce(latitude_dec, latitude_dec2),
          longitude_dec = coalesce(longitude_dec, longitude_dec2)) %>%
  as_sf

df_remaining_plots <- s1_som_summ %>%
  filter(count == 1)

df_20_cm <- s1_som_summ %>%
  filter(count > 1 &
           any_80_cm == FALSE &
           any_20_cm == TRUE)

df_80_cm <- s1_som_summ %>%
  filter(count > 1 &
           any_80_cm == TRUE &
           any_20_cm == TRUE)

 map_icpf(layers = c("df_remaining_plots",
                     "df_20_cm",
                     "df_80_cm"),
          title = "ICP Forests (solid soil) · **Level I**",
          legend_title =
            "**Data availability<span style='color:white'>...........</span>**",
          legend_classes = c(paste0("One survey"),
                             paste0("Repeated surveys<br>",
                                    "(0 - 20 cm)"),
                             paste0("Repeated surveys<br>",
                                    "(0 - 80 cm)")),
          export_name = "map_s1_data_availability3",
          export_folder = "other_graphs",
          point_size = 0.1,
          biogeo_palette = NULL,
          with_logo = TRUE,
          count_plots_legend = TRUE)



















 # LII (may 2024) ----

 so_som_summ <- so_som %>%
   group_by(unique_survey,
            country, code_country, code_plot, plot_id, survey_year) %>%
   reframe(any_ff = any(layer_type == "forest_floor" &
                          is.na(layer_limit_superior) |
                          (layer_limit_superior < 0)),
           any_20_cm = any(!is.na(layer_limit_inferior) &
                             (layer_limit_inferior >= 20)),
           any_80_cm = any(!is.na(layer_limit_inferior) &
                             (layer_limit_inferior >= 80))) %>%
   ungroup %>%
   arrange(country, code_plot, survey_year) %>%
   group_by(plot_id,
            country, code_country, code_plot) %>%
   reframe(any_ff = any(any_ff == TRUE),
           any_20_cm = any(any_20_cm == TRUE),
           any_80_cm = any(any_80_cm == TRUE),
           count = n(),
           year_range = max(survey_year) - min(survey_year),
           before_and_after_2000 =
             any(survey_year < 2000) & any(survey_year >= 2000)) %>%
   ungroup %>%
   arrange(country, code_plot) %>%
   left_join(coordinates_so, by = "plot_id") %>%
   as_sf

 df_remaining_plots <- so_som_summ %>%
   filter(count == 1)

 df_20_cm <- so_som_summ %>%
   filter(count > 1 &
            any_80_cm == FALSE &
            any_20_cm == TRUE)

 df_80_cm <- so_som_summ %>%
   filter(count > 1 &
            any_80_cm == TRUE &
            any_20_cm == TRUE)

 map_icpf(layers = c("df_remaining_plots",
                     "df_20_cm",
                     "df_80_cm"),
          title = "ICP Forests (solid soil) · **Level II**",
          legend_title =
            "**Data availability<span style='color:white'>...........</span>**",
          legend_classes = c(paste0("One survey"),
                             paste0("Repeated surveys<br>",
                                    "(0 - 20 cm)"),
                             paste0("Repeated surveys<br>",
                                    "(0 - 80 cm)")),
          export_name = "map_so_data_availability",
          export_folder = "other_graphs",
          point_size = 0.6,
          biogeo_palette = NULL,
          with_logo = TRUE,
          count_plots_legend = TRUE)







# Layer 0 LI ----

source("./src/functions/read_raw.R")

read_raw("y1", save_to_env = TRUE)
read_raw("s1", save_to_env = TRUE)

s1_strat <- read.csv("./data/additional_data/s1_strat.csv", sep = ";")

d_depth_level_soil <-
   read.csv("./data/additional_data/d_depth_level_soil.csv",
            sep = ";") %>%
   select(code,
          layer_limit_superior,
          layer_limit_inferior) %>%
   rename(layer_limit_superior_theory =
             layer_limit_superior) %>%
   rename(layer_limit_inferior_theory =
             layer_limit_inferior)

s1_som_summ <- s1_som %>%
   left_join(d_depth_level_soil,
             by = join_by(code_layer == code)) %>%
   mutate(layer_limit_superior =
             ifelse(!is.na(.data$layer_limit_superior),
                    .data$layer_limit_superior,
                    .data$layer_limit_superior_theory)) %>%
   mutate(layer_limit_inferior =
             ifelse(!is.na(.data$layer_limit_inferior),
                    .data$layer_limit_inferior,
                    .data$layer_limit_inferior_theory)) %>%
   left_join(s1_strat %>%
                select(plot_id, eff_soil_depth),
             by = "plot_id") %>%
   group_by(unique_survey,
            country, code_country, code_plot, plot_id, survey_year,
            eff_soil_depth) %>%
   reframe(
      obs_max = ifelse(
         any(!is.na(layer_limit_inferior)),
         max(layer_limit_inferior, na.rm = TRUE),
         NA_real_),
      any_ff = any(layer_type == "forest_floor" &
                      is.na(layer_limit_superior) |
                      (layer_limit_superior < 0)),
      any_20_cm = any(!is.na(layer_limit_inferior) &
                         ((layer_limit_inferior >= 20) |
                             (eff_soil_depth < 20 &
                                 (layer_limit_inferior >=
                                     0.8 * eff_soil_depth)))),
      any_80_cm = any(!is.na(layer_limit_inferior) &
                         ((layer_limit_inferior >= 80) |
                             (eff_soil_depth < 80 &
                                 (layer_limit_inferior >=
                                     0.8 * eff_soil_depth))))) %>%
   ungroup %>%
   arrange(country, code_plot, survey_year) %>%
   group_by(plot_id,
            country, code_country, code_plot) %>%
   reframe(any_ff = any(any_ff == TRUE),
           any_20_cm = any(any_20_cm == TRUE),
           any_80_cm = any(any_80_cm == TRUE),
           count = n(),
           year_range = max(survey_year) - min(survey_year),
           before_and_after_2000 =
              any(survey_year < 2000) & any(survey_year >= 2000)) %>%
   ungroup %>%
   arrange(country, code_plot) %>%
   left_join(coordinates_s1, by = "plot_id") %>%
   as_sf




df_remaining_plots <- s1_som_summ %>%
   filter(count == 1)

df_20_cm <- s1_som_summ %>%
   filter(count > 1 &
             any_80_cm == FALSE &
             any_20_cm == TRUE)

df_80_cm <- s1_som_summ %>%
   filter(count > 1 &
             any_80_cm == TRUE &
             any_20_cm == TRUE)

map_icpf(layers = c("df_remaining_plots",
                    "df_20_cm",
                    "df_80_cm"),
         title = "ICP Forests (solid soil) · **Level I (layer 0)**",
         legend_title =
            "**Data availability<span style='color:white'>...........</span>**",
         legend_classes = c(paste0("One survey"),
                            paste0("Repeated surveys<br>",
                                   "(0 - 20 cm)"),
                            paste0("Repeated surveys<br>",
                                   "(0 - 80 cm)")),
         export_name = "map_s1_data_availability_layer0",
         export_folder = "other_graphs",
         point_size = 0.1,
         biogeo_palette = NULL,
         with_logo = FALSE,
         count_plots_legend = TRUE)








# Layer 0 LII ----

source("./src/functions/read_raw.R")

read_raw("si", save_to_env = TRUE)
read_raw("so", save_to_env = TRUE)
read_raw("sw", save_to_env = TRUE)

so_strat <- read.csv("./data/additional_data/so_strat.csv", sep = ";")

so_som_summ <- so_som %>%
   left_join(d_depth_level_soil,
             by = join_by(code_layer == code)) %>%
   mutate(layer_limit_superior =
             ifelse(!is.na(.data$layer_limit_superior),
                    .data$layer_limit_superior,
                    .data$layer_limit_superior_theory)) %>%
   mutate(layer_limit_inferior =
             ifelse(!is.na(.data$layer_limit_inferior),
                    .data$layer_limit_inferior,
                    .data$layer_limit_inferior_theory)) %>%
   left_join(so_strat %>%
                select(plot_id, eff_soil_depth),
             by = "plot_id") %>%
   group_by(unique_survey,
            country, code_country, code_plot, plot_id, survey_year,
            eff_soil_depth) %>%
   reframe(
      obs_max = ifelse(
         any(!is.na(layer_limit_inferior)),
         max(layer_limit_inferior, na.rm = TRUE),
         NA_real_),
      any_ff = any(layer_type == "forest_floor" &
                      is.na(layer_limit_superior) |
                      (layer_limit_superior < 0)),
      any_20_cm = any(!is.na(layer_limit_inferior) &
                         ((layer_limit_inferior >= 20) |
                             (eff_soil_depth < 20 &
                                 (layer_limit_inferior >=
                                     0.8 * eff_soil_depth)))),
      any_80_cm = any(!is.na(layer_limit_inferior) &
                         ((layer_limit_inferior >= 80) |
                         (eff_soil_depth < 80 &
                             (layer_limit_inferior >=
                                    0.8 * eff_soil_depth))))) %>%
   ungroup %>%
   arrange(country, code_plot, survey_year) %>%
   group_by(plot_id,
            country, code_country, code_plot) %>%
   reframe(any_ff = any(any_ff == TRUE),
           any_20_cm = any(any_20_cm == TRUE),
           any_80_cm = any(any_80_cm == TRUE),
           count = n(),
           year_range = max(survey_year) - min(survey_year),
           before_and_after_2000 =
              any(survey_year < 2000) & any(survey_year >= 2000)) %>%
   ungroup %>%
   arrange(country, code_plot) %>%
   left_join(coordinates_so, by = "plot_id") %>%
   as_sf




df_remaining_plots <- so_som_summ %>%
 filter(count == 1)

df_20_cm <- so_som_summ %>%
 filter(count > 1 &
           any_80_cm == FALSE &
           any_20_cm == TRUE)

df_80_cm <- so_som_summ %>%
 filter(count > 1 &
           any_80_cm == TRUE &
           any_20_cm == TRUE)

map_icpf(layers = c("df_remaining_plots",
                  "df_20_cm",
                  "df_80_cm"),
       title = "ICP Forests (solid soil) · **Level II (layer 0)**",
       legend_title =
          "**Data availability<span style='color:white'>...........</span>**",
       legend_classes = c(paste0("One survey"),
                          paste0("Repeated surveys<br>",
                                 "(0 - 20 cm)"),
                          paste0("Repeated surveys<br>",
                                 "(0 - 80 cm)")),
       export_name = "map_so_data_availability_layer0",
       export_folder = "other_graphs",
       point_size = 0.6,
       biogeo_palette = NULL,
       with_logo = FALSE,
       count_plots_legend = TRUE)






# Layer 1+ LI ----


source("./src/functions/read_processed.R")
read_processed(path_name = "./data/layer1_data/",
               save_to_env = TRUE)



s1_som_summ <- s1_som %>%
   left_join(d_depth_level_soil,
             by = join_by(code_layer == code)) %>%
   left_join(s1_strat %>%
                select(plot_id, eff_soil_depth),
             by = "plot_id") %>%
   group_by(unique_survey,
            country, code_country, code_plot, plot_id, survey_year,
            eff_soil_depth) %>%
   reframe(
      obs_max = ifelse(
         any(!is.na(layer_limit_inferior)),
         max(layer_limit_inferior, na.rm = TRUE),
         NA_real_),
      any_ff = any(layer_type == "forest_floor" &
                      is.na(layer_limit_superior) |
                      (layer_limit_superior < 0)),
      any_20_cm = any(!is.na(layer_limit_inferior) &
                         ((layer_limit_inferior >= 20) |
                             (eff_soil_depth < 20 &
                                 (layer_limit_inferior >=
                                     0.8 * eff_soil_depth)) |
                             grepl("12", code_layer))),
      any_80_cm = any(!is.na(layer_limit_inferior) &
                         ((layer_limit_inferior >= 80) |
                             (eff_soil_depth < 80 &
                                 (layer_limit_inferior >=
                                     0.8 * eff_soil_depth)) |
                             grepl("48", code_layer)))) %>%
   ungroup %>%
   arrange(country, code_plot, survey_year) %>%
   group_by(plot_id,
            country, code_country, code_plot) %>%
   reframe(any_ff = any(any_ff == TRUE),
           any_20_cm = any(any_20_cm == TRUE),
           any_80_cm = any(any_80_cm == TRUE),
           count = n(),
           year_range = max(survey_year) - min(survey_year),
           before_and_after_2000 =
              any(survey_year < 2000) & any(survey_year >= 2000)) %>%
   ungroup %>%
   arrange(country, code_plot) %>%
   left_join(coordinates_s1, by = "plot_id") %>%
   as_sf




df_remaining_plots <- s1_som_summ %>%
   filter(count == 1)

df_20_cm <- s1_som_summ %>%
   filter(count > 1 &
             any_80_cm == FALSE &
             any_20_cm == TRUE)

df_80_cm <- s1_som_summ %>%
   filter(count > 1 &
             any_80_cm == TRUE &
             any_20_cm == TRUE)

map_icpf(layers = c("df_remaining_plots",
                    "df_20_cm",
                    "df_80_cm"),
         title = "ICP Forests (solid soil) · **Level I (layer 1+)**",
         legend_title =
            "**Data availability<span style='color:white'>...........</span>**",
         legend_classes = c(paste0("One survey"),
                            paste0("Repeated surveys<br>",
                                   "(0 - 20 cm)"),
                            paste0("Repeated surveys<br>",
                                   "(0 - 80 cm)")),
         export_name = "map_s1_data_availability_layer1",
         export_folder = "other_graphs",
         point_size = 0.1,
         biogeo_palette = NULL,
         with_logo = FALSE,
         count_plots_legend = TRUE)



# Layer 1+ LII ----



so_som_summ <- so_som %>%
   left_join(d_depth_level_soil,
             by = join_by(code_layer == code)) %>%
   left_join(so_strat %>%
                select(plot_id, eff_soil_depth),
             by = "plot_id") %>%
   group_by(unique_survey,
            country, code_country, code_plot, plot_id, survey_year,
            eff_soil_depth) %>%
   reframe(
      obs_max = ifelse(
         any(!is.na(layer_limit_inferior)),
         max(layer_limit_inferior, na.rm = TRUE),
         NA_real_),
      any_ff = any(layer_type == "forest_floor" &
                      is.na(layer_limit_superior) |
                      (layer_limit_superior < 0)),
      any_20_cm = any(!is.na(layer_limit_inferior) &
                         ((layer_limit_inferior >= 20) |
                             (eff_soil_depth < 20 &
                                 (layer_limit_inferior >=
                                     0.8 * eff_soil_depth)) |
                             grepl("12", code_layer))),
      any_80_cm = any(!is.na(layer_limit_inferior) &
                         ((layer_limit_inferior >= 80) |
                             (eff_soil_depth < 80 &
                                 (layer_limit_inferior >=
                                     0.8 * eff_soil_depth)) |
                             grepl("48", code_layer)))) %>%
   ungroup %>%
   arrange(country, code_plot, survey_year) %>%
   group_by(plot_id,
            country, code_country, code_plot) %>%
   reframe(any_ff = any(any_ff == TRUE),
           any_20_cm = any(any_20_cm == TRUE),
           any_80_cm = any(any_80_cm == TRUE),
           count = n(),
           year_range = max(survey_year) - min(survey_year),
           before_and_after_2000 =
              any(survey_year < 2000) & any(survey_year >= 2000)) %>%
   ungroup %>%
   arrange(country, code_plot) %>%
   left_join(coordinates_so, by = "plot_id") %>%
   as_sf




df_remaining_plots <- so_som_summ %>%
   filter(count == 1)

df_20_cm <- so_som_summ %>%
   filter(count > 1 &
             any_80_cm == FALSE &
             any_20_cm == TRUE)

df_80_cm <- so_som_summ %>%
   filter(count > 1 &
             any_80_cm == TRUE &
             any_20_cm == TRUE)

map_icpf(layers = c("df_remaining_plots",
                    "df_20_cm",
                    "df_80_cm"),
         title = "ICP Forests (solid soil) · **Level II (layer 1+)**",
         legend_title =
            "**Data availability<span style='color:white'>...........</span>**",
         legend_classes = c(paste0("One survey"),
                            paste0("Repeated surveys<br>",
                                   "(0 - 20 cm)"),
                            paste0("Repeated surveys<br>",
                                   "(0 - 80 cm)")),
         export_name = "map_so_data_availability_layer1",
         export_folder = "other_graphs",
         point_size = 0.6,
         biogeo_palette = NULL,
         with_logo = FALSE,
         count_plots_legend = TRUE)












# Data sources ----


hor_bar_plot <- function(data_frame,
                         data_frame2 = NULL,
                         variable_cat,
                         title = NA,
                         left_title = NA,
                         right_title = NA,
                         path) {

   if (!is.null(data_frame2)) {

      plot_data <- data.frame(
         category =
            unique(c(names(summary(as.factor(data_frame[[variable_cat]]))),
                     names(summary(as.factor(data_frame2[[variable_cat]])))
                     ))) %>%
         rename(!!variable_cat := category) %>%
         left_join(
            data_frame %>%
               group_by(.data[[variable_cat]]) %>%
               reframe(count = n()) %>%
               ungroup(),
            by = variable_cat) %>%
         left_join(
            data_frame2 %>%
               group_by(.data[[variable_cat]]) %>%
               reframe(count2 = n()) %>%
               ungroup(),
            by = variable_cat) %>%
         rename(category = all_of(variable_cat))

   } else {


   # Create a dataframe from the summary
   categories <- names(summary(as.factor(data_frame[[variable_cat]])))
   counts <- as.numeric(summary(as.factor(data_frame[[variable_cat]])))

   plot_data <- data.frame(
      category = categories,
      count = counts)

   }


   # Sort the data by count in descending order
   plot_data <- plot_data[order(-plot_data$count), ]

   # Make the category a factor with levels in REVERSE order
   # This puts the highest count at the top
   plot_data$category <- factor(plot_data$category,
                                levels = rev(plot_data$category))


   # # Sort the categories based on the left plot (data_frame1)
   # sorted_categories <- plot_data1$category[order(-plot_data1$count)]
   #
   # # Apply the same factor levels to both dataframes
   # plot_data1$category <-
   #    factor(plot_data1$category, levels = rev(sorted_categories))
   # plot_data2$category <-
   #    factor(plot_data2$category, levels = rev(sorted_categories))

   # Plot 1
   p1 <- ggplot(plot_data,
                aes(x = count, y = category)) +
      geom_bar(stat = "identity",
               fill = "#c04384",
               width = 0.7) +
      geom_text(aes(label = count),
                hjust = -0.3,
                color = "#c04384",
                size = 3.5) +
      theme_minimal() +
      theme(
         text = element_text(size = 10, color = "black"),
         axis.title = element_blank(),
         axis.text.y = element_markdown(size = 10,
                                        color = "black",
                                        margin = margin(r = 10)),
         axis.text.x = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.4)))


   if (!is.null(data_frame2)) {



   # Plot 2
   p2 <- ggplot(plot_data,
                aes(x = count2, y = category)) +
      geom_bar(stat = "identity",
               fill = "#c04384",
               width = 0.7) +
      geom_text(aes(label = count2),
                hjust = -0.3,
                color = "#c04384",
                size = 3.5) +
      theme_minimal() +
      theme(
         text = element_text(size = 10,
                             color = "black"),
         axis.title = element_blank(),
         axis.text.y = element_blank(), # Hide y-axis labels on the right plot
         axis.text.x = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.4)))




   # Assuming your two plots are called p1 and p2
   p1 <- p1 + ggtitle(left_title) +
      theme(plot.title = element_markdown(hjust = 0,
                                          size = 10))
   p2 <- p2 + ggtitle(right_title) +
      theme(plot.title = element_markdown(hjust = 0,
                                          size = 10))

   # Combine with patchwork
   p <- (p1 | p2) +
      plot_annotation(
         title = title,
         theme = theme(
            plot.title = element_markdown(hjust = 0, size = 10)))


   } else {

      p <- p1

   }


   # Save as .png
   suppressWarnings(
      ggsave(path,
             # Set the resolution of the saved plot (dots per inch)
             # Recommendation: at least 500 dpi
             plot = p,
             dpi = 500,
             # Set the width and height of the saved plot in inches
             width = 6.81)
   )


}




## eff_soil_depth ----

s1_strat_summ <- s1_strat %>%
   mutate(depth_stock =
             ifelse(is.na(.data$eff_soil_depth) |
                       (.data$eff_soil_depth > 100),
                    100,
                    .data$eff_soil_depth)) %>%
   rowwise() %>%
   mutate(
      # depth_stock_source:
      # - "Known to be >= 100 cm (depth of 'pfh')"
      # - "Depth of profile ('pfh')"
      # - "Manually harmonised"
      # - "Default based on WRB soil classification"
      # - "Max. of rooting, rock and obstacle depth"
      # - "Original eff_soil_depth"
      # - "Top of parent material horizons"
      # - "Top of stony zone"
      # - "Default (i.e. 100 cm)"
      depth_stock_source = case_when(
         any(!is.na(depth_stock) & depth_stock == 100 &
                is.na(eff_soil_depth)) ~
            "Default (i.e. 100 cm)",
         any(grepl("no info", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Default (i.e. 100 cm)",
         any(grepl("deep", eff_soil_depth_source, ignore.case = TRUE) &
                grepl("'pfh'", eff_soil_depth_source)) ~
            "Known to be ≥ 100 cm (depth of 'pfh')",
         any(eff_soil_depth_source == "Bottom of profile ('pfh')") ~
            "Depth of profile ('pfh')",
         any(grepl("\\bWRB\\b", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Default based on WRB soil classification",
         any(grepl("obstacle depth", eff_soil_depth_source,
                   ignore.case = TRUE)) ~
            "Max. of rooting, rock and obstacle depth",
         any(grepl("Original", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Original eff_soil_depth",
         any(grepl("parent material", eff_soil_depth_source,
                   ignore.case = TRUE)) ~
            "Top of parent material horizons",
         any(grepl("stony zone", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Top of stony zone",
         any(grepl("Harmonised", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Manually harmonised",
         TRUE ~ NA_character_)) %>%
   ungroup()



so_strat_summ <- so_strat %>%
   mutate(depth_stock =
             ifelse(is.na(.data$eff_soil_depth) |
                       (.data$eff_soil_depth > 100),
                    100,
                    .data$eff_soil_depth)) %>%
   rowwise() %>%
   mutate(
      # depth_stock_source:
      # - "Known to be >= 100 cm (depth of 'pfh')"
      # - "Depth of profile ('pfh')"
      # - "Manually harmonised"
      # - "Default based on WRB soil classification"
      # - "Max. of rooting, rock and obstacle depth"
      # - "Original eff_soil_depth"
      # - "Top of parent material horizons"
      # - "Top of stony zone"
      # - "Default (i.e. 100 cm)"
      depth_stock_source = case_when(
         any(!is.na(depth_stock) & depth_stock == 100 &
                is.na(eff_soil_depth)) ~
            "Default (i.e. 100 cm)",
         any(grepl("no info", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Default (i.e. 100 cm)",
         any(grepl("deep", eff_soil_depth_source, ignore.case = TRUE) &
                grepl("'pfh'", eff_soil_depth_source)) ~
            "Known to be ≥ 100 cm (depth of 'pfh')",
         any(eff_soil_depth_source == "Bottom of profile ('pfh')") ~
            "Depth of profile ('pfh')",
         any(grepl("\\bWRB\\b", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Default based on WRB soil classification",
         any(grepl("obstacle depth", eff_soil_depth_source,
                   ignore.case = TRUE)) ~
            "Max. of rooting, rock and obstacle depth",
         any(grepl("Original", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Original eff_soil_depth",
         any(grepl("parent material", eff_soil_depth_source,
                   ignore.case = TRUE)) ~
            "Top of parent material horizons",
         any(grepl("stony zone", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Top of stony zone",
         any(grepl("Harmonised", eff_soil_depth_source, ignore.case = TRUE)) ~
            "Manually harmonised",
         TRUE ~ NA_character_)) %>%
   ungroup()



hor_bar_plot(
   data_frame = s1_strat_summ,
   data_frame2 = so_strat_summ,
   variable_cat = "depth_stock_source",
   title = paste0("Source of ",
                  "**<span style='color:#c04384'>soil depth</span>**",
                  " for stocks (max. 100 cm) (plots)"),
   left_title = "**Level I**",
   right_title = "**Level II**",
   path = "./output/other_graphs/eff_soil_depth_source.png")




hor_bar_plot(
   data_frame = s1_strat_summ,
   variable_cat = "depth_stock_source",
   title = paste0("**<span style='color:#c04384'>Level I</span>**<br>",
                  "Source of **soil depth** for stocks (max. 100 cm) (plots)"),
   path = "./output/other_graphs/eff_soil_depth_source_s1.png")


## bulk_density ----


s1_summ <- s1_som %>%
   filter(!is.na(layer_number)) %>%
   rowwise() %>%
   mutate(
      # bulk_density_source:
      # - "Old data source"
      # - "'pfh' (measured)"
      # - "'pfh' (estimated)"
      # - "'som'"
      # - "'sw_swc'"
      # - "PTF"
      bulk_density_source2 = case_when(
         any(grepl("som", bulk_density_source) &
                layer_type != "forest_floor") ~
            paste0("'som'"),
         any(grepl("swc", bulk_density_source) &
                layer_type != "forest_floor") ~
            "'sw_swc'",
         any(grepl("pfh_measure", bulk_density_source)
             & layer_type != "forest_floor") ~
            paste0("'pfh' (measured)"),
         any(grepl("pfh_est", bulk_density_source) &
                layer_type != "forest_floor") ~
            paste0("'pfh' (estimated)"),
         any(grepl("fscdb", bulk_density_source, ignore.case = TRUE) &
                layer_type != "forest_floor") ~
            "External data source",
         any(grepl("PTF", bulk_density_source, ignore.case = TRUE) &
                layer_type != "forest_floor") ~
            "Pedotransfer function",
         TRUE ~ "(NA)"),
      # organic_layer_weight_source:
      # - "'som'"
      # - "Old data source"
      # - "Bulk density and layer thickness"
      organic_layer_weight_source2 = case_when(
         any(grepl("^som", organic_layer_weight_source) &
                !grepl("bulk_density", organic_layer_weight_source) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            paste0("'som'"),
         any(grepl("bulk_density", organic_layer_weight_source) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            "Bulk density & layer thickness",
         any(grepl("fscdb", organic_layer_weight_source, ignore.case = TRUE) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            "External data source",
         TRUE ~ "(NA)"),
      # coarse_fragment_vol_source:
      # - "Old data source"
      # - "'pfh' (code_horizon_coarse_vol)"
      # - "'pfh' (horizon_coarse_weight)"
      # - "'som'"
      # - "Default (i.e. 0 vol%)"
      coarse_fragment_vol_source2 = case_when(
         any(grepl("som", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0("'som'"),
         any(grepl("pfh_weight", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0("'pfh' (horizon_coarse_weight)"),
         any(grepl("pfh_code_vol", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0("'pfh' (code_horizon_coarse_vol)"),
         any(grepl("fscdb", coarse_fragment_vol_source, ignore.case = TRUE) &
                layer_type == "mineral") ~
            "External data source",
         TRUE ~ "Default (i.e. 0 vol%)"),
      # TOC:
      # - "Old data source"
      # - "'pfh' (same period)"
      # - "'som'"
      # - "PIR"
      organic_carbon_total_source2 = case_when(
         any(grepl("som", organic_carbon_total_source)) ~
            paste0("'som'"),
         any(grepl("fscdb", organic_carbon_total_source,
                   ignore.case = TRUE)) ~
            "External data source",
         any(grepl("pfh", organic_carbon_total_source)) ~
            paste0("'pfh' (same period)"),
         any(organic_carbon_total_source == "PIR") ~ "PIR",
         any(grepl("manual correction", organic_carbon_total_source)) ~
            "Manual correction (FSCC)",
         TRUE ~ "(NA)"),
      # TN:
      n_total_source2 = case_when(
         any(grepl("som", n_total_source)) ~
            paste0("'som'"),
         any(grepl("fscdb", n_total_source,
                   ignore.case = TRUE)) ~
            "External data source",
         any(grepl("pfh", n_total_source)) ~
            paste0("'pfh' (same period)"),
         any(n_total_source == "PIR") ~ "PIR",
         TRUE ~ "(NA)"),
      # texture:
      # - "Old data source"
      # - "som"
      # - "pfh"
      # - "texture class"
      texture_source2 = case_when(
         any(grepl("som", part_size_clay_source)) ~
            paste0("'som'"),
         any(grepl("fscdb", part_size_clay_source,
                   ignore.case = TRUE)) ~
            "External data source",
         any(grepl("pfh", part_size_clay_source)) ~
            paste0("'pfh'"),
         any(part_size_clay_source == "PIR") ~ "PIR",
         any(is.na(part_size_clay_source) &
                !is.na(code_texture_class)) ~
            "Texture class",
         TRUE ~ "(NA)")) %>%
   ungroup()






so_summ <- so_som %>%
   filter(!is.na(layer_number)) %>%
   rowwise() %>%
   mutate(
      # bulk_density_source:
      # - "Old data source"
      # - "'pfh' (measured)"
      # - "'pfh' (estimated)"
      # - "'som'"
      # - "'sw_swc'"
      # - "PTF"
      bulk_density_source2 = case_when(
         any(grepl("som", bulk_density_source) &
                layer_type != "forest_floor") ~
            paste0("'som'"),
         any(grepl("swc", bulk_density_source) &
                layer_type != "forest_floor") ~
            "'sw_swc'",
         any(grepl("pfh_measure", bulk_density_source)
             & layer_type != "forest_floor") ~
            paste0("'pfh' (measured)"),
         any(grepl("pfh_est", bulk_density_source) &
                layer_type != "forest_floor") ~
            paste0("'pfh' (estimated)"),
         any(grepl("fscdb", bulk_density_source, ignore.case = TRUE) &
                layer_type != "forest_floor") ~
            "External data source",
         any(grepl("PTF", bulk_density_source, ignore.case = TRUE) &
                layer_type != "forest_floor") ~
            "Pedotransfer function",
         TRUE ~ "(NA)"),
      # organic_layer_weight_source:
      # - "'som'"
      # - "Old data source"
      # - "Bulk density and layer thickness"
      organic_layer_weight_source2 = case_when(
         any(grepl("^som", organic_layer_weight_source) &
                !grepl("bulk_density", organic_layer_weight_source) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            paste0("'som'"),
         any(grepl("bulk_density", organic_layer_weight_source) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            "Bulk density & layer thickness",
         any(grepl("fscdb", organic_layer_weight_source, ignore.case = TRUE) &
                layer_type != "mineral" &
                !grepl("L", code_layer)) ~
            "External data source",
         TRUE ~ "(NA)"),
      # coarse_fragment_vol_source:
      # - "Old data source"
      # - "'pfh' (code_horizon_coarse_vol)"
      # - "'pfh' (horizon_coarse_weight)"
      # - "'som'"
      # - "Default (i.e. 0 vol%)"
      coarse_fragment_vol_source2 = case_when(
         any(grepl("som", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0("'som'"),
         any(grepl("pfh_weight", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0("'pfh' (horizon_coarse_weight)"),
         any(grepl("pfh_code_vol", coarse_fragment_vol_source) &
                layer_type == "mineral") ~
            paste0("'pfh' (code_horizon_coarse_vol)"),
         any(grepl("fscdb", coarse_fragment_vol_source, ignore.case = TRUE) &
                layer_type == "mineral") ~
            "External data source",
         TRUE ~ "Default (i.e. 0 vol%)"),
      # parameter_for_stock_source:
      # - "Old data source"
      # - "'pfh' (same period)"
      # - "'som'"
      # - "PIR"
      organic_carbon_total_source2 = case_when(
         any(grepl("som", organic_carbon_total_source)) ~
            paste0("'som'"),
         any(grepl("fscdb", organic_carbon_total_source,
                   ignore.case = TRUE)) ~
            "External data source",
         any(grepl("pfh", organic_carbon_total_source)) ~
            paste0("'pfh' (same period)"),
         any(organic_carbon_total_source == "PIR") ~ "PIR",
         any(grepl("manual correction", organic_carbon_total_source)) ~
            "Manual correction (FSCC)",
         TRUE ~ "(NA)"),
      # TN:
      n_total_source2 = case_when(
         any(grepl("som", n_total_source)) ~
            paste0("'som'"),
         any(grepl("fscdb", n_total_source,
                   ignore.case = TRUE)) ~
            "External data source",
         any(grepl("pfh", n_total_source)) ~
            paste0("'pfh' (same period)"),
         any(n_total_source == "PIR") ~ "PIR",
         TRUE ~ "(NA)"),
      # texture:
      # - "Old data source"
      # - "som"
      # - "pfh"
      # - "texture class"
      texture_source2 = case_when(
         any(grepl("som", part_size_clay_source)) ~
            paste0("'som'"),
         any(grepl("fscdb", part_size_clay_source,
                   ignore.case = TRUE)) ~
            "External data source",
         any(grepl("pfh", part_size_clay_source)) ~
            paste0("'pfh'"),
         any(part_size_clay_source == "PIR") ~ "PIR",
         any(is.na(part_size_clay_source) &
                !is.na(code_texture_class)) ~
            "Texture class",
         TRUE ~ "(NA)")) %>%
   ungroup()







hor_bar_plot(
   data_frame = s1_summ %>%
      filter(layer_type == "mineral"),
   data_frame2 = so_summ %>%
      filter(layer_type == "mineral"),
   variable_cat = "bulk_density_source2",
   title = paste0("Source of ",
                  "**<span style='color:#c04384'>bulk density</span>**",
                  " (mineral layers)"),
   left_title = "**Level I**",
   right_title = "**Level II**",
   path = "./output/other_graphs/bulk_density_source.png")



## organic_layer_weight_source ----

hor_bar_plot(
   data_frame = s1_summ %>%
      filter(!grepl("L", code_layer)) %>%
      filter(layer_type == "forest_floor"),
   data_frame2 = so_summ %>%
      filter(!grepl("L", code_layer)) %>%
      filter(layer_type == "forest_floor"),
   variable_cat = "organic_layer_weight_source2",
   title = paste0("Source of ",
                  "**<span style='color:#c04384'>organic layer weight</span>**",
                  " (non-OL forest floor layers)"),
   left_title = "**Level I**",
   right_title = "**Level II**",
   path = "./output/other_graphs/organic_layer_weight_source.png")



## coarse_fragment_vol_source ----


hor_bar_plot(
   data_frame = s1_summ %>%
      filter(layer_type == "mineral"),
   data_frame2 = so_summ %>%
      filter(layer_type == "mineral"),
   variable_cat = "coarse_fragment_vol_source2",
   title = paste0("Source of ",
                  "**<span style='color:#c04384'>",
                  "vol% coarse fragments</span>**",
                  " for stocks (mineral layers)"),
   left_title = "**Level I**",
   right_title = "**Level II**",
   path = "./output/other_graphs/coarse_fragment_vol_source.png")



## organic_carbon_total_source ----

hor_bar_plot(
   data_frame = s1_summ,
   data_frame2 = so_summ,
   variable_cat = "organic_carbon_total_source2",
   title = paste0("Source of ",
                  "**<span style='color:#c04384'>",
                  "total organic carbon</span>**",
                  " (layers)"),
   left_title = "**Level I**",
   right_title = "**Level II**",
   path = "./output/other_graphs/organic_carbon_total_source.png")



## n_total_source ----

hor_bar_plot(
   data_frame = s1_summ,
   data_frame2 = so_summ,
   variable_cat = "n_total_source2",
   title = paste0("Source of ",
                  "**<span style='color:#c04384'>",
                  "total nitrogen</span>**",
                  " (layers)"),
   left_title = "**Level I**",
   right_title = "**Level II**",
   path = "./output/other_graphs/n_total_source.png")




## texture ----


hor_bar_plot(
   data_frame = s1_summ %>%
      filter(layer_type == "mineral"),
   data_frame2 = so_summ %>%
      filter(layer_type == "mineral"),
   variable_cat = "texture_source2",
   title = paste0("Source of ",
                  "**<span style='color:#c04384'>",
                  "soil texture</span>**",
                  " (mineral layers)"),
   left_title = "**Level I**",
   right_title = "**Level II**",
   path = "./output/other_graphs/texture_source.png")













# C stock potential ----

s1_c_stock_potential_summary <-
   read.csv(paste0("./output/stocks/20241024_20241023_c_stocks/",
                   "s1_c_stock_potential_summary.csv"),
            sep = ";")

s1_pot_summ <- s1_c_stock_potential_summary %>%
   filter(reason_excluded != "No records in 'som'") %>%
   filter(reason_excluded != "Continuous bedrock at 0 cm") %>%
   rowwise() %>%
   mutate(
      potential = case_when(
         plaus_stock_calculated == TRUE ~ "Plausible SOC stock calculated",
         grepl("density should be below", reason_excluded) ~
            "Implausibly high C density",
         grepl("Stock should be between", reason_excluded) ~
            "Implausibly low or high C stock",
         grepl("Max. below-ground total organic carbon should be",
               reason_excluded) ~
            "Implausibly low below-ground C concentration",
         grepl("Annual sequestration rate should be ", reason_excluded) ~
            "Implausibly high annual change rate",
         grepl("Rel. annual sequestration rate should be ", reason_excluded) ~
            "Implausibly high rel. annual change rate",
         grepl("Observation depth should be", reason_excluded) ~
            "Topsoil stock only (observation depth too shallow)",
         grepl("Plots should have non-OL forest floor layers",
               reason_excluded) |
            grepl("Non-OL layers should have a known organic layer weight",
                  reason_excluded) ~
            "Non-mull forest floor C stock cannot be calculated",
         grepl("Organic layer weight should be below", reason_excluded) ~
            "Implausibly high organic layer weight",
         grepl("Total organic carbon of forest floor should be",
               reason_excluded) |
            grepl("Change rate of mean forest floor total organic carbon",
                  reason_excluded) ~
            "Implausible forest floor C concentration",

         grepl("Insufficient below-ground layers",
               reason_excluded) ~
            "Insufficient below-ground layers with C density",
         grepl("Insufficient data to calculate density in upper below-ground",
               reason_excluded) ~
            "No C density for upper below-ground layer",
         grepl("No below-ground layers reported in",
               reason_excluded) ~
            "No below-ground layers",
         grepl("No relevant total organic carbon data",
               reason_excluded) ~
            "No C concentration data",
         TRUE ~ "(Other)"
      )) %>%
   ungroup()







so_c_stock_potential_summary <-
   read.csv(paste0("./output/stocks/20250115_20250114_c_stocks/",
                   "so_c_stock_potential_summary.csv"),
            sep = ";")


so_pot_summ <- so_c_stock_potential_summary %>%
   filter(reason_excluded != "No records in 'som'") %>%
   filter(reason_excluded != "Continuous bedrock at 0 cm") %>%
   filter(reason_excluded != "Considered implausible by partner") %>%
   rowwise() %>%
   mutate(
      potential = case_when(
         plaus_stock_calculated == TRUE ~ "Plausible SOC stock calculated",
         grepl("density should be below", reason_excluded) ~
            "Implausibly high C density",
         grepl("Stock should be between", reason_excluded) ~
            "Implausibly low or high C stock",
         grepl("Max. below-ground total organic carbon should be",
               reason_excluded) ~
            "Implausibly low below-ground C concentration",
         grepl("Annual sequestration rate should be ", reason_excluded) ~
            "Implausibly high annual change rate",
         grepl("Rel. annual sequestration rate should be ", reason_excluded) ~
            "Implausibly high rel. annual change rate",
         grepl("Observation depth should be", reason_excluded) ~
            "Topsoil stock only (observation depth too shallow)",
         grepl("Plots should have non-OL forest floor layers",
               reason_excluded) |
            grepl("Non-OL layers should have a known organic layer weight",
                  reason_excluded) ~
            "Non-mull forest floor C stock cannot be calculated",
         grepl("Organic layer weight should be below", reason_excluded) ~
            "Implausibly high organic layer weight",
         grepl("Total organic carbon of forest floor should be",
               reason_excluded) |
            grepl("Change rate of mean forest floor total organic carbon",
                  reason_excluded) ~
            "Implausible forest floor C concentration",

         grepl("Insufficient below-ground layers",
               reason_excluded) ~
            "Insufficient below-ground layers with C density",
         grepl("Insufficient data to calculate density in upper below-ground",
               reason_excluded) ~
            "No C density for upper below-ground layer",
         grepl("No below-ground layers reported in",
               reason_excluded) ~
            "No below-ground layers",
         grepl("No relevant 'organic_carbon_total' data",
               reason_excluded) ~
            "No C concentration data",
         TRUE ~ "(Other)"
      )) %>%
   ungroup()




hor_bar_plot(
   data_frame = s1_pot_summ,
   data_frame2 = so_pot_summ,
   variable_cat = "potential",
   title = paste0("Potential for plausible ",
                  "**<span style='color:#c04384'>",
                  "SOC stock</span>**",
                  " calculation (plot x year)"),
   left_title = "**Level I**",
   right_title = "**Level II**",
   path = "./output/other_graphs/c_stock_potential.png")





## map LI ----

s1_pot_summ_plot <- s1_pot_summ %>%
   group_by(plot_id) %>%
   reframe(
      count_plaus_stock =
         sum(plaus_stock_calculated == TRUE, na.rm = TRUE),
      count_plaus_stock_topsoil =
         sum(grepl("Incomplete", plaus_stock_coverage),
             na.rm = TRUE)) %>%
   left_join(coordinates_s1, by = "plot_id") %>%
   as_sf

df_repeated <- s1_pot_summ_plot %>%
   filter(count_plaus_stock > 1)

df_once <- s1_pot_summ_plot %>%
   filter(count_plaus_stock == 1)

df_topsoil <- s1_pot_summ_plot %>%
   filter(count_plaus_stock_topsoil >= 1)

df_remaining <- s1_pot_summ_plot %>%
   filter(count_plaus_stock == 0 &
             count_plaus_stock_topsoil == 0)

map_icpf(layers = c("df_remaining",
                    "df_topsoil",
                    "df_once",
                    "df_repeated"),
         title = paste0("Potential for SOC stock calculation · **Level I**"),
         legend_title = paste0("**Plausible SOC stock(s)?",
                               "<span style='color:white'>",
                               "...........</span>**"),
         legend_classes = c(paste0("None"),
                            paste0("Topsoil only"),
                            paste0("One survey"),
                            paste0("Repeated surveys")),
         export_name = "map_s1_c_stock_potential",
         export_folder = "other_graphs",
         point_size = 0.1,
         biogeo_palette = NULL,
         with_logo = FALSE,
         count_plots_legend = TRUE,
         inset_maps_offset_x = -0.7)



## map LII ----

so_pot_summ_plot <- so_pot_summ %>%
   group_by(plot_id) %>%
   reframe(
      count_plaus_stock =
         sum(plaus_stock_calculated == TRUE, na.rm = TRUE),
      count_plaus_stock_topsoil =
         sum(grepl("Incomplete", plaus_stock_coverage),
             na.rm = TRUE)) %>%
   left_join(coordinates_so, by = "plot_id") %>%
   as_sf

df_repeated <- so_pot_summ_plot %>%
   filter(count_plaus_stock > 1)

df_once <- so_pot_summ_plot %>%
   filter(count_plaus_stock == 1)

df_topsoil <- so_pot_summ_plot %>%
   filter(count_plaus_stock_topsoil >= 1)

df_remaining <- so_pot_summ_plot %>%
   filter(count_plaus_stock == 0 &
             count_plaus_stock_topsoil == 0)

map_icpf(layers = c("df_remaining",
                    "df_topsoil",
                    "df_once",
                    "df_repeated"),
         title = paste0("Potential for SOC stock calculation · **Level II**"),
         legend_title = paste0("**Plausible SOC stock(s)?",
                               "<span style='color:white'>",
                               "...........</span>**"),
         legend_classes = c(paste0("None"),
                            paste0("Topsoil only"),
                            paste0("One survey"),
                            paste0("Repeated surveys")),
         export_name = "map_so_c_stock_potential",
         export_folder = "other_graphs",
         point_size = 0.6,
         biogeo_palette = NULL,
         with_logo = FALSE,
         count_plots_legend = TRUE,
         inset_maps_offset_x = -0.7)





