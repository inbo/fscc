

# Maps showing data coverage of ICP Forests

# Script initiation date: 20240306

source("./src/functions/map_icpf.R")

# LI ----

s1_som_summ <- s1_som %>%
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
          title = "ICP Forests (solid soil) · **Level I**",
          legend_title =
            "**Data availability<span style='color:white'>...........</span>**",
          legend_classes = c(paste0("One survey"),
                             paste0("Repeated surveys\n",
                                    "(0 - 20 cm)"),
                             paste0("Repeated surveys\n",
                                    "(0 - 80 cm)")),
          export_name = "map_s1_data_availability",
          export_folder = "other_graphs",
          point_size = 0.1,
          biogeo_palette = NULL)




 # LII ----

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
                             paste0("Repeated surveys\n",
                                    "(0 - 20 cm)"),
                             paste0("Repeated surveys\n",
                                    "(0 - 80 cm)")),
          export_name = "map_so_data_availability",
          export_folder = "other_graphs",
          point_size = 0.6,
          biogeo_palette = NULL)




