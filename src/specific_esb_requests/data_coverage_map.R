

# Maps showing data coverage of ICP Forests

# Script initiation date: 20240306

source("./src/functions/map_icpf.R")

# LI ----

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















