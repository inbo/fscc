

# Calculate nutrient availability metric
# --------------------------------------

# Question: calculate nutrient availability metrics, which can be used for
# integrated studies with other ICP Forests surveys. E.g. using the approach
# by Van Mechelen et al. ()

# (At least for Level II)

# Script initiation: January 2025

# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O


stopifnot(require("ggpmisc",
                  "viridis"))




# Input plot versus plot survey ----

agg_level <- "plot"
agg_level <- "plot_survey"

# Input level ----

level <- "LI"
level <- "LII"

code_survey <- case_when(
  level == "LI" ~ "s1",
  level == "LII" ~ "so")









# Import data ----

## Soil data aggregated into one value per plot + stratifiers ----

path <- "./output/nutrient_avail/"

files <- list.files(path)[
  which(grepl(paste0(agg_level, "_agg_data.csv"), list.files(path)) &
          grepl(paste0(code_survey, "_"), list.files(path)))]

dates <- as.Date(sapply(strsplit(files, "_"), `[`, 1),
                 format = "%Y%m%d")

df_agg <- read.csv(paste0(path, files[which.max(dates)]),
                            sep = ";") %>%
  as_tibble



# P avail ----

# Boundaries extrac_p

extrac_p_quant <- round(
  quantile(na.omit(df_agg$extrac_p_conc_forest_floor_proxy),
           c(0.1, 0.3, 0.7, 0.9)))

extrac_p_quant <- c(400, 600, 800, 1200)


p <- df_agg %>%
  filter(!is.na(c_to_p_ratio_forest_floor) &
           c_to_p_ratio_forest_floor <= 1000) %>%
  ggplot(aes(x = c_to_p_ratio_forest_floor,
             y = c_to_p_ratio_forest_floor_proxy)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) + # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red") + # Regression line
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "~~~")),
               formula = y ~ x, parse = TRUE) + # Equation and R²
  # stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
  #              formula = y ~ x, parse = TRUE) + # Equation and R²
  theme_minimal() +
  theme(
    axis.title.y = element_markdown(angle = 0,
                                    margin = margin(r = 5),
                                    lineheight = 1.2,
                                    hjust = 0,
                                    vjust = 1),
    axis.title.x = element_markdown(hjust = 1,
                                    margin = margin(t = 5)),
    axis.text = element_markdown(colour = "black",
                                 size = 10)) +
  labs(
    x = "C:P ratio forest floor",
    y = "C:P ratio<br>forest floor<br>(proxy)")

ggsave(filename = paste0(code_survey, "_c_to_p_forest_floor_lin_reg", ".png"),
       plot = p,
       path = paste0("./output/nutrient_avail/"),
       dpi = 500,
       width = 6.81)

c_to_p_boundaries <- round(c(200, 400, 600, 800) * 0.916 - 89.8)



df_p_avail <- df_agg %>%
  mutate(
    # Rating value CLIMATE ZONE
    rv_climate = case_when(
      # 1: "Boreal, Mountaineous (North)"
      (biogeographical_region == "Boreal" &
         latitude_dec > 60) |
        (biogeographical_region == "Alpine" &
           latitude_dec > 50) ~ 1,
      # 2: "Temperate Boreal, Mountaineous (South)"
      (biogeographical_region == "Boreal" &
         latitude_dec <= 60) |
        (biogeographical_region == "Alpine" &
           latitude_dec <= 50) ~ 2,
      # 3: "North Atlantic, Subatlantic, Continental"
      (biogeographical_region == "Atlantic" &
         latitude_dec > 45) |
        biogeographical_region %in% c("Continental", "Pannonian") ~ 3,
      # 4: "Meditteranean"
      # Atlantic < 45
      # Black Sea
      # Macaronesia
      # Meditteranean
      (biogeographical_region == "Atlantic" &
         latitude_dec >= 45) |
        biogeographical_region %in% c("Black Sea", "Macaronesia",
                                      "Mediterranean") ~ 4
      # 5: "South Atlantic"
    ),
    # Class value of phosphorus stock (instead of concentration!)
    rv_extrac_p = case_when(
      extrac_p_conc_forest_floor_proxy < extrac_p_quant[1] ~ 1,
      extrac_p_conc_forest_floor_proxy < extrac_p_quant[2] ~ 2,
      extrac_p_conc_forest_floor_proxy < extrac_p_quant[3] ~ 3,
      extrac_p_conc_forest_floor_proxy < extrac_p_quant[4] ~ 4,
      !is.na(extrac_p_conc_forest_floor_proxy) ~ 5),
    # Rating value of C:P ratio (m/m) in organic layer
    rv_c_to_p = case_when(
      c_to_p_ratio_forest_floor_proxy == -1 ~ NA,
      c_to_p_ratio_forest_floor_proxy <= c_to_p_boundaries[1] ~ 5,
      c_to_p_ratio_forest_floor_proxy <= c_to_p_boundaries[2] ~ 4,
      c_to_p_ratio_forest_floor_proxy <= c_to_p_boundaries[3] ~ 3,
      c_to_p_ratio_forest_floor_proxy <= c_to_p_boundaries[4] ~ 2,
      !is.na(c_to_p_ratio_forest_floor_proxy) ~ 1),
    # Rating value for pH-CaCl2
    rv_ph = case_when(
      ph_cacl2 > 7 ~ 1,
      ph_cacl2 > 6 ~ 2,
      ph_cacl2 > 5 ~ 3,
      ph_cacl2 > 4 ~ 2,
      ph_cacl2 > 3 ~ 1.5,
      !is.na(ph_cacl2) ~ 1
    ),
    # Cumulative class value for organic C in mineral layers
    rv_org_c = case_when(
      c_conc_below_ground_topsoil <= 10 ~ 1 * 2 - 1,
      c_conc_below_ground_topsoil <= 20 ~ 2 * 2 - 1,
      c_conc_below_ground_topsoil <= 40 ~ 3 * 2 - 1,
      c_conc_below_ground_topsoil <= 80 ~ 4 * 2 - 1,
      !is.na(c_conc_below_ground_topsoil) ~ 5 * 2 - 1)) %>%
  # P availability metric
  mutate(
    p_avail = ifelse(
      !is.na(rv_climate) &
        !is.na(rv_extrac_p) &
        !is.na(rv_c_to_p) &
        !is.na(rv_ph) &
        !is.na(rv_org_c),
      rv_extrac_p * rv_ph + rv_c_to_p + rv_climate + rv_org_c,
      NA_real_)) %>%
  filter(!is.na(p_avail)) %>%
  mutate(
    p_avail_class_num = case_when(
      p_avail <= 8.2 ~ 1,
      p_avail <= 10.8 ~ 2,
      p_avail <= 14.6 ~ 3,
      p_avail <= 18.8 ~ 4,
      TRUE ~ 5),
    p_avail_class_cat = case_when(
      p_avail <= 8.2 ~ "Very low",
      p_avail <= 10.8 ~ "Low",
      p_avail <= 14.6 ~ "Medium",
      p_avail <= 18.8 ~ "High",
      TRUE ~ "Very high")) %>%
  arrange(desc(p_avail))

hist(df_p_avail$p_avail_class_num)

df_p_avail_sf <- df_p_avail %>%
  group_by(plot_id) %>%
  slice_max(order_by = survey_year) %>%
  ungroup() %>%
  arrange(desc(p_avail)) %>%
  as_sf

map_icpf(layers = "df_p_avail_sf",
         title = paste0("**P availability** metric · ",
                        "ICP Forests (Level II)",
                        "<br>",
                        n_distinct(df_p_avail$plot_id),
                        " plots (most recent survey)",
                        "<br>_",
                        "Metric according to Vanmechelen et al. (1997)_"),
         legend_title = "**P availability**",
         legend_classes = TRUE,
         variable_cat = "p_avail_class_cat",
         export_name = paste0(code_survey, "_p_avail"),
         export_folder = "nutrient_avail",
         point_col = NULL,
         point_size = 0.5,
         biogeo_palette = NULL,
         count_plots_legend = TRUE,
         inset_maps_offset_x = 1.25)


p <- df_p_avail %>%
  # Count the number of occurrences for each year/class combination
  count(survey_year,
        p_avail_class_num,
        p_avail_class_cat) %>%
  # `n` is the count of each combination
  ggplot(aes(x = survey_year,
             y = factor(p_avail_class_num),
             fill = n)) +
  geom_tile() +  # Create a heatmap-like plot
  scale_fill_viridis(option = "magma",
                     direction = 1,
                     name = "Abundance",
                     trans = scales::pseudo_log_trans(sigma = 5)) +
  scale_y_discrete(
    labels = setNames(df_p_avail$p_avail_class_cat,
                      df_p_avail$p_avail_class_num)) +
  labs(
    x = "Survey year",
    title = paste0("**P availability** metric · ICP Forests (Level II)",
                   "<br>_Metric according to Vanmechelen et al. (1997)_")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black"),
    plot.title = element_markdown(hjust = 0,
                                  colour = "black",
                                  lineheight = 1.2,
                                  size = 10,
                                  margin = margin(b = 5)),
    axis.title.y = element_blank(),
    axis.title.x = element_markdown(hjust = 1,
                                    margin = margin(t = 5)),
    axis.text.x = element_text(hjust = 1,
                               size = 10,
                               colour = "black",
                               margin = margin(t = 5)),
    axis.text.y = element_text(size = 10,
                               margin = margin(r = 5),
                               colour = "black"),
    legend.title = element_markdown(size = 10,
                                    margin = margin(b = 10)),
    legend.text = element_text(size = 10),
    panel.grid = element_blank())


ggsave(paste0("./output/nutrient_avail/", code_survey,
              "_p_avail_over_time.png"),
       # Set the resolution of the saved plot (dots per inch)
       # Recommendation: at least 500 dpi
       plot = p,
       dpi = 500,
       # Set the width and height of the saved plot in inches
       width = 6.81)









# N avail ----

df_n_avail <- df_agg %>%
  mutate(
    c_stock_forest_floor = coalesce(c_stock_forest_floor, 0),
    n_stock_forest_floor = coalesce(n_stock_forest_floor, 0),
    c_to_n_ratio_topsoil =
      ifelse(!is.na(c_stock_below_ground_topsoil) &
               !is.na(n_stock_below_ground_topsoil),
             round((c_stock_forest_floor + c_stock_below_ground_topsoil) /
                     (n_stock_forest_floor + n_stock_below_ground_topsoil),
                   2),
             NA_real_),
    # c_to_n_ratio_topsoil = ifelse(
    #   !is.na(c_to_n_ratio_forest_floor_proxy) &
    #     !is.na(c_to_n_ratio_below_ground_topsoil),
    #   1/3 * c_to_n_ratio_forest_floor_proxy +
    #     2/3 * c_to_n_ratio_below_ground_topsoil,
    #   NA_real_)
    ) %>%
  mutate(
    # Rating value CLIMATE ZONE
    rv_climate = case_when(
      # 1: "Boreal, Mountaineous (North)"
      (biogeographical_region == "Boreal" &
         latitude_dec > 60) |
        (biogeographical_region == "Alpine" &
           latitude_dec > 50) ~ 1,
      # 2: "Temperate Boreal, Mountaineous (South)"
      (biogeographical_region == "Boreal" &
         latitude_dec <= 60) |
        (biogeographical_region == "Alpine" &
           latitude_dec <= 50) ~ 2,
      # 3: "North Atlantic, Subatlantic, Continental"
      (biogeographical_region == "Atlantic" &
         latitude_dec > 45) |
        biogeographical_region %in% c("Continental", "Pannonian") ~ 3,
      # 4: "Meditteranean"
      # Atlantic < 45
      # Black Sea
      # Macaronesia
      # Meditteranean
      (biogeographical_region == "Atlantic" &
         latitude_dec >= 45) |
        biogeographical_region %in% c("Black Sea", "Macaronesia",
                                      "Mediterranean") ~ 4
      # 5: "South Atlantic"
    ),
    # Class value of nitrogen conc (0 - 10)
    rv_n = case_when(
      n_conc_below_ground_topsoil < 0.5 ~ 1 * 2 - 1,
      n_conc_below_ground_topsoil < 1 ~ 2 * 2 - 1,
      n_conc_below_ground_topsoil < 2.5 ~ 3 * 2 - 1,
      n_conc_below_ground_topsoil < 5 ~ 4 * 2 - 1,
      !is.na(n_conc_below_ground_topsoil) ~ 5 * 2 - 1),
    # Rating value of C:N ratio (m/m) in organic and mineral layers (1 - 15)
    rv_c_to_n = case_when(
      c_to_n_ratio_topsoil == -1 ~ NA,
      c_to_n_ratio_topsoil <= 16 ~ 1 * 3 - 1.5,
      c_to_n_ratio_topsoil <= 24 ~ 2 * 3 - 1.5,
      c_to_n_ratio_topsoil <= 30 ~ 3 * 3 - 1.5,
      c_to_n_ratio_topsoil <= 40 ~ 4 * 3 - 1.5,
      !is.na(c_to_n_ratio_topsoil) ~ 5 * 3 - 1.5)) %>%
  # P availability metric
  mutate(
    n_avail = ifelse(
      !is.na(rv_climate) &
        !is.na(rv_n) &
        !is.na(rv_c_to_n),
      rv_n + rv_c_to_n + rv_climate,
      NA_real_)) %>%
  filter(!is.na(n_avail)) %>%
  mutate(
    n_avail_class_num = case_when(
      n_avail <= 12 ~ 1,
      n_avail <= 17.5 ~ 2,
      n_avail <= 23 ~ 3,
      n_avail <= 26.5 ~ 4,
      TRUE ~ 5),
    n_avail_class_cat = case_when(
      n_avail <= 12 ~ "Very low",
      n_avail <= 17.5 ~ "Low",
      n_avail <= 23 ~ "Medium",
      n_avail <= 26.5 ~ "High",
      TRUE ~ "Very high")) %>%
  arrange(desc(n_avail))

hist(df_n_avail$n_avail_class_num)

df_n_avail_sf <- df_n_avail %>%
  group_by(plot_id) %>%
  slice_max(order_by = survey_year) %>%
  ungroup()  %>%
  arrange(desc(n_avail)) %>%
  as_sf

map_icpf(layers = "df_n_avail_sf",
         title = paste0("**N availability** metric · ",
                        "ICP Forests (Level II)",
                        "<br>",
                        n_distinct(df_n_avail$plot_id),
                        " plots (most recent survey)",
                        "<br>_",
                        "Metric according to Vanmechelen et al. (1997)_"),
         legend_title = "**N availability**",
         legend_classes = TRUE,
         variable_cat = "n_avail_class_cat",
         export_name = paste0(code_survey, "_n_avail"),
         export_folder = "nutrient_avail",
         point_col = NULL,
         point_size = 0.5,
         biogeo_palette = NULL,
         count_plots_legend = TRUE,
         inset_maps_offset_x = 1.25)



p <- df_n_avail %>%
  # Count the number of occurrences for each year/class combination
  count(survey_year,
        n_avail_class_num,
        n_avail_class_cat) %>%
  # `n` is the count of each combination
  ggplot(aes(x = survey_year,
             y = factor(n_avail_class_num),
             fill = n)) +
  geom_tile() +  # Create a heatmap-like plot
  scale_fill_viridis(option = "magma",
                     direction = 1,
                     name = "Abundance",
                     trans = scales::pseudo_log_trans(sigma = 5)) +
  scale_y_discrete(
    labels = setNames(df_n_avail$n_avail_class_cat,
                      df_n_avail$n_avail_class_num)) +
  labs(
    x = "Survey year",
    title = paste0("**N availability** metric · ICP Forests (Level II)",
                   "<br>_Metric according to Vanmechelen et al. (1997)_")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black"),
    plot.title = element_markdown(hjust = 0,
                                  colour = "black",
                                  lineheight = 1.2,
                                  size = 10,
                                  margin = margin(b = 5)),
    axis.title.y = element_blank(),
    axis.title.x = element_markdown(hjust = 1,
                                    margin = margin(t = 5)),
    axis.text.x = element_text(hjust = 1,
                               size = 10,
                               colour = "black",
                               margin = margin(t = 5)),
    axis.text.y = element_text(size = 10,
                               margin = margin(r = 5),
                               colour = "black"),
    legend.title = element_markdown(size = 10,
                                    margin = margin(b = 10)),
    legend.text = element_text(size = 10),
    panel.grid = element_blank())



ggsave(paste0("./output/nutrient_avail/", code_survey,
              "_n_avail_over_time.png"),
       # Set the resolution of the saved plot (dots per inch)
       # Recommendation: at least 500 dpi
       plot = p,
       dpi = 500,
       # Set the width and height of the saved plot in inches
       width = 6.81)



# Acidification ----


bs_boundaries <- c(10, 20, 50, 95) # 0-10 is class 1
ph_boundaries <- c(3.2, 4.0, 5.0, 6.0) # <3.2 is class 1
carbonates_boundaries <- c(0.5, 20, 200, 500)


df_acid <- df_agg %>%
  rowwise() %>%
  mutate(
    bs = case_when(
      # 1: sum_base_cations and sum_acid_cations
      (!is.na(sum_acid_cations) & !is.na(sum_base_cations)) ~
        1E2 * sum_base_cations / (sum_acid_cations + sum_base_cations),
      # # 2: base_saturation
      # (!is.na(base_saturation)) ~ base_saturation,
      # 3: exch_bce and exch_ace
      (!is.na(exch_bce) & !is.na(exch_ace)) ~
        1E2 * exch_bce / (exch_bce + exch_ace),
      # 4:
      (!is.na(exch_bce) & !is.na(exch_cec)) ~
        1E2 * exch_bce / exch_cec,
      # 5:
      (!is.na(exch_ace) & !is.na(exch_cec)) ~
        1E2 * (exch_cec - exch_ace) / exch_cec,
      # 6:
      (!is.na(exch_bce) & !is.na(exch_acidiy)) ~
        1E2 * exch_bce / (exch_bce + exch_acidiy),
      # 7:
      (!is.na(sum_base_cations) & !is.na(exch_acidiy)) ~
        1E2 * sum_base_cations / (sum_base_cations + exch_acidiy),
      TRUE ~ NA_real_),
    carbonates = case_when(
      is.na(carbonates) & !is.na(ph_cacl2) & ph_cacl2 <= 5.5 ~ 0,
      TRUE ~ carbonates)) %>%
  ungroup() %>%
  mutate(
    # Class value of pH (cumulative)
    cv_ph = case_when(
      ph_cacl2 <= ph_boundaries[1] ~ 1 * 2 - 1,
      ph_cacl2 <= ph_boundaries[2] ~ 2 * 2 - 1,
      ph_cacl2 <= ph_boundaries[3] ~ 3 * 2 - 1,
      ph_cacl2 <= ph_boundaries[4] ~ 4 * 2 - 1,
      !is.na(ph_cacl2) ~ 5 * 2 - 1),
    # Class value of base saturation
    cv_bs = case_when(
      bs <= bs_boundaries[1] ~ 1,
      bs <= bs_boundaries[2] ~ 2,
      bs <= bs_boundaries[3] ~ 3,
      bs <= bs_boundaries[4] ~ 4,
      !is.na(bs) ~ 5),
    # Class value carbonates
    cv_caco3 = case_when(
      carbonates <= carbonates_boundaries[1] ~ 1,
      carbonates <= carbonates_boundaries[2] ~ 2,
      carbonates <= carbonates_boundaries[3] ~ 3,
      carbonates <= carbonates_boundaries[4] ~ 4,
      !is.na(carbonates) ~ 5)) %>%
  # Acidification index
  mutate(
    acidif = ifelse(
      !is.na(cv_ph) &
        !is.na(cv_bs) &
        !is.na(cv_caco3),
      cv_ph + cv_bs + cv_caco3,
      NA_real_)) %>%
  filter(!is.na(acidif)) %>%
  mutate(
    acidif_class_num = case_when(
      acidif > 12 ~ 1,
      acidif >= 10 ~ 2,
      acidif >= 7 ~ 3,
      acidif > 4 ~ 4,
      TRUE ~ 5),
    acidif_class_cat = case_when(
      acidif > 12 ~ "Very low",
      acidif >= 10 ~ "Low",
      acidif >= 7 ~ "Medium",
      acidif > 4 ~ "High",
      TRUE ~ "Very high")) %>%
  arrange((acidif))

hist(df_acid$acidif_class_num)



df_acid %>%
  filter(!is.na(bs) & !is.na(bs_class)) %>%
  mutate(bs_class = as.factor(bs_class)) %>%
  ggplot(aes(x = bs_class,
             y = bs)) +
  geom_boxplot()




df_acid_sf <- df_acid %>%
  group_by(plot_id) %>%
  slice_max(order_by = survey_year) %>%
  ungroup()  %>%
  arrange((acidif)) %>%
  as_sf

map_icpf(layers = "df_acid_sf",
         title = paste0("**Soil acidification** metric · ",
                        "ICP Forests (Level II)",
                        "<br>",
                        n_distinct(df_acid$plot_id),
                        " plots (most recent survey)",
                        "<br>_",
                        "Metric according to Vanmechelen et al. (1997)_"),
         legend_title = "**Acidification**",
         legend_classes = TRUE,
         variable_cat = "acidif_class_cat",
         export_name = paste0(code_survey, "_acidif"),
         export_folder = "nutrient_avail",
         point_col = NULL,
         point_size = 0.5,
         biogeo_palette = NULL,
         count_plots_legend = TRUE,
         inset_maps_offset_x = 1.25)




p <- df_acid %>%
  # Count the number of occurrences for each year/class combination
  count(survey_year,
        acidif_class_num,
        acidif_class_cat) %>%
  # `n` is the count of each combination
  ggplot(aes(x = survey_year,
             y = factor(acidif_class_num),
             fill = n)) +
  geom_tile() +  # Create a heatmap-like plot
  scale_fill_viridis(option = "magma",
                     direction = 1,
                     name = "Abundance",
                     trans = scales::pseudo_log_trans(sigma = 5)) +
  scale_y_discrete(
    labels = setNames(df_acid$acidif_class_cat,
                      df_acid$acidif_class_num)  # Map numbers to categories
  ) +
  labs(
    x = "Survey year",
    # y = "**Acidification**<br>class",
    title = paste0("**Soil acidification** metric · ICP Forests (Level II)",
                   "<br>_Metric according to Vanmechelen et al. (1997)_")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black"),
    plot.title = element_markdown(hjust = 0,
                                  colour = "black",
                                  lineheight = 1.2,
                                  size = 10,
                                  margin = margin(b = 5)),
    axis.title.y = element_blank(),
    axis.title.x = element_markdown(hjust = 1,
                                    margin = margin(t = 5)),
    axis.text.x = element_text(hjust = 1,
                               size = 10,
                               colour = "black",
                               margin = margin(t = 5)),
    axis.text.y = element_text(size = 10,
                               margin = margin(r = 5),
                               colour = "black"),
    legend.title = element_markdown(size = 10,
                                    margin = margin(b = 10)),
    legend.text = element_text(size = 10),
    panel.grid = element_blank())


ggsave(paste0("./output/nutrient_avail/", code_survey,
              "_acidification_over_time.png"),
       # Set the resolution of the saved plot (dots per inch)
       # Recommendation: at least 500 dpi
       plot = p,
       dpi = 500,
       # Set the width and height of the saved plot in inches
       width = 6.81)









