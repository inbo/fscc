
# Calculate carbon density between 80 and 100 cm
# _________________________________________

# This script calculates an average value for the carbon density (t C ha-1 cm-1)
# to be used to gap-fill carbon densities between 80 and 100 cm
# in the carbon stock calculation function ("get_stocks")

# Script initiation date: 2024-03-24


# Import data ----

# Layer 1 data

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)

# Data Denmark

denmark_stocks <-
  openxlsx::read.xlsx(paste0("data/additional_data/",
                             "national_coauthor_carbon_stocks/Denmark/",
                             "Denmark_BIOSOIL Level I soil C data from 2007 ",
                             "and 20187Mar2024.xlsx"),
                      sheet = 1) %>%
  mutate(plot_id = paste0("8_", Plot)) %>%
  rename(c_layer_stock = "C.(Mg.C.ha-1)") %>%
  filter(Layer == "80-100 cm") %>%
  mutate(c_density = c_layer_stock / 20) %>%
  select(plot_id, c_density)



# Combine data ----

subsoil_c_densities <- bind_rows(
  s1_som %>%
    select(plot_id, profile_id,
           layer_limit_superior, layer_limit_inferior,
           bulk_density, coarse_fragment_vol, organic_carbon_total),
  s1_pfh %>%
    rename(layer_limit_superior = horizon_limit_up) %>%
    rename(layer_limit_inferior = horizon_limit_low) %>%
    rename(organic_carbon_total = horizon_c_organic_total) %>%
    select(plot_id, profile_id,
           layer_limit_superior, layer_limit_inferior,
           bulk_density, coarse_fragment_vol, organic_carbon_total),
  so_som %>%
    select(plot_id, profile_id,
           layer_limit_superior, layer_limit_inferior,
           bulk_density, coarse_fragment_vol, organic_carbon_total),
  so_pfh %>%
    rename(layer_limit_superior = horizon_limit_up) %>%
    rename(layer_limit_inferior = horizon_limit_low) %>%
    rename(organic_carbon_total = horizon_c_organic_total) %>%
    select(plot_id, profile_id,
           layer_limit_superior, layer_limit_inferior,
           bulk_density, coarse_fragment_vol, organic_carbon_total)) %>%
  # Filter for the wanted depths
  filter(!is.na(layer_limit_superior) &
           !is.na(layer_limit_inferior) &
           layer_limit_superior >= 70 &
           layer_limit_superior < 90 &
           layer_limit_inferior <= 110 &
           layer_limit_inferior > 90) %>%
  mutate(coarse_fragment_vol_frac =
           ifelse(is.na(.data$coarse_fragment_vol),
                  0,
                  .data$coarse_fragment_vol / 100)) %>%
  mutate(layer_thickness = ifelse(!is.na(.data$layer_limit_superior) &
                                    !is.na(.data$layer_limit_inferior) &
                                    (.data$layer_limit_superior !=
                                       .data$layer_limit_inferior),
                                  abs(.data$layer_limit_superior -
                                        .data$layer_limit_inferior),
                                  NA_real_)) %>%
  # Carbon stock per layer (t C ha-1 (per layer))
  mutate(c_stock_layer =
           (.data$organic_carbon_total * .data$bulk_density *
              (1 - .data$coarse_fragment_vol_frac) * .data$layer_thickness) /
           10000) %>%
  filter(!is.na(c_stock_layer)) %>%
  group_by(profile_id, plot_id) %>%
  reframe(c_stock_layer = sum(c_stock_layer, na.rm = TRUE),
          layer_thickness = sum(layer_thickness)) %>%
  ungroup() %>%
  # Calculate carbon density per cm (t C ha-1 cm-1)
  mutate(c_density = c_stock_layer / layer_thickness) %>%
  group_by(plot_id) %>%
  reframe(c_density = mean(c_density)) %>%
  ungroup %>%
  bind_rows(denmark_stocks) %>%
  arrange(c_density) %>%
  pull(c_density)



summary(subsoil_c_densities)
median(subsoil_c_densities)
quantile(subsoil_c_densities, c(0.05, 0.95))

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.009834  0.098497  0.306272  0.730857  0.616370 12.661800

#           5%        95%
#   0.05784099 2.23027130





