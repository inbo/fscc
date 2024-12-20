
# Calculate carbon density between 80 and 100 cm
# _________________________________________

# This script calculates an average value for the carbon density (t C ha-1 cm-1)
# to be used to gap-fill carbon densities between 80 and 100 cm
# in the carbon stock calculation function ("get_stocks")

# Script initiation date: 2024-03-24


parameter_for_density <- "exch_ca"


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
  mutate(density = c_layer_stock / 20) %>%
  select(plot_id, density)



# Combine data ----

subsoil_densities <- bind_rows(
  s1_som %>%
    select(plot_id, profile_id,
           layer_limit_superior, layer_limit_inferior,
           bulk_density, coarse_fragment_vol, organic_carbon_total,
           n_total, extrac_p, extrac_s, rea_fe, rea_al, exch_ca),
  s1_pfh %>%
    rename(layer_limit_superior = horizon_limit_up) %>%
    rename(layer_limit_inferior = horizon_limit_low) %>%
    rename(organic_carbon_total = horizon_c_organic_total) %>%
    rename(n_total = horizon_n_total) %>%
    mutate(extrac_p = NA) %>%
    mutate(extrac_s = NA) %>%
    mutate(rea_fe = NA) %>%
    mutate(rea_al = NA) %>%
    mutate(exch_ca = horizon_exch_ca) %>%
    select(plot_id, profile_id,
           layer_limit_superior, layer_limit_inferior,
           bulk_density, coarse_fragment_vol, organic_carbon_total,
           n_total, extrac_p, extrac_s, rea_fe, rea_al, exch_ca)) %>%
  # so_som %>%
  #   select(plot_id, profile_id,
  #          layer_limit_superior, layer_limit_inferior,
  #          bulk_density, coarse_fragment_vol, organic_carbon_total),
  # so_pfh %>%
  #   rename(layer_limit_superior = horizon_limit_up) %>%
  #   rename(layer_limit_inferior = horizon_limit_low) %>%
  #   rename(organic_carbon_total = horizon_c_organic_total) %>%
  #   select(plot_id, profile_id,
  #          layer_limit_superior, layer_limit_inferior,
  #          bulk_density, coarse_fragment_vol, organic_carbon_total)) %>%
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
                                  NA_real_))


assertthat::assert_that(parameter_for_density %in% names(subsoil_densities))

names(subsoil_densities)[which(
  names(subsoil_densities) == parameter_for_density)] <- "parameter_for_density"


subsoil_densities <- subsoil_densities %>%
  # Carbon stock per layer (t C ha-1 (per layer))
  mutate(stock_layer =
           (.data$parameter_for_density * .data$bulk_density *
              (1 - .data$coarse_fragment_vol_frac) * .data$layer_thickness) /
           10000) %>%
  filter(!is.na(stock_layer) &
           stock_layer >= 0) %>%
  group_by(profile_id, plot_id) %>%
  reframe(stock_layer = sum(stock_layer, na.rm = TRUE),
          layer_thickness = sum(layer_thickness)) %>%
  ungroup() %>%
  # Calculate carbon density per cm (t C ha-1 cm-1)
  mutate(density = stock_layer / layer_thickness) %>%
  group_by(plot_id) %>%
  reframe(density = mean(density)) %>%
  ungroup

if (parameter_for_density == "organic_carbon_total") {

  subsoil_densities <- subsoil_densities %>%
    bind_rows(denmark_stocks)
}

subsoil_densities <- subsoil_densities %>%
  arrange(density) %>%
  pull(density)



summary(subsoil_densities)
median(subsoil_densities)
quantile(subsoil_densities, c(0.05, 0.95))

# organic_carbon_total:

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.009834  0.097200  0.298600  0.636684  0.565500 12.445875

#           5%        95%
#   0.05776157 2.58060000



# n_total:

#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0008195 0.0077880 0.0177666 0.0405909 0.0458679 0.8360472

#           5%        95%
#   0.00494602 0.11981280



# extrac_p:
# Only based on five plots!!! (four in UK, one in Ireland)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.416   4.883  11.801  12.539  20.703  23.894

#         5%       95%
#   2.109588 23.255596



# extrac_s:
# Only based on five plots!!!

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 3.991  11.465  17.918  15.631  19.556  25.223

#         5%       95%
#   5.485744 24.089562



# rea_fe:
# Only based on five plots!!!

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  2.557   4.143 151.380 238.341 385.578 648.046

#         5%       95%
#   2.874324 595.552650




# rea_al:
# Only based on five plots!!!

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  5.535  28.952  75.265  90.862 137.175 207.383

#         5%       95%
#   10.21837 193.34104




# exch_ca:

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.001186  0.015470  0.107171  0.560656  0.805233 10.578141

#            5%         95%
#   0.002165719 2.136135929





