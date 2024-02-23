
# Overlay of ICP Forests plots with European Soil Database
# as a last option to assess the WRB Reference Soil Group (LI)

# Script initiation date: 11 Dec 2023


# Define required packages
stopifnot(require("sf"),
          require("tidyverse"),
          require("assertthat"),
          require("terra"),
          require("stars"))

# 1. Import data ----

## 1.1. Raster: WRB-LEV1 (European Soil Database) ----

path_soil_database <-
  paste0("./data/additional_data/shapefiles/European Soil Database/",
         "SoilDB_rasters/")

soil_wrb_lev1 <- terra::rast(paste0(path_soil_database,
                                    "wrblv1.tif"))
terra::crs(soil_wrb_lev1)

legend_wrb_lev1 <- read.csv(paste0(path_soil_database,
                                   "wrb-lev1-dictionary.csv"), sep = ";")




## 1.2. Raster: WRB-ADJ1 (European Soil Database) ----

soil_wrb_adj1 <- terra::rast(paste0(path_soil_database,
                                    "wrbadj1.tif"))
terra::crs(soil_wrb_adj1)

legend_wrb_adj1 <- read.csv(paste0(path_soil_database,
                                   "wrb-adj1-dictionary.csv"), sep = ";")


## 1.3. Raster: WRB-ADJ2 (European Soil Database) ----

soil_wrb_adj2 <- terra::rast(paste0(path_soil_database,
                                    "wrbadj2.tif"))
terra::crs(soil_wrb_adj2)

# Use legend_wrb_adj1



## 1.4. Raster: WRB-FULL (European Soil Database) ----

soil_wrb_full <- terra::rast(paste0(path_soil_database,
                                    "wrbfu.tif"))
terra::crs(soil_wrb_full)

legend_wrb_full <- read.csv(paste0(path_soil_database,
                                   "wrb-full-dictionary.csv"), sep = ";")





## 1.5. Raster: FAO90-FULL (European Soil Database) ----

soil_fao90_full <- terra::rast(paste0(path_soil_database,
                                      "fao90fu.tif"))
terra::crs(soil_fao90_full)

legend_fao90_full <- read.csv(paste0(path_soil_database,
                                     "fao90-full-dictionary.csv"), sep = ";")





## 1.6. "s1" plots ----

source("./src/functions/dec_coordinate.R")
source("./src/functions/as_sf.R")

s1_missing_wrb <-
  read.csv("./data/additional_data/S1_PRF_231204_missing_WRB_qual.csv",
           sep = ";") %>%
  rename(plot_id = PLOTID) %>%
  filter(!is.na(plot_id) &
           plot_id != "") %>%
  mutate(plot_id = ifelse(plot_id == "6_940010",
                          "6_10",
                          plot_id)) %>%
  left_join(coordinates_s1,
            by = "plot_id") %>%
  select(plot_id, latitude_dec, longitude_dec)

s1_missing_wrb_sf <-
  s1_missing_wrb %>%
  as_sf

st_crs(s1_missing_wrb_sf)



# 2. Extract raster values to vector points ----

## 2.1. WRB-LEV1 ----

s1_extracted <-
  suppressWarnings(terra::extract(soil_wrb_lev1,
                                  terra::vect(s1_missing_wrb_sf),
                                  ID = FALSE))

s1_missing_wrb$wrb_lev1 <- s1_extracted[, 1]


## 2.2. WRB-ADJ1 ----

s1_extracted <-
  suppressWarnings(terra::extract(soil_wrb_adj1,
                                  terra::vect(s1_missing_wrb_sf),
                                  ID = FALSE))

s1_missing_wrb$wrb_adj1 <- s1_extracted[, 1]


## 2.3. WRB-ADJ2 ----

s1_extracted <-
  suppressWarnings(terra::extract(soil_wrb_adj2,
                                  terra::vect(s1_missing_wrb_sf),
                                  ID = FALSE))

assertthat::assert_that(all(is.na(pull(unique(s1_extracted))) |
                              pull(unique(s1_extracted)) %in%
                              seq(1, 6)))

# This layer does not contain any data


## 2.4. WRB-FULL ----

s1_extracted <-
  suppressWarnings(terra::extract(soil_wrb_full,
                                  terra::vect(s1_missing_wrb_sf),
                                  ID = FALSE))

s1_missing_wrb$wrb_full <- s1_extracted[, 1]

## 2.5. FAO90-FULL ----

s1_extracted <-
  suppressWarnings(terra::extract(soil_fao90_full,
                                  terra::vect(s1_missing_wrb_sf),
                                  ID = FALSE))

s1_missing_wrb$fao90_full <- s1_extracted[, 1]




s1_wrb <- s1_missing_wrb %>%
  # Convert to characters
  mutate_at(vars(wrb_lev1, wrb_adj1, wrb_full, fao90_full), as.character) %>%
  # Convert records that contain number "6" to "6"
  mutate(across(c(wrb_lev1, wrb_adj1, wrb_full, fao90_full),
                ~ if_else(grepl("6", .), "6", .))) %>%
  # Convert records that contain number "6" to "6"
  mutate(across(c(wrb_lev1, wrb_adj1, wrb_full, fao90_full),
                ~ if_else(grepl("3", .), "3", .)))




# 3. Fill data gaps ----
## 3.1. Actual Swedish data gaps ----

s1_wrb_gaps <- s1_wrb %>%
  filter(is.na(wrb_lev1) |
           is.na(wrb_adj1) |
           is.na(wrb_full)) %>%
  as_sf

  # Gaps for:
  # - three Canarian plots → no data in European Soil Database
  # - five Swedish plots → no data because right outside the edge with the
  #   sea. Reasonable to take the WRB nearby.



# WRB-LEV1: Make a map to explore the Swedish data gaps

dev.new()

# Plot the raster with focus on Swedish plots
terra::plot(soil_wrb_lev1, xlim = c(4.5E6, 4.8E6), ylim = c(3.85E6, 4.05E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)



# WRB-ADJ1: Make a map to explore the Swedish data gaps

dev.new()

# Plot the raster with focus on Swedish plots
terra::plot(soil_wrb_adj1, xlim = c(4.5E6, 4.8E6), ylim = c(3.85E6, 4.05E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)



# WRB-FULL: Make a map to explore the Swedish data gaps

dev.new()

# Plot the raster with focus on Swedish plots
terra::plot(soil_wrb_full, xlim = c(4.5E6, 4.8E6), ylim = c(3.85E6, 4.05E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16, add = TRUE)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)





# FAO90-FULL: Make a map to explore the Swedish data gaps

dev.new()

# Plot the raster with focus on Swedish plots
terra::plot(soil_fao90_full, xlim = c(4.5E6, 4.8E6), ylim = c(3.85E6, 4.05E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16, add = TRUE)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)








# The five Swedish data gaps can be filled easily based on the map

s1_wrb <- s1_wrb %>%
  # WRB-LEV1
  mutate(wrb_lev1 = case_when(
    plot_id %in% c("13_12987", "13_22447") ~ "PZ",
    plot_id == "13_17887" ~ "CM",
    plot_id %in% c("13_17841", "13_17526") ~ "LP",
    TRUE ~ wrb_lev1)) %>%
  # WRB-ADJ1
  mutate(wrb_adj1 = ifelse(wrb_adj1 %in% c("13_12987", "13_22447", "13_17887",
                                           "13_17841", "13_17526"),
                           "dy",
                           .data$wrb_adj1)) %>%
  # WRB-FULL
  mutate(wrb_full = case_when(
    plot_id %in% c("13_12987", "13_22447") ~ "PZha",
    plot_id == "13_17887" ~ "CMdy",
    plot_id %in% c("13_17841", "13_17526") ~ "LPdy",
    TRUE ~ wrb_full)) %>%
  # FAO90-FULL
  mutate(fao90_full = case_when(
    plot_id %in% c("13_12987", "13_22447") ~ "PZh",
    plot_id == "13_17887" ~ "CMd",
    plot_id %in% c("13_17841", "13_17526") ~ "LPd",
    TRUE ~ fao90_full))






## 3.2. Plots on a water body ----

s1_wrb_gaps <- s1_wrb %>%
  filter(wrb_lev1 == "3" |
           wrb_adj1 == "3" |
           wrb_full == "3") %>%
  as_sf

# Gaps for:
# - one Spanish plot: 11_979
#   Reasonable to take the WRB nearby.


# WRB-LEV1: Make a map to explore the plot

dev.new()

# Plot the raster with focus on the plot
terra::plot(soil_wrb_lev1, xlim = c(2.9E6, 3E6), ylim = c(2E6, 2.1E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)



# WRB-ADJ1: Make a map to explore the plot

dev.new()

# Plot the raster with focus on the plot
terra::plot(soil_wrb_adj1, xlim = c(2.9E6, 3E6), ylim = c(2E6, 2.1E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)



# WRB-FULL: Make a map to explore the plot

dev.new()

# Plot the raster with focus on the plot
terra::plot(soil_wrb_full, xlim = c(2.9E6, 3E6), ylim = c(2E6, 2.1E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16, add = TRUE)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)





# FAO90-FULL: Make a map to explore the plot

dev.new()

# Plot the raster with focus on the plot
terra::plot(soil_fao90_full,  xlim = c(2.9E6, 3E6), ylim = c(2E6, 2.1E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16, add = TRUE)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)








# The class can be filled easily based on the map

s1_wrb <- s1_wrb %>%
  # WRB-LEV1
  mutate(wrb_lev1 = case_when(
    plot_id == "11_979" ~ "RG",
    TRUE ~ wrb_lev1)) %>%
  # WRB-ADJ1
  mutate(wrb_adj1 = case_when(
    plot_id == "11_979" ~ "dy",
    TRUE ~ wrb_adj1)) %>%
  # WRB-FULL
  mutate(wrb_full = case_when(
    plot_id == "11_979" ~ "RG",
    TRUE ~ wrb_full)) %>%
  # FAO90-FULL
  mutate(fao90_full = case_when(
    plot_id == "11_979" ~ "RGd",
    TRUE ~ fao90_full))






## 3.3. Plots on a rock outcrop ----

s1_wrb_gaps <- s1_wrb %>%
  filter(wrb_lev1 == "6" |
           wrb_adj1 == "6" |
           wrb_full == "6") %>%
  as_sf

# Gaps for:
# - one Swedish plot: 13_10534
#   Reasonable to take the WRB nearby.


# WRB-LEV1: Make a map to explore the plot

dev.new()

# Plot the raster with focus on the plot
terra::plot(soil_wrb_lev1, xlim = c(4.4E6, 4.6E6), ylim = c(4.4E6, 4.6E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)



# WRB-ADJ1: Make a map to explore the plot

dev.new()

# Plot the raster with focus on the plot
terra::plot(soil_wrb_adj1, xlim = c(4.4E6, 4.6E6), ylim = c(4.4E6, 4.6E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)



# WRB-FULL: Make a map to explore the plot

dev.new()

# Plot the raster with focus on the plot
terra::plot(soil_wrb_full, xlim = c(4.4E6, 4.6E6), ylim = c(4.4E6, 4.6E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16, add = TRUE)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)





# FAO90-FULL: Make a map to explore the plot

dev.new()

# Plot the raster with focus on the plot
terra::plot(soil_fao90_full, xlim = c(4.4E6, 4.6E6), ylim = c(4.4E6, 4.6E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16, add = TRUE)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "red", cex = 0.8)








# The class can be filled easily based on the map

s1_wrb <- s1_wrb %>%
  # WRB-LEV1
  mutate(wrb_lev1 = case_when(
    plot_id == "13_10534" ~ "PZ",
    TRUE ~ wrb_lev1)) %>%
  # WRB-ADJ1
  mutate(wrb_adj1 = case_when(
    plot_id == "13_10534" ~ "ha",
    TRUE ~ wrb_adj1)) %>%
  # WRB-FULL
  mutate(wrb_full = case_when(
    plot_id == "13_10534" ~ "PZha",
    TRUE ~ wrb_full)) %>%
  # FAO90-FULL
  mutate(fao90_full = case_when(
    plot_id == "13_10534" ~ "PZh",
    TRUE ~ fao90_full))


## 3.4. Canarian plots ----

# Remaining gaps

s1_wrb_gaps <- s1_wrb %>%
  filter(is.na(wrb_lev1)) %>%
  as_sf

# The Canary islands are not covered by the European soil map. However,
# their soil group can be derived based on the ISRIC website:
# https://soilgrids.org/

# This was done by visually comparing the maps and coordinates

leaflet() %>%
  addTiles() %>%  # OSM basemap
  addCircleMarkers(lng = s1_wrb_gaps$longitude_dec,
                   lat = s1_wrb_gaps$latitude_dec,
                   label = s1_wrb_gaps$plot_id,
                   labelOptions = labelOptions(noHide = TRUE))




# Fill the classes based on the map

s1_wrb <- s1_wrb %>%
  # WRB-LEV1
  mutate(wrb_lev1 = case_when(
    plot_id == "95_2113" ~ "LV",
    plot_id == "95_2114" ~ "LV",
    plot_id == "95_2115" ~ "CM",
    TRUE ~ wrb_lev1))



## 3.5. Convert codes to the actual groups ----

s1_wrb <- s1_wrb %>%
  # WRB-LEV1
  left_join(legend_wrb_lev1 %>%
              rename(wrb_soil_group = RSG),
            by = join_by("wrb_lev1" == "WRB.LEV1")) %>%
  # WRB-ADJ1
  left_join(legend_wrb_adj1 %>%
              rename(wrb_qualifier_1 = qualifier_1),
            by = join_by("wrb_adj1" == "WRB.ADJ1")) %>%
  # WRB-FULL
  left_join(legend_wrb_full %>%
              rename(wrb_full_class = wrb_full),
            by = join_by("wrb_full" == "WRB.FULL")) %>%
  # FAO90-FULL
  left_join(legend_fao90_full %>%
              rename(fao90_full_class = fao90_full),
            by = join_by("fao90_full" == "FAO90.FULL")) %>%
  relocate(wrb_lev1, wrb_adj1, wrb_full, fao90_full, .after = fao90_full_class)




# 4. Export data ----

write.table(s1_wrb,
            file =
              "./data/additional_data/S1_PRF_231204_missing_WRB_qual_suppl.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")






