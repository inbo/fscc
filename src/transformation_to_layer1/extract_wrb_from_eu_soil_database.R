
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

## 1.1. Raster: European Soil Database ----
#       (WRB-LEV1)

path_soil_database <-
  paste0("./data/additional_data/shapefiles/European Soil Database/",
         "SoilDB_rasters/wrblv1.tif")

soil_rsg <- terra::rast(path_soil_database)
terra::crs(soil_rsg)

legend <-
  read.csv(paste0("./data/additional_data/shapefiles/European Soil Database/",
                  "SoilDB_rasters/wrb-lev1-dictionary.csv"), sep = ";")



## 1.2. "s1" plots ----

source("./src/functions/dec_coordinate.R")
source("./src/functions/as_sf.R")

s1_missing_wrb <-
  read.csv("./data/additional_data/S1_PRF_missing_FAO_WRB.csv",
           sep = ";") %>%
  mutate(latitude_dec =
           sapply(latitude, dec_coordinate, error_report = FALSE),
         longitude_dec =
           sapply(longitude, dec_coordinate, error_report = FALSE)) %>%
  select(plot_id, latitude_dec, longitude_dec)

s1_missing_wrb_sf <-
  s1_missing_wrb %>%
  as_sf

st_crs(s1_missing_wrb_sf)



# 2. Extract raster values to vector points ----

s1_extracted <-
  suppressWarnings(terra::extract(soil_rsg, terra::vect(s1_missing_wrb_sf),
                                  ID = FALSE))

s1_missing_wrb$wrb_lev1 <- s1_extracted[, 1]

s1_wrb <- s1_missing_wrb %>%
  mutate(wrb_lev1 = as.character(wrb_lev1))


# 3. Fill data gaps ----

s1_wrb_gaps <- s1_wrb %>%
  filter(is.na(wrb_lev1)) %>%
  as_sf

  # Gaps for:
  # - three Canarian plots → no data in European Soil Database
  # - five Swedish plots → no data because right outside the edge with the
  #   sea (in comparison with raster). Reasonable to take the WRB nearby.

# Make a map to explore the Swedish data gaps

dev.new()

# Plot the raster with focus on Swedish plots
terra::plot(soil_rsg, xlim = c(4.5E6, 4.8E6), ylim = c(3.85E6, 4.05E6))

# Add points on top of the raster
terra::points(terra::vect(s1_wrb_gaps), col = "red", pch = 16, add = TRUE)

# Add labels based on the plot_id column
terra::text(terra::vect(s1_wrb_gaps),
            labels = s1_wrb_gaps$plot_id, pos = 1, col = "blue", cex = 0.8,
            add = TRUE)

# The five Swedish data gaps can be filled easily based on the map

s1_wrb <- s1_wrb %>%
  mutate(wrb_lev1 = case_when(
    plot_id %in% c("13_12987", "13_22447") ~ "PZ",
    plot_id == "13_17887" ~ "CM",
    plot_id %in% c("13_17841", "13_17526") ~ "LP",
    TRUE ~ wrb_lev1)) %>%
  left_join(legend,
            by = join_by("wrb_lev1" == "WRB.LEV1")) %>%
  rename(code_wrb_soil_group = wrb_lev1) %>%
  rename(wrb_soil_group = RSG)



# 4. Export data ----

write.table(s1_wrb,
            file = "./data/additional_data/S1_PRF_missing_FAO_WRB_suppl.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")






