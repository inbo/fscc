
overlay_tif <- function(sf1,
                        path_tif,
                        name_col = "col_overlay",
                        map = FALSE) {

  assertthat::assert_that(file.exists(path_tif))

  # Define required packages
  stopifnot(require("sf"),
            require("tidyverse"),
            require("assertthat"),
            require("terra"),
            require("stars"))

  source("./src/functions/add_dec_coord_columns.R")
  source("./src/functions/as_sf.R")

  # 1. Import tif ----

  tif <- terra::rast(path_tif)

  if (terra::crs(tif) != terra::crs("EPSG:3035")) {

    # Project the raster to the target CRS (EPSG 3035)
    tif <- terra::project(tif, terra::crs("EPSG:3035"))
  }


  # 2. Prepare data ----

  if (!"sf" %in% class(sf1)) {

    if (!"longitude_dec" %in% names(sf1) &&
        !"latitude_dec" %in% names(sf1)) {

      assertthat::assert_that(
        "longitude" %in% names(sf1) &&
          "latitude" %in% names(sf1),
        msg = paste0("No coordinates or geographical information found."))

      sf1 <- sf1 %>%
        add_dec_coord_columns
    }

    sf1 <- sf1 %>%
      as_sf
  }


  # 3. Extract raster values to vector points ----

  sf1_extracted <-
    suppressWarnings(terra::extract(tif,
                                    terra::vect(sf1),
                                    ID = FALSE))

  sf1$col_overlay <- sf1_extracted[, 1]

  if (name_col != "col_overlay") {
    names(sf1)[which(names(sf1) == "col_overlay")] <- name_col
  }


  if (map == TRUE) {

  # Make a map

  dev.new()

  # Plot the raster with focus on Swedish plots
  terra::plot(tif)#, xlim = c(4.5E6, 4.8E6), ylim = c(3.85E6, 4.05E6))

  # Add points on top of the raster
  terra::points(terra::vect(sf1), col = "red", pch = 16)

  # Add labels based on the plot_id column
  terra::text(terra::vect(sf1),
              labels = sf1$plot_id, pos = 1, col = "red", cex = 0.8)
  }



}
