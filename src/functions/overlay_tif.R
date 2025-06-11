
overlay_tif <- function(sf1,
                        path_tif = NULL,
                        spat_raster = NULL,
                        values_to_avoid = NULL,
                        buffer_radius = 2000,
                        buffer_radius_max = 5000,
                        map = FALSE) {

  assertthat::assert_that(!(identical(path_tif, NULL) &&
                              identical(spat_raster, NULL)))

  if (!identical(path_tif, NULL)) {
    assertthat::assert_that(file.exists(path_tif))
  }


  # Define required packages
  stopifnot(require("sf"),
            require("tidyverse"),
            require("assertthat"),
            require("terra"),
            require("stars"))

  source("./src/functions/add_dec_coord_columns.R")
  source("./src/functions/as_sf.R")

  # 1. Import tif ----

  if (!identical(path_tif, NULL)) {

    tif <- terra::rast(path_tif)

  } else {

    tif <- spat_raster
  }


  if (terra::crs(tif, proj = TRUE) == terra::crs("EPSG:4326", proj = TRUE)) {

    # This is usually less work than converting the tif

    sf1 <- sf1 %>%
      st_transform(crs = 4326)


  } else if (terra::crs(tif, proj = TRUE) !=
             terra::crs("EPSG:3035", proj = TRUE)) {

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

  name_col <- names(sf1_extracted)


  # 4. Avoid NA and values in values_to_avoid ----

  vec <- which(is.na(sf1$col_overlay) |
                 sf1$col_overlay %in% values_to_avoid)

  # If there are any such records, create a buffer and retrieve the
  # most abundant value apart from the unwanted options

  if (!identical(vec, integer(0))) {

    sf1_sub <- sf1[vec, ]

    sf1_sub_buffer <- terra::buffer(terra::vect(sf1_sub), buffer_radius)

    # Extract raster values within the buffered zone
    extracted_values <- suppressWarnings(
      terra::extract(tif,
                     sf1_sub_buffer,
                     exact = TRUE,
                     ID = TRUE,
                     na.rm = TRUE))

    names(extracted_values)[2] <- "col_overlay"

    # Retrieve the most common value apart from the unwanted values

    # Summarise per ID

    extracted_values_summ <- NULL

    for (i in seq_along(unique(extracted_values$ID))) {

      id_i <- unique(extracted_values$ID)[i]

      data_i <- extracted_values %>%
        filter(ID == id_i)

      reframed_col_overlay <-
        if (all(is.na(data_i$col_overlay))) {

          NA

          } else {

        # Calculate total abundance of each value
        abundance <- data_i %>%
          group_by(col_overlay) %>%
          reframe(total_abundance = sum(fraction, na.rm = TRUE)) %>%
          ungroup()

        # Remove NA values and values in values_to_avoid
        filtered_abundance <- abundance %>%
          filter(!is.na(col_overlay) & !(col_overlay %in% values_to_avoid))

        # If nothing left, return NA
        if (nrow(filtered_abundance) == 0) {
          NA
        } else {
          # Find the value with the highest abundance
          max_abundance_value <- filtered_abundance %>%
            arrange(desc(total_abundance)) %>%
            slice_head(n = 1) %>%
            pull(col_overlay)

          max_abundance_value
        }
          }

      extracted_values_summ <- bind_rows(
        extracted_values_summ,
        data.frame(ID = id_i,
                   col_overlay = reframed_col_overlay))

    } # End of for loop along all IDs



    # Add values to sf1_sub

    sf1_sub$col_overlay_buffer <- extracted_values_summ[, 2]

    sf1 <- sf1 %>%
      left_join(sf1_sub %>%
                  st_drop_geometry() %>%
                  select(plot_id, col_overlay_buffer),
                by = "plot_id") %>%
      mutate(
        col_overlay = ifelse(
          (is.na(col_overlay) | col_overlay %in% values_to_avoid) &
            (!is.na(col_overlay_buffer)),
          col_overlay_buffer,
          col_overlay)) %>%
      select(-col_overlay_buffer)

  } # End of "summarise buffer when any unwanted values"


  # 5. If there are still any unwanted values ----

  vec <- which(is.na(sf1$col_overlay) |
                 sf1$col_overlay %in% values_to_avoid)

  # If there are any such records, create a buffer and retrieve the
  # most abundant value apart from the unwanted options

  if (!identical(vec, integer(0))) {

    sf1_sub <- sf1[vec, ]

    sf1_sub_buffer <- terra::buffer(terra::vect(sf1_sub), buffer_radius_max)

    # Extract raster values within the buffered zone
    extracted_values <- suppressWarnings(
      terra::extract(tif,
                     sf1_sub_buffer,
                     exact = TRUE,
                     ID = TRUE,
                     na.rm = TRUE))

    names(extracted_values)[2] <- "col_overlay"

    # Retrieve the most common value apart from the unwanted values

    # Summarise per ID

    extracted_values_summ <- NULL

    for (i in seq_along(unique(extracted_values$ID))) {

      id_i <- unique(extracted_values$ID)[i]

      data_i <- extracted_values %>%
        filter(ID == id_i)

      reframed_col_overlay <-
        if (all(is.na(data_i$col_overlay))) {

          NA

        } else {

          # Calculate total abundance of each value
          abundance <- data_i %>%
            group_by(col_overlay) %>%
            reframe(total_abundance = sum(fraction, na.rm = TRUE)) %>%
            ungroup()

          # Remove NA values and values in values_to_avoid
          filtered_abundance <- abundance %>%
            filter(!is.na(col_overlay) & !(col_overlay %in% values_to_avoid))

          # If nothing left, return NA
          if (nrow(filtered_abundance) == 0) {
            NA
          } else {
            # Find the value with the highest abundance
            max_abundance_value <- filtered_abundance %>%
              arrange(desc(total_abundance)) %>%
              slice_head(n = 1) %>%
              pull(col_overlay)

            max_abundance_value
          }
        }

      extracted_values_summ <- bind_rows(
        extracted_values_summ,
        data.frame(ID = id_i,
                   col_overlay = reframed_col_overlay))

    } # End of for loop along all IDs



    # Add values to sf1_sub

    sf1_sub$col_overlay_buffer <- extracted_values_summ[, 2]

    sf1 <- sf1 %>%
      left_join(sf1_sub %>%
                  st_drop_geometry() %>%
                  select(plot_id, col_overlay_buffer),
                by = "plot_id") %>%
      mutate(
        col_overlay = ifelse(
          (is.na(col_overlay) | col_overlay %in% values_to_avoid) &
            (!is.na(col_overlay_buffer)),
          col_overlay_buffer,
          col_overlay)) %>%
      select(-col_overlay_buffer)

  } # End of "summarise buffer_max when still any unwanted values"











  if (st_crs(sf1)$epsg != 3035) {

    sf1 <- sf1 %>%
      st_transform(crs = 3035)
  }



  # Add name of column

  names(sf1)[which(names(sf1) == "col_overlay")] <- name_col



  return(sf1)





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
