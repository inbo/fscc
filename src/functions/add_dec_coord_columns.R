
add_dec_coord_columns <- function(data_frame,
                                  incl_etrs89 = TRUE) {

  source("./src/functions/dec_coordinate.R")
  source("./src/functions/as_sf.R")


  assertthat::assert_that("longitude" %in% names(data_frame) &&
                            "latitude" %in% names(data_frame))


  df_added <- data_frame %>%
    mutate(latitude_dec = sapply(latitude,
                                 function(x) {
                                   paste(dec_coordinate(x,
                                                        error_report = TRUE),
                                         collapse = "_")
                                 }
    )) %>%
    separate(latitude_dec,
             into = c("latitude_dec", "latitude_dec_error"),
             sep = "_") %>%
    mutate(longitude_dec = sapply(longitude,
                                  function(x) {
                                    paste(dec_coordinate(x,
                                                         error_report = TRUE),
                                          collapse = "_")
                                  }
    )) %>%
    separate(longitude_dec,
             into = c("longitude_dec", "longitude_dec_error"),
             sep = "_") %>%
    mutate(ddmmss_error = ifelse(latitude_dec_error != "NA" |
                                   longitude_dec_error != "NA",
                                 "Error: outside range (0 - 59)",
                                 NA)) %>%
    mutate(longitude_dec = as.numeric(longitude_dec),
           latitude_dec = as.numeric(latitude_dec)) %>%
    select(-latitude_dec_error,
           -longitude_dec_error)


  if (incl_etrs89 == TRUE) {

    coordinate_list <- df_added %>%
      as_sf # EPSG 3035 (ETRS89 / LAEA Europe)

    # Extract the transformed coordinates
    coords_epsg3035 <- st_coordinates(coordinate_list)

    # Add the ETRS89 coordinates as new columns to the original dataframe
    df_added <- coordinate_list %>%
      mutate(x_etrs89 = coords_epsg3035[, 1],
             y_etrs89 = coords_epsg3035[, 2]) %>%
      st_drop_geometry()
  }


  return(df_added)

}
