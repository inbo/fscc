
add_dec_coord_columns <- function(data_frame) {

  source("./src/functions/dec_coordinate.R")

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
    select(-latitude_dec_error,
           -longitude_dec_error)

  return(df_added)

}
