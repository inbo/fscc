
#' Convert dataframe to spatial points dataframe
#'
#' This function takes a dataframe containing decimal longitude and latitude
#' information and converts it into a spatial points dataframe using the
#' `sf` package. It ensures that the required columns 'longitude_dec'
#' and 'latitude_dec' are present in the input dataframe and that they
#' contain valid numeric values without any missing data (NAs).
#'
#' @param dataframe A dataframe containing longitude and latitude information.
#'
#' @return A spatial points dataframe (sf object) where the 'longitude_dec' and
#' 'latitude_dec' columns are used to create the spatial points geometry. The
#' spatial reference system (CRS) of the resulting sf object is set to
#' EPSG:3035.
#'
#' @details The input dataframe must have 'longitude_dec' and 'latitude_dec'
#' columns that represent the decimal degrees of longitude and latitude,
#' respectively.
#'
#' @seealso \code{\link{st_as_sf}}, \code{\link{st_transform}}
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' df <- data.frame(longitude_dec = c(12.34, 15.67, 18.91),
#'                  latitude_dec = c(56.78, 59.01, 61.23))
#' spatial_df <- as_sf(df)

as_sf <- function(dataframe) {

  assertthat::assert_that("longitude_dec" %in% names(dataframe) &&
                            "latitude_dec" %in% names(dataframe),
                          msg = paste0("Columns 'longitude_dec' and/or ",
                                       "'latitude_dec' do not exist in the ",
                                       "given dataframe."))

  assertthat::assert_that(
    length(which(is.na(dataframe$longitude_dec))) == 0 &&
      length(which(is.na(dataframe$latitude_dec))) == 0,
    msg = paste0("Column(s) 'longitude_dec' and/or 'latitude_dec' contain(s) ",
                 "at least one NA."))

  dataframe_spat <- dataframe %>%
    mutate(longitude_dec_sf = longitude_dec) %>%
    mutate(latitude_dec_sf = latitude_dec) %>%
    st_as_sf(coords = c("longitude_dec_sf", "latitude_dec_sf"),
             crs = 4326) %>%
    st_transform(crs = 3035)

  return(dataframe_spat)

}
