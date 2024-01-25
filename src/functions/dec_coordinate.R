
#' Convert coordinate from +/-DDMMSS to a decimal degree
#'
#' This function converts a coordinate in +/-DDMMSS to decimal degrees.
#'
#' @param coord Integer - Coordinate value (longitude or latitude) in +/-DDMMSS
#' format
#' @param error_report Logical - Indicates whether any transformation error
#' (due to coordinates out of the 0 - 59 range) have to be reported along.
#' Default is TRUE.
#' @return
#' Outputs - This function returns a vector with two values:
#' - the decimal coordinate
#' - an "error" (character: "Error: not in possible range (0 - 59)") if any
#' of the original mm or ss is not in the 0-59 range, or an NA if this is
#' not the case.
#'
#' @examples
#' dec_coordinate(-3929)

dec_coordinate <- function(coord, error_report = TRUE) {


  # Split all the individual digits of the given coordinate value
  # + convert to numeric + save as "vec"
  # Suppress warnings meanwhile, since the minus sign creates the warning that
  # NAs are introduced by coercion (which implies that the first value of "vec"
  # is an NA in case the coordinate is negative)

suppressWarnings(
  vec <- as.numeric(strsplit(as.character(format(coord, scientific = FALSE)),
                             "")[[1]])
  )

  # Create "vec_short" which contains the values without the initial NA
  # (when negative)

if (is.na(vec[1])) { # When negative

vec_short <- vec[2:length(vec)]

} else {
  vec_short <- vec
  }

  # Identify dd, mm and ss

    # If there are 6 digits

    if (length(vec_short) == 6) {

      dd <- as.numeric(paste0(vec_short[1], vec_short[2]))
      mm <- as.numeric(paste0(vec_short[3], vec_short[4]))
      ss <- as.numeric(paste0(vec_short[5], vec_short[6]))

    # If there are 5 digits

    } else if (length(vec_short) == 5) {

      dd <- vec_short[1]
      mm <- as.numeric(paste0(vec_short[2], vec_short[3]))
      ss <- as.numeric(paste0(vec_short[4], vec_short[5]))

    # If there are 4 digits

    } else if (length(vec_short) == 4) {

      dd <- 0
      mm <- as.numeric(paste0(vec_short[1], vec_short[2]))
      ss <- as.numeric(paste0(vec_short[3], vec_short[4]))

    # If there are 3 digits

    } else if (length(vec_short) == 3) {

      dd <- 0
      mm <- vec_short[1]
      ss <- as.numeric(paste0(vec_short[2], vec_short[3]))

    # If there are 2 digits

    } else if (length(vec_short) == 2) {

      dd <- 0
      mm <- 0
      ss <- as.numeric(paste0(vec_short[1], vec_short[2]))

    # If there is 1 digit

    } else if (length(vec_short) == 1) {

      dd <- 0
      mm <- 0
      ss <- vec_short[1]
      }


  # Multiply with *(-1) if the original coordinate was negative

    if (is.na(vec[1])) {

      if (dd > 0) {
      dd <- (-1) * dd

    } else if (mm > 0) {
      mm <- (-1) * mm

    } else {
      ss <- (-1) * ss
    }
    }


  # Create "coord_error" which contains an inconsistency
  # if mm or ss are higher than 59
  # or an NA otherwise

    coord_error <- NA

    if (mm > 59 || ss > 59) {
      coord_error <- "Error: outside range (0 - 59)"
      }

  # Sum up the final decimal coordinate

    if (is.na(vec[1])) { # Negative
        coord_dec <- dd - abs(mm) / 60 - abs(ss) / 3600
      } else { # Positive
        coord_dec <- dd + mm / 60 + ss / 3600
      }


  # Return a vector with the decimal coordinate + "coord_error"
  # (inconsistency or NA)

    if (error_report == TRUE) {
        return(c(round(coord_dec, 5), coord_error))
    } else {
        return(round(coord_dec, 5))
      }

}
