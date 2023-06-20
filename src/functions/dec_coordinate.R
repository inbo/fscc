
#' Convert coordinate from +/-DDMMSS to a decimal degree
#'
#' This function converts a coordinate in +/-DDMMSS to decimal degrees.
#'
#' @param coord Integer - Coordinate value (longitude or latitude) in +/-DDMMSS format
#' @return 
#' Outputs - This function returns a vector with two values:
#' - the decimal coordinate
#' - an "error" (character) if any of the original MM or SS is not in the 0-59 range,
#' or an NA if this is not the case.
#'
#' @examples
#' dec_coordinate(-3929)

dec_coordinate <- function(coord) {

  # Split all the individual digits of the given coordinate value
  # + convert to numeric + save as "vec"
  # Suppress warnings meanwhile, since the minus sign creates the warning that
  # NAs are introduced by coercion (which implies that the first value of "vec"
  # is an NA in case the coordinate is negative)
  
suppressWarnings(vec <- as.numeric(strsplit(as.character(coord), "")[[1]]));
  
  # Create "vec_short" which contains the values without the initial NA (when negative)
  
if (is.na(vec[1])) # when negative
{vec_short <- vec[2:length(vec)]} else {vec_short <- vec};
 
  # Identify DD, MM and SS
   
    # If there are 6 digits
  
    if (length(vec_short) == 6) {
      DD <- as.numeric(paste0(vec_short[1], vec_short[2]));
      MM <- as.numeric(paste0(vec_short[3], vec_short[4]));
      SS <- as.numeric(paste0(vec_short[5], vec_short[6]))
    
    # If there are 5 digits
    
    } else if (length(vec_short) == 5) {
      DD <- vec_short[1];
      MM <- as.numeric(paste0(vec_short[2],vec_short[3]));
      SS <- as.numeric(paste0(vec_short[4],vec_short[5]))
    
    # If there are 4 digits
    
    } else if (length(vec_short) == 4) {
      DD <- 0;
      MM <- as.numeric(paste0(vec_short[1],vec_short[2]));
      SS <- as.numeric(paste0(vec_short[3],vec_short[4]))
    
    # If there are 3 digits
    
    } else if (length(vec_short) == 3) {
      DD <- 0;
      MM <- vec_short[1];
      SS <- as.numeric(paste0(vec_short[2], vec_short[3]))
    
    # If there are 2 digits
    
    } else if (length(vec_short) == 2) {
      DD <- 0;
      MM <- 0;
      SS <- as.numeric(paste0(vec_short[1], vec_short[2]))
    
    # If there is 1 digit
    
    } else if (length(vec_short) == 1) {
      DD <- 0;
      MM <- 0;
      SS <- vec_short[1]};
    
  
  # Multiply with *(-1) if the original coordinate was negative
  
    if (is.na(vec[1])) {
      if (DD > 0) {
        DD <- (-1)*DD
    } else if (MM > 0) {
      MM <- (-1)*MM
    } else {
      SS <- (-1)*SS}};
    
  
  # Create "coord_error" which contains an inconsistency
  # if MM or SS are higher than 59
  # or an NA otherwise
  
    coord_error <- NA;
    if (MM > 59 | SS > 59) {
      coord_error <- "Error: out of range (0 - 59)"};
    if (is.na(vec[1])) {
      coord_dec <- DD - abs(MM)/60 - abs(SS)/3600} else {
        coord_dec <- DD + MM/60 + SS/3600};


  # Return a vector with the decimal coordinate + "coord_error"
  # (inconsistency or NA)
    
return(c(coord_dec, coord_error))
  
}
