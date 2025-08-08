
harmonise_quantiles <- function(quantiles, round_level = NULL) {

  # To harmonise quantiles into nice interval boundaries
  # (e.g., to use for categorical classes for a continuous var in a map)
  # Example:
  #      20%       40%       60%       80%      100%
  # 65.10000  88.47667 113.17000 154.00500 833.77000
  # becomes:
  # 20%  40%  60%  80% 100%
  #  65   90  115  155  835

  if (is.null(round_level)) {

    max_value <- max(quantiles, na.rm = TRUE)

    # Determine rounding level based on max value
    if (max_value >= 0 & max_value < 100) {
      round_level <- 1
    } else if (max_value >= 100 & max_value < 1000) {
      round_level <- 5
    } else if (max_value >= 1000 & max_value < 10000) {
      round_level <- 100
    } else if (max_value >= 10000 & max_value < 100000) {
      round_level <- 500
    } else if (max_value >= 100000 & max_value < 1000000) {
      round_level <- 5000
    } else {
      # For values >= 1,000,000, you might want to add more levels
      round_level <- 50000
    }
  }

  # Round to the appropriate level
  # Using round() with digits parameter for level-based rounding
  rounded_quantiles <- round(quantiles / round_level) * round_level

  return(rounded_quantiles)

}

