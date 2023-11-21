
#' Fit spline and return predicted data per cm
#'
#' Fit a cm-specific mass-preserving spline (using mpspline2 function)
#' and extrapolate from 0 cm until a maximum soil depth
#' 
#' Initiated by Bruno
#' 
#' @param id Name of profile
#' @param depth_top Vector with layer top (in cm) for each horizon
#' @param depth_bottom Vector with layer bottom (in cm) for each horizon
#' @param variab Vector with data of variable to spline
#' @param max_soil_depth Maximum soil depth until which spline should be
#' extrapolated
#' @param graph Logical indicating whether a graph of the spline fitting
#' should be made for the given depth profile
#'
#' @details
#' Splines can only be fit to profiles with known variable data for at least
#' two layers, for which layer limits are known and from which one is the
#' upper below-ground layer
#' 
#' @return spline_output
#' @export
#'
#' @examples

soilspline <- function(id,
                       depth_top,
                       depth_bottom,
                       variab,
                       max_soil_depth,
                       graph = TRUE) {
  
  # Prepare packages
  
  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"),
            require("mpspline2"))
  
  # Set directory where graphs should be saved
  
  path <- c("./output/stocks/splines/")
  
  # Create directory if it doesn't exist
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  # Assert that input data are in the correct format ----
  
  assertthat::assert_that(all(!is.na(depth_top)) &&
                            all(!is.na(depth_bottom)) &&
                            all(!is.na(variab)) &&
                            !is.na(max_soil_depth))
  
  assertthat::assert_that(length(depth_top) == length(depth_bottom) &&
                            length(depth_top) == length(variab) &&
                            length(depth_top) >= 2)
  
  assertthat::assert_that(min(depth_top) == 0)
  
  
  
  # Fit a spline using mpspline function (mpspline2 package) ----
  # The max depth will be the bottom layer limit of the lowest layer)
  
  # Information about mpspline_one from GitHub:
  
  # This function implements the mass-preserving spline method of
  # Bishop et al (1999) for interpolating between measured soil attributes
  # down a single soil profile.
  # - site: data frame containing data for a single soil profile.
  #         * Column 1 must contain site identifiers.
  #         * Columns 2 and 3 must contain upper and lower sample depths,
  #           respectively, measured in centimeters.
  #         * Subsequent columns will contain measured values for those depths.
  # - var_name: length-1 character or length-1 integer denoting the column in
  #             "site" in which target data is stored. If not supplied, the
  #             fourth column of the input object is assumed to contain the
  #             target data.
  # - lam: smoothing parameter for spline. Defaults to 0.1.
  # - d: sequential integer vector; denotes the output depth ranges in cm.
  #      Defaults to c(0, 5, 15, 30, 60, 100, 200) after the GlobalSoilMap
  #      specification, giving output predictions over intervals 0-5 cm,
  #      5-15 cm, etc.
  # - vlow: numeric; constrains the minimum predicted value to a realistic
  #         number. Defaults to 0.
  # - vhigh: numeric; constrains the maximum predicted value to a realistic
  #          number. Defaults to 1000.
  
  # Returns a list with the following elements:
  # - Site ID,
  # - vector of predicted values over input intervals
  # - vector of predicted values for each cm down the profile to max(d)
  # - vector of predicted values over d (output) intervals
  # - root mean squared error (RMSE) +
  #   the interquartile range of the root mean squared error (RMSE_IQR)
  
  
  # Prepare input for mpspline_one
  
  prof_input <-
    data.frame(id,
               depth_top,
               depth_bottom,
               variab)
  
  mpspline_output <- mpspline_one(site = prof_input,
                                  var_name = "variab",
                                  lam = 0.1)
  
  # Extrapolate the mass-preserving spline output to depth max_soil_depth ----
  # (i.e. effective soil depth; max 100 cm)
  # using the spline function (stats package)
  
  x <- mpspline_output$est_1cm[seq_len(max(depth_bottom))]
  
  spline_output <-
    spline(x = x,
           # y should not be negative (e.g. not c(-1, -2, -3...))
           y = NULL,
           # Natural splines
           method = "natural",
           # "xout" is an optional set of values specifying where
           # interpolation is to take place
           # This enables extrapolation beyond the lower boundary of
           # the observations
           xout = seq_len(max_soil_depth))
  
  # Predicted and extrapolated variab
  spline_output <- spline_output$y  
  
  # Output variable should not be below 0
  spline_output <- ifelse(spline_output < 0,
                          0,
                          spline_output)
  
  # Creating a list to store multiple results
  result <- list(
    spline_output = spline_output,
    rmse_mpspline = mpspline_output$est_err[1])
  
  
  # Make a graph ----
  
  if (graph == TRUE) { 
    
    par(mfrow = c(1, 1))
    
    # Save plot as a file
    
    fname <- paste0(path, id, ".png")
    png(file = fname)
    
    # Plot mpspline
    
    depth_average <- (-1) * (depth_top + depth_bottom) / 2
    
    plot(variab,
         depth_average,
         xlim = c(0, max(variab, na.rm = TRUE)),
         ylim = c(-1 * max_soil_depth, 0),
         ylab = c("Depth (cm)"),
         col = "black",
         cex = 1.2,
         pch = 16,
         main = id, 
         xlab = expression('Carbon density (t C ha'^-1*'cm'^-1*')'))
    
    # Plot bulk horizons
    
    x_left <- rep(0, length(depth_top))
    y_bottom <- (-1) * depth_bottom
    x_right <- variab
    y_top <- (-1) * depth_top
    
    rect(x_left,
         y_bottom,
         x_right,
         y_top,
         col = 8,
         border = "white",
         lwd = 2)
    
    # Plot points and lines
    
    # Max depth of profile
    abline(h = -1 * max_soil_depth,
           col = 3,
           lty = 2,
           lwd = 2)
    
    # 1-cm mpspline till max depth
    lines(spline_output[seq_len(max_soil_depth)],
          -1 * seq_len(max_soil_depth),
          col = "blue",
          lwd = 3)
    
    points(spline_output,
           seq_along(spline_output),
           col = 3)
    
    # Midpoints of horizons
    points(variab,
           depth_average,
           col = "black",
           cex = 1.2,
           pch = 16,
           main = id)
    
    # Save PNG graph output with id name
    dev.off()
    
  }
  
  # Return splined variable for each cm until the maximum soil depth
  return(result)
}
