
#' Calculate stocks per 10-cm depth interval
#'
#' Calculate stocks per 10-cm depth interval
#' using the soilspline function for a given profile
#' 
#' Initiated by Bruno
#' 
#' @param prof Dataframe of a single soil profile (below-ground) with a column
#' plot_id, profile_id, code_layer, depth_top, depth_bottom, carbon_density,
#' eff_soil_depth for the different depth layers
#'
#' @return stocks
#' @export
#'
#' @examples

calculate_stocks <- function(prof) {
  
  assertthat::assert_that(all(c("profile_id",
                                "depth_top",
                                "depth_bottom",
                                "carbon_density",
                                "eff_soil_depth") %in% names(prof)))
  
  source("./src/stock_calculations/functions/soilspline.R")
  
  # Assert that there are at least two horizons
  
  assertthat::assert_that(nrow(prof) >= 2)
  
  # Assert that one horizon is the upper below-ground layer
  
  assertthat::assert_that(min(prof$depth_top) == 0)
  
  # Fit spline
  
  max_soil_depth <- unique(prof$eff_soil_depth)
  
  spline_output <-
    soilspline(id = unique(prof$profile_id),
               depth_top = prof$depth_top,
               depth_bottom = prof$depth_bottom,
               variab = prof$carbon_density,
               max_soil_depth = max_soil_depth,
               graph = TRUE)
  
  spline_output_per_cm <- spline_output$spline_output
  
  # Calculate cumulative carbon stocks from 0 until i cm,
  # until max_soil_depth
  # (spline_output_per_cm[i] gives the carbon density per cm
  # at a depth of i cm)
  
  stocks <- data.frame(
    nlay = length(prof$carbon_density),
    # Cumulative carbon stocks from 0 until x cm
    carbon_stock_cum_10 = ifelse(10 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(10)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_20 = ifelse(20 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(20)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_30 = ifelse(30 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(30)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_40 = ifelse(40 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(40)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_50 = ifelse(50 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(50)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_60 = ifelse(60 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(60)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_70 = ifelse(70 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(70)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_80 = ifelse(80 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(80)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_90 = ifelse(90 <= max_soil_depth,
                                 round(sum(spline_output_per_cm[seq_len(90)]),
                                       2),
                                 as.numeric(NA)),
    carbon_stock_cum_100 = ifelse(100 <= max_soil_depth,
                                  round(sum(spline_output_per_cm[seq_len(100)]),
                                        2),
                                  as.numeric(NA)),
    carbon_stock_below_ground =
      ifelse(max_soil_depth > 1,
             round(sum(spline_output_per_cm[seq_len(max_soil_depth)]), 2),
             as.numeric(NA)),
    rmse_mpspline = round(as.numeric(spline_output$rmse_mpspline[1]),
                          3))
  
  return(stocks)
}
