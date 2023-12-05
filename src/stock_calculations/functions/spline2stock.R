
#' Calculate stocks per 10-cm depth interval from splines
#'
#' Calculate stocks per 10-cm depth interval
#' using the soilspline function for a given profile
#'
#' Initiated by Bruno
#'
#' @param prof Dataframe of a single soil profile (below-ground) with a column
#' plot_id, profile_id, code_layer, depth_top, depth_bottom, c_density,
#' soil_depth for the different depth layers
#' @param variab_name Name of the "density" column of the given variable,
#' e.g. "c_density".
#' @param survey_form Name of the survey form from which stocks are being
#' calculated, for the graphs
#' @param density_per_three_cm Logical indicating whether the density data
#' as returned from the spline function per 3 cm depth increment should be
#' included for graphing purposes
#' @param graph Logical indicating whether a graph of the spline fitting
#' should be made for the given depth profile
#'
#' @return stocks
#' @export
#'
#' @examples

spline2stock <- function(prof,
                         variab_name,
                         survey_form = NULL,
                         density_per_three_cm = FALSE,
                         graph = TRUE) {

  assertthat::assert_that(all(c("profile_id",
                                "depth_top",
                                "depth_bottom",
                                variab_name,
                                "soil_depth") %in% names(prof)))

  source("./src/stock_calculations/functions/soilspline.R")

  # Assert that there are at least two horizons

  assertthat::assert_that(nrow(prof) >= 2)

  # Assert that one horizon is the upper below-ground layer

  assertthat::assert_that(min(prof$depth_top) == 0)

  # Fit spline

  max_soil_depth <- unique(prof$soil_depth)

  spline_output <-
    soilspline(id = unique(prof$profile_id),
               depth_top = prof$depth_top,
               depth_bottom = prof$depth_bottom,
               variab = prof[[variab_name]],
               max_soil_depth = max_soil_depth,
               survey_form = survey_form,
               graph = graph)

  spline_output_per_cm <- spline_output$spline_output

  # Calculate cumulative carbon stocks from 0 until i cm,
  # until max_soil_depth
  # (spline_output_per_cm[i] gives the carbon density per cm
  # at a depth of i cm)

  stocks <- data.frame(
    nlay_below_ground = length(prof[[variab_name]]),
    # Cumulative carbon stocks from 0 until x cm
    stock_10 = ifelse(10 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(10)]),
                              2),
                        as.numeric(NA)),
    stock_20 = ifelse(20 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(20)]),
                              2),
                        as.numeric(NA)),
    stock_30 = ifelse(30 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(30)]),
                              2),
                        as.numeric(NA)),
    stock_40 = ifelse(40 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(40)]),
                              2),
                        as.numeric(NA)),
    stock_50 = ifelse(50 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(50)]),
                              2),
                        as.numeric(NA)),
    stock_60 = ifelse(60 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(60)]),
                              2),
                        as.numeric(NA)),
    stock_70 = ifelse(70 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(70)]),
                              2),
                        as.numeric(NA)),
    stock_80 = ifelse(80 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(80)]),
                              2),
                        as.numeric(NA)),
    stock_90 = ifelse(90 <= max_soil_depth,
                        round(sum(spline_output_per_cm[seq_len(90)]),
                              2),
                        as.numeric(NA)),
    stock_100 = ifelse(100 <= max_soil_depth,
                         round(sum(spline_output_per_cm[seq_len(100)]),
                               2),
                         as.numeric(NA)),
    stock_below_ground =
      ifelse(max_soil_depth > 1,
             round(sum(spline_output_per_cm[seq_len(max_soil_depth)]), 2),
             as.numeric(NA)),
    stock_below_ground_topsoil =
      ifelse(max_soil_depth > 1,
             ifelse(max_soil_depth < 30,
                    round(sum(spline_output_per_cm[seq_len(max_soil_depth)]),
                          2),
                    round(sum(spline_output_per_cm[seq_len(30)]),
                          2)),
             as.numeric(NA)),
    rmse_mpspline = round(as.numeric(spline_output$rmse_mpspline[1]),
                          3))



  if (density_per_three_cm == TRUE) {

    density_depths <- seq(1, 100, by = 3)

    for (i in density_depths) {
      stocks$new_col <- NA

      if (i <= round(max_soil_depth)) {
      stocks$new_col <- spline_output_per_cm[i]
      }

      names(stocks)[which(names(stocks) == "new_col")] <-
        paste0("density_", i)
    }

  }

  return(stocks)
}
