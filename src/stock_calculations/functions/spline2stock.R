
#' Calculate stocks per 10-cm depth interval from splines
#'
#' Calculate stocks per 10-cm depth interval
#' using the soilspline function for a given profile
#'
#' Initiated by Bruno
#'
#' @param prof Dataframe of a single soil profile (below-ground) with a column
#' plot_id, profile_id, code_layer, depth_top, depth_bottom, c_density,
#' depth_stock for the different depth layers
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
                         parameter_name,
                         survey_form = NULL,
                         density_per_three_cm = FALSE,
                         graph = TRUE) {

  assertthat::assert_that(all(c("profile_id",
                                "depth_top",
                                "depth_bottom",
                                variab_name,
                                "depth_stock") %in% names(prof)))

  source("./src/stock_calculations/functions/soilspline.R")

  # Assert that there are at least two horizons

  assertthat::assert_that(nrow(prof) >= 2 ||
                            (nrow(prof) == 1 &&
                               max(prof$depth_bottom) >= 0.7 *
                               unique(prof$depth_stock)))

  # Assert that one horizon is the upper below-ground layer

  assertthat::assert_that(min(prof$depth_top) == 0)

  # Fit spline

  max_depth_stock <- unique(prof$depth_stock)

  if (any(paste0(variab_name, "_min") %in% names(prof))) {
    variab_min_i <- prof[[paste0(variab_name, "_min")]]
  } else {
    variab_min_i <- NULL
  }

  if (any(paste0(variab_name, "_max") %in% names(prof))) {
    variab_max_i <- prof[[paste0(variab_name, "_max")]]
  } else {
    variab_max_i <- NULL
  }

  if (any("gapfilled_post_layer1" %in% names(prof))) {
    variab_source <- prof$gapfilled_post_layer1
  } else {
    variab_source <- NULL
  }

  spline_output <-
    soilspline(id = unique(prof$profile_id),
               depth_top = prof$depth_top,
               depth_bottom = prof$depth_bottom,
               variab = prof[[variab_name]],
               variab_min = variab_min_i,
               variab_max = variab_max_i,
               variab_source = variab_source,
               max_depth_stock = max_depth_stock,
               parameter_name = parameter_name,
               survey_form = survey_form,
               graph = graph)

  spline_output_per_cm <- spline_output$spline_output

  # Calculate cumulative carbon stocks from 0 until i cm,
  # until max_depth_stock
  # (spline_output_per_cm[i] gives the carbon density per cm
  # at a depth of i cm)

  if ("gapfilled_post_layer1" %in% names(prof)) {

    nlay_below_ground <- prof %>%
      filter(is.na(gapfilled_post_layer1)) %>%
      nrow

  } else {

    nlay_below_ground <- length(prof[[variab_name]])

  }

  stocks <- data.frame(
    nlay_below_ground = nlay_below_ground,
    # Cumulative carbon stocks from 0 until x cm
    stock_10 = ifelse(10 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(10)]),
                              2),
                        NA_real_),
    stock_20 = ifelse(20 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(20)]),
                              2),
                        NA_real_),
    stock_30 = ifelse(30 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(30)]),
                              2),
                        NA_real_),
    stock_40 = ifelse(40 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(40)]),
                              2),
                        NA_real_),
    stock_50 = ifelse(50 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(50)]),
                              2),
                        NA_real_),
    stock_60 = ifelse(60 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(60)]),
                              2),
                        NA_real_),
    stock_70 = ifelse(70 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(70)]),
                              2),
                        NA_real_),
    stock_80 = ifelse(80 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(80)]),
                              2),
                        NA_real_),
    stock_90 = ifelse(90 <= max_depth_stock,
                        round(sum(spline_output_per_cm[seq_len(90)]),
                              2),
                        NA_real_),
    stock_100 = ifelse(100 <= max_depth_stock,
                         round(sum(spline_output_per_cm[seq_len(100)]),
                               2),
                         NA_real_),
    stock_below_ground =
      ifelse(max_depth_stock >= 1,
             round(sum(spline_output_per_cm[seq_len(max_depth_stock)]), 2),
             NA_real_),
    stock_below_ground_topsoil =
      ifelse(max_depth_stock >= 1,
             ifelse(max_depth_stock < 30,
                    round(sum(spline_output_per_cm[seq_len(max_depth_stock)]),
                          2),
                    round(sum(spline_output_per_cm[seq_len(30)]),
                          2)),
             NA_real_),
    rmse_mpspline = round(as.numeric(spline_output$rmse_mpspline[1]),
                          3))

  if ("spline_output_max" %in% names(spline_output) &&
      "spline_output_min" %in% names(spline_output)) {

    spline_output_per_cm_min <- spline_output$spline_output_min
    spline_output_per_cm_max <- spline_output$spline_output_max

    stocks <- cbind(
      stocks,
      data.frame(
        stock_below_ground_min =
          ifelse(max_depth_stock > 1,
                 round(sum(spline_output_per_cm_min[seq_len(max_depth_stock)]),
                       2),
                 NA_real_),
        stock_below_ground_max =
          ifelse(max_depth_stock > 1,
                 round(sum(spline_output_per_cm_max[seq_len(max_depth_stock)]),
                       2),
                 NA_real_),
        stock_below_ground_topsoil_min =
          ifelse(max_depth_stock > 1,
                 ifelse(max_depth_stock < 30,
                        round(sum(spline_output_per_cm_min[
                          seq_len(max_depth_stock)]),
                              2),
                        round(sum(spline_output_per_cm_min[seq_len(30)]),
                              2)),
                 NA_real_),
        stock_below_ground_topsoil_max =
          ifelse(max_depth_stock > 1,
                 ifelse(max_depth_stock < 30,
                        round(sum(spline_output_per_cm_max[
                          seq_len(max_depth_stock)]),
                          2),
                        round(sum(spline_output_per_cm_max[seq_len(30)]),
                              2)),
                 NA_real_)))

  }

  if (density_per_three_cm == TRUE) {

    density_depths <- seq(1, 100, by = 3)

    for (i in density_depths) {

      stocks$new_col <- NA

      if (i <= round(max_depth_stock)) {
      stocks$new_col <- round(spline_output_per_cm[i], 2)
      }

      names(stocks)[which(names(stocks) == "new_col")] <-
        paste0("density_", i)
    }

  }

  return(stocks)
}
