
# Some statistics relevant for stocks
# Note that probably not all of them are still relevant, especially those
# referring to stock changes.
# These functions were made before the statistics meeting
# with Thierry, from which it appears that focussing on changes in individual
# plots is not the best approach.



# Define a function to calculate slope of linear regression
calculate_slope <- function(x, y) {
  if (length(unique(x)) > 1 &&
      length(x[!is.na(y)]) > 1 &&
      # Spanning more than eight years
      ((max(x) - min(x)) > 8)) {
    model <- lm(y ~ x)
    coef(model)[[2]]  # Slope coefficient
  } else {
    NA_real_
  }
}

# Define a function to calculate intercept of linear regression
calculate_intercept <- function(x, y) {
  if (length(unique(x)) > 1 &&
      length(x[!is.na(y)]) > 1 &&
      # Spanning more than eight years
      ((max(x) - min(x)) > 8)) {
    model <- lm(y ~ x)
    coef(model)[[1]]  # Slope coefficient
  } else {
    NA_real_
  }
}



# Bias-corrected and accelerated (BCa) confidence intervals, which adjust
# for both bias and skewness in the bootstrapped samples

boot_icpf <- function(data,
                      column_name,
                      stat = "mean_median",
                      nboot = 5000,
                      conf = 0.95) {

  alpha <- c(0.5 * (1 - conf),
             1 - 0.5 * (1 - conf))

  if (grepl("mean", stat)) {

    bca_ci <- bcanon(data %>%
                       pull(.data[[column_name]]),
                     nboot = nboot,
                     function(x) {mean(x, na.rm = TRUE)},
                     alpha = alpha)

    output_mean <- c(
      mean = round(mean(data[[column_name]]), 2),
      n = round(length(na.omit(data[[column_name]]))),
      mean_bca_ci_min = round(as.numeric(bca_ci$confpoints[1, 2]), 2),
      mean_bca_ci_max = round(as.numeric(bca_ci$confpoints[2, 2]), 2))
  }

  if (grepl("median", stat)) {

    bca_ci <- bcanon(data %>%
                       pull(.data[[column_name]]),
                     nboot = nboot,
                     function(x) {median(x, na.rm = TRUE)},
                     alpha = alpha)

    output_median <- c(
      median = round(median(data[[column_name]]), 2),
      n = round(length(na.omit(data[[column_name]]))),
      median_bca_ci_min = round(as.numeric(bca_ci$confpoints[1, 2]), 2),
      median_bca_ci_max = round(as.numeric(bca_ci$confpoints[2, 2]), 2))

  }

  if (stat == "mean") {
    output <- output_mean
  } else if (stat == "median") {
    output <- output_median
  } else {
    output <- c(output_mean, output_median)
  }

  return(output)

}





# Monte Carlo sampling function to estimate slope (sequestration rate)
# with uncertainty using triangular distribution.
# The triangular distribution is useful when you know the minimum, maximum,
# and the most likely value but lack information about the full distribution
# shape.

monte_carlo_slope <- function(data, column_name,
                              nsamples = 100) {

  # nsamples should be 1000 ideally,
  # but for try-out purposes, you can use a lower value (faster)

  slopes <- numeric(nsamples)
  column_name_min <- paste0(column_name, "_min")
  column_name_max <- paste0(column_name, "_max")

  for (i in seq_len(nsamples)) {

    sampled_stock <- mapply(rtriangle,
                            # lower limit of triangle distribution
                            a = data[[column_name_min]],
                            # upper limit of triangle distribution
                            b = data[[column_name_max]],
                            # mode (maximum) of triangle distribution
                            c = data[[column_name]])

    slopes[i] <- calculate_slope(data$survey_year, sampled_stock)

  }
  return(slopes)
}







# Bias-corrected and accelerated (BCa) confidence intervals of sequestration
# rates
# This function bootstraps the entire set of Monte Carlo-sampled slopes
# (sequestration rates) from all plots, this way directly reflecting
# the propagated uncertainties in the final bootstrapping process.

boot_mc_change <- function(data,
                           column_name,
                           stat = "mean_median",
                           nsamples_mc = 50,
                           nboot = 200,
                           conf = 0.95) {

  assertthat::assert_that(is_grouped_df(data))

  stock_changes <- data %>%
    # Combine direct slope calculation and Monte Carlo sampling
    do({
      # Direct slope calculation
      direct_slope <- calculate_slope(.$survey_year,
                                      .data[[column_name]])
      # Monte Carlo slope sampling
      mc_slopes <- monte_carlo_slope(.,
                                     column_name = column_name,
                                     nsamples = nsamples_mc)
      # Create tibble with both results
      tibble(
        # plot_id = unique(.$plot_id),
        stock_change = direct_slope,
        mean_slope_mc = mean(mc_slopes, na.rm = TRUE),
        median_slope_mc = median(mc_slopes, na.rm = TRUE),
        slopes_mc = list(mc_slopes)
      )
    }) %>%
    ungroup() %>%
    filter(!is.na(stock_change))

  # Unlist all slopes into a single vector for bootstrapping
  all_slopes <- unlist(stock_changes$slopes_mc)

  # Bootstrapping on all sampled slopes
  slope_results <- boot_icpf(data.frame(all_slopes),
                             column_name = "all_slopes",
                             stat = stat,
                             nboot = nboot,
                             conf = conf)

  slope_results[which(names(slope_results) == "n")] <-
    nrow(stock_changes)

  slope_results[which(names(slope_results) == "mean")] <-
    round(mean(stock_changes$stock_change), 2)

  slope_results[which(names(slope_results) == "median")] <-
    round(median(stock_changes$stock_change), 2)

  return(slope_results)
}

























