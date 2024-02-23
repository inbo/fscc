
#' Get bulk density statistics
#'
#' This function retrieves bulk density statistics from various data sources.
#'
#' @param mode A character string specifying the type of stat.
#'             Possible values are "median" (default) and "quantile". If
#'             "quantile", the upper and lower quantile values of the whole
#'             range is returned. If "median", the median of the plausible
#'             values (i.e. those within the quantile range) is returned.
#' @param matrix_type A character string specifying the type of matrix.
#'                   Possible values are "organic" (default) and "mineral".
#' @param quantile A numeric value indicating the quantile range
#'                (default is 95).
#'
#' @return If mode is set to "quantile", returns a numeric vector containing
#'         the lower and upper quantiles. If mode is set to "median", returns
#'         the median bulk density value.
#'
#' @details This function retrieves bulk density statistics from multiple
#'          data sources based on the specified parameters. It calculates the
#'          median or quantile bulk density values based on the specified mode
#'          and matrix type.
#'
#' @examples
#' get_bulk_density_stats()
#'
#' @export

get_bulk_density_stats <- function(mode = "median",
                                   matrix_type = "organic",
                                   quantile = 95) {

  source("./src/functions/get_env.R")

  # Define quantile limits ----

  quantile_low <- 0.01 * (0.5 * (100 - quantile))
  quantile_up <- 0.01 * (0.5 * (100 + quantile))


  # Retrieve the dataframes ----

  # s1_som

  assertthat::assert_that(exists("s1_som", envir = .GlobalEnv))
  df_s1_som <- get_env("s1_som")

  if ("bulk_density_source" %in% names(df_s1_som)) {
    df_s1_som <- df_s1_som %>%
      filter(bulk_density_source == "som (same year)")
  }

  # s1_pfh

  assertthat::assert_that(exists("s1_pfh", envir = .GlobalEnv))
  df_s1_pfh <- get_env("s1_pfh")

  if ("bulk_density_source" %in% names(df_s1_pfh)) {
    df_s1_pfh <- df_s1_pfh %>%
      filter(bulk_density_source == "pfh (measured)")
  }

  # so_som

  assertthat::assert_that(exists("so_som", envir = .GlobalEnv))
  df_so_som <- get_env("so_som")

  if ("bulk_density_source" %in% names(df_so_som)) {
    df_so_som <- df_so_som %>%
      filter(bulk_density_source == "som (same year)")
  }

  # so_pfh

  assertthat::assert_that(exists("so_pfh", envir = .GlobalEnv))
  df_so_pfh <- get_env("so_pfh")

  if ("bulk_density_source" %in% names(df_so_pfh)) {
    df_so_pfh <- df_so_pfh %>%
      filter(bulk_density_source == "pfh (measured)")
  }

  # sw_swc

  assertthat::assert_that(exists("sw_swc", envir = .GlobalEnv))
  df_sw_swc <- get_env("sw_swc")




  # Assemble bulk densities including implausible ones ----

  bulk_densities_complete <- bind_rows(
    # s1_som
    df_s1_som %>%
      mutate(
        # Layer thickness
        layer_thickness =
          ifelse(!is.na(.data$layer_limit_superior) &
                   !is.na(.data$layer_limit_inferior) &
                   (.data$layer_limit_superior !=
                      .data$layer_limit_inferior),
                 abs(.data$layer_limit_superior -
                       .data$layer_limit_inferior),
                 NA_real_),
        # Bulk density based on layer weight
        # to compare with plausibility range of bulk density
        bulk_density_layer_weight =
          ifelse(!is.na(.data$organic_layer_weight) &
                   (.data$organic_layer_weight != -1) &
                   !is.na(.data$layer_thickness) &
                   (.data$layer_type %in% c("forest_floor", "peat")),
                 # kg m-3
                 .data$organic_layer_weight / (.data$layer_thickness * 1e-2),
                 NA_real_)) %>%
      # Assumption: bulk densities derived from organic layer weights
      # are generally more reliable than normal bulk densities in organic
      # layers
      mutate(bulk_density_harm =
               coalesce(bulk_density_layer_weight, bulk_density)) %>%
      filter(!is.na(bulk_density_harm)) %>%
      select(layer_type, code_layer, survey_year, bulk_density_harm),

    # s1_pfh
    df_s1_pfh %>%
      mutate(bulk_density_harm = .data$horizon_bulk_dens_measure) %>%
      filter(!is.na(bulk_density_harm)) %>%
      rename(code_layer = horizon_master) %>%
      select(layer_type, code_layer, survey_year, bulk_density_harm),

    # so_som
    df_so_som %>%
      mutate(
        # Layer thickness
        layer_thickness =
          ifelse(!is.na(.data$layer_limit_superior) &
                   !is.na(.data$layer_limit_inferior) &
                   (.data$layer_limit_superior !=
                      .data$layer_limit_inferior),
                 abs(.data$layer_limit_superior -
                       .data$layer_limit_inferior),
                 NA_real_),
        # Bulk density based on layer weight
        # to compare with plausibility range of bulk density
        bulk_density_layer_weight =
          ifelse(!is.na(.data$organic_layer_weight) &
                   (.data$organic_layer_weight != -1) &
                   !is.na(.data$layer_thickness) &
                   (.data$layer_type %in% c("forest_floor", "peat")),
                 # kg m-3
                 .data$organic_layer_weight / (.data$layer_thickness * 1e-2),
                 NA_real_)) %>%
      # Assumption: bulk densities derived from organic layer weights
      # are generally more reliable than normal bulk densities in organic
      # layers
      mutate(bulk_density_harm =
               coalesce(bulk_density_layer_weight, bulk_density)) %>%
      filter(!is.na(bulk_density_harm)) %>%
      select(layer_type, code_layer, survey_year, bulk_density_harm),

    # so_pfh
    df_so_pfh %>%
      mutate(bulk_density_harm = .data$horizon_bulk_dens_measure) %>%
      filter(!is.na(bulk_density_harm)) %>%
      rename(code_layer = horizon_master) %>%
      select(layer_type, code_layer, survey_year, bulk_density_harm),

    # sw_swc
    df_sw_swc %>%
      mutate(bulk_density_harm = bulk_density) %>%
      filter(!is.na(bulk_density_harm)) %>%
      rename(code_layer = code_depth_layer) %>%
      select(layer_type, code_layer, survey_year, bulk_density_harm))





  if (matrix_type == "organic") {

    bulk_densities_complete <- bulk_densities_complete %>%
      filter(layer_type %in% c("forest_floor", "peat")) %>%
      arrange(bulk_density_harm) %>%
      pull(bulk_density_harm)

  }

  if (matrix_type == "mineral") {

    bulk_densities_complete <- bulk_densities_complete %>%
      filter(layer_type %in% c("mineral")) %>%
      arrange(bulk_density_harm) %>%
      pull(bulk_density_harm)

  }

  quantiles_complete <-
    round(quantile(bulk_densities_complete, c(quantile_low, quantile_up)))

  if (mode == "quantile") {
    return(quantiles_complete)
  }

  if (mode == "median") {

    # Since the data for an organic matrix are skewed, with a long tail
    # of larger values, the median may be a better measure of central
    # tendency. The median is less influenced by extreme values or
    # outliers compared to the average, making it more robust in skewed
    # distributions.

    bulk_densities_plausible <-
      bulk_densities_complete[which(
        bulk_densities_complete > as.numeric(quantiles_complete[1]) &
          bulk_densities_complete < as.numeric(quantiles_complete[2]))]

    median_plausible <- round(median(bulk_densities_plausible))

    if (matrix_type == "organic") {
      assertthat::assert_that(median_plausible > 75 &
                                median_plausible < 130)
    }

    if (matrix_type == "mineral") {
      assertthat::assert_that(median_plausible > 1100 &
                                median_plausible < 1250)
    }

    return(median_plausible)
  }
}
