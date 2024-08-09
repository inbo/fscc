
#' Get parameter statistics
#'
#' This function retrieves parameter statistics from various data sources.
#'
#' @param parameter A character string specifying the variable that needs
#'                  to be summarised
#' @param mode A character string specifying the type of stat.
#'             Possible values are "quantile", "vec" and "stat". If
#'             "quantile", the upper and lower quantile values of the whole
#'             range is returned. If "stat", some statistics such as
#'             the median or mean of the plausible values (i.e. those within
#'             the quantile range) is returned. If "vec", the total vector
#'             is returned.
#' @param layer_type A character string specifying the type of layer.
#'                   Possible values are "organic" (default), "forest_floor",
#'                   "forest_floor_excl_ol", "peat" and "mineral",
#'                   "ol", "of", "oh".
#' @param data_source A character string specifying the source data to use
#'                    for the parameter statistics. Options include "LI",
#'                    "LII" and "LI_and_LII". Default is "LI", since Level I
#'                    is the systematic network and therefore assumed to be
#'                    representative for Europe.
#' @param quantile A numeric value indicating the quantile range
#'                (default is 95).
#' @param hist_plausible A logical indicating whether a histogram has to be
#'                       plotted from the plausible range of the data
#'                       (i.e. within quantile). Default is FALSE.
#'
#' @return If mode is set to "quantile", returns a numeric vector containing
#'         the lower and upper quantiles. If mode is set to "median", returns
#'         the median bulk density value.
#'
#' @details This function retrieves bulk density statistics from multiple
#'          data sources based on the specified parameters. It calculates the
#'          median or quantile bulk density values based on the specified mode
#'          and layer type.
#'
#' @examples
#' get_parameter_stats()
#'
#' @export

get_parameter_stats <- function(parameter = "bulk_density",
                                mode = "median",
                                layer_type = "organic",
                                data_source = "LI",
                                quantile = 95,
                                print = FALSE,
                                hist_plausible = FALSE) {

  source("./src/functions/get_env.R")

  par_source <- paste0(parameter, "_source")

  # Define quantile limits ----

  quantile_low <- 0.01 * (0.5 * (100 - quantile))
  quantile_up <- 0.01 * (0.5 * (100 + quantile))


  # Retrieve the dataframes ----

  empty_df <- data.frame(
    plot_id = NA_character_,
    code_layer = NA_character_,
    survey_year = NA_integer_,
    layer_limit_superior = NA_integer_,
    layer_limit_inferior = NA_integer_,
    layer_type = NA_character_,
    organic_layer_weight = NA_integer_,
    parameter = NA_integer_)


  ## s1_som ----

  if (data_source %in% c("LI", "LI_and_LII")) {

    if (exists("s1_som", envir = .GlobalEnv)) {

      df_s1_som <- get_env("s1_som")

      if (par_source %in% names(df_s1_som)) {
        df_s1_som <- df_s1_som %>%
          filter(!(grepl("pfh|swc", .data[[par_source]])))
      }

      if (parameter == "bulk_density") {

        df_s1_som <- df_s1_som %>%
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
                     .data$organic_layer_weight /
                       (.data$layer_thickness * 1e-2),
                     NA_real_)) %>%
          # Assumption: bulk densities derived from organic layer weights
          # are generally more reliable than normal bulk densities in organic
          # layers
          mutate(bulk_density_harm =
                   coalesce(bulk_density_layer_weight, bulk_density)) %>%
          select(-bulk_density) %>%
          rename(bulk_density = bulk_density_harm)
      }

      df_s1_som <- df_s1_som %>%
        rename("parameter" = any_of(parameter))

    } else {

      df_s1_som <- empty_df
      cat("\n's1_som' does not exist in Global Environment.\n")

    }


  } else {

    df_s1_som <- empty_df
  }


  ## s1_pfh ----

  if (data_source %in% c("LI", "LI_and_LII")) {

    if (exists("s1_pfh", envir = .GlobalEnv)) {

      df_s1_pfh <- get_env("s1_pfh")

      if (!"bulk_density" %in% names(df_s1_pfh)) {
        df_s1_pfh <- df_s1_pfh %>%
          mutate(bulk_density = horizon_bulk_dens_measure)
      }

      df_s1_pfh <- df_s1_pfh %>%
        rename(organic_carbon_total = horizon_c_organic_total) %>%
        rename(code_layer = horizon_master) %>%
        rename("parameter" = any_of(parameter))

      if (par_source %in% names(df_s1_pfh)) {
        df_s1_pfh <- df_s1_pfh %>%
          filter(!(grepl("som|swc", .data[[par_source]])))
      }


    } else {

      df_s1_pfh <- empty_df
      cat("\n's1_pfh' does not exist in Global Environment.\n")

    }


  } else {

    df_s1_pfh <- empty_df
  }


  ## so_som ----

  if (data_source %in% c("LII", "LI_and_LII")) {

    if (exists("so_som", envir = .GlobalEnv)) {

      df_so_som <- get_env("so_som")

      if (par_source %in% names(df_so_som)) {
        df_so_som <- df_so_som %>%
          filter(!(grepl("pfh|swc", .data[[par_source]])))
      }

      if (parameter == "bulk_density") {

        df_so_som <- df_so_som %>%
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
                     .data$organic_layer_weight /
                       (.data$layer_thickness * 1e-2),
                     NA_real_)) %>%
          # Assumption: bulk densities derived from organic layer weights
          # are generally more reliable than normal bulk densities in organic
          # layers
          mutate(bulk_density_harm =
                   coalesce(bulk_density_layer_weight, bulk_density)) %>%
          select(-bulk_density) %>%
          rename(bulk_density = bulk_density_harm)
      }

      df_so_som <- df_so_som %>%
        rename("parameter" = any_of(parameter))


    } else {

      df_so_som <- empty_df
      cat("\n'so_som' does not exist in Global Environment.\n")
    }

  } else {

    df_so_som <- empty_df
  }



  ## so_pfh ----

  if (data_source %in% c("LII", "LI_and_LII")) {

    if (exists("so_pfh", envir = .GlobalEnv)) {

      df_so_pfh <- get_env("so_pfh")

      if (!"bulk_density" %in% names(df_so_pfh)) {
        df_so_pfh <- df_so_pfh %>%
          mutate(bulk_density = horizon_bulk_dens_measure)
      }

      df_so_pfh <- df_so_pfh %>%
        rename(organic_carbon_total = horizon_c_organic_total) %>%
        rename(code_layer = horizon_master) %>%
        rename("parameter" = any_of(parameter))

      if (par_source %in% names(df_so_pfh)) {
        df_so_pfh <- df_so_pfh %>%
          filter(!(grepl("som|swc", .data[[par_source]])))
      }


    } else {

      df_so_pfh <- empty_df
      cat("\n'so_pfh' does not exist in Global Environment.\n")
    }

  } else {

    df_so_pfh <- empty_df
  }





  ## sw_swc ----

  if (data_source %in% c("LII", "LI_and_LII")) {

    if (parameter == "bulk_density") {

      if (exists("sw_swc", envir = .GlobalEnv)) {

        df_sw_swc <- get_env("sw_swc") %>%
          rename(code_layer = code_depth_layer) %>%
          rename("parameter" = any_of(parameter))

      } else {

        df_sw_swc <- empty_df
        cat("\n'sw_swc' does not exist in Global Environment.\n")
      }
    }

  } else {

    df_sw_swc <- empty_df
  }





  # Assemble data including implausible ones ----


  data_complete <- bind_rows(
    # s1_som
    df_s1_som %>%
      select(plot_id, layer_type, code_layer, survey_year, parameter),

    # s1_pfh
    df_s1_pfh %>%
      select(plot_id, layer_type, code_layer, survey_year, parameter),

    # so_som
    df_so_som %>%
      select(plot_id, layer_type, code_layer, survey_year, parameter),

    # so_pfh
    df_so_pfh %>%
      select(plot_id, layer_type, code_layer, survey_year, parameter))

  if (parameter == "bulk_density") {

    data_complete <- bind_rows(
      data_complete,
      # sw_swc
      df_sw_swc %>%
        select(plot_id, layer_type, code_layer, survey_year, parameter))
  }


  # Filter out NAs

  data_complete <- data_complete %>%
    filter(!is.na(parameter)) %>%
    arrange(parameter)



  # Obtain one value per code_layer per plot_id x survey_year
  # (so that each plot contributes to the same extent to the average)

  # Note that this is not yet the ideal approach, since this should ideally be
  # based on a depth match rather than a code_layer match

  data_complete <- data_complete %>%
    # Filter to keep only the most recent survey year for each plot_id
    group_by(plot_id) %>%
    filter(survey_year == max(survey_year)) %>%
    ungroup() %>%
    # Take average per code_layer
    group_by(plot_id, code_layer, layer_type) %>%
    reframe(parameter = mean(parameter, na.rm = TRUE))




  if (layer_type == "organic") {

    data_complete <- data_complete %>%
      filter(layer_type %in% c("forest_floor", "peat")) %>%
      arrange(parameter) %>%
      pull(parameter)

  }

  if (layer_type == "forest_floor") {

    data_complete <- data_complete %>%
      filter(layer_type %in% c("forest_floor")) %>%
      arrange(parameter) %>%
      pull(parameter)

  }

  if (layer_type == "forest_floor_excl_ol") {

    data_complete <- data_complete %>%
      filter(layer_type %in% c("forest_floor")) %>%
      filter(!grepl("L", .data$code_layer, ignore.case = TRUE)) %>%
      arrange(parameter) %>%
      pull(parameter)

  }

  if (layer_type == "ol") {

    data_complete <- data_complete %>%
      filter(layer_type %in% c("forest_floor")) %>%
      filter(code_layer == "OL") %>%
      arrange(parameter) %>%
      pull(parameter)

  }

  if (layer_type == "of") {

    data_complete <- data_complete %>%
      filter(layer_type %in% c("forest_floor")) %>%
      filter(code_layer == "OF") %>%
      arrange(parameter) %>%
      pull(parameter)

  }

  if (layer_type == "oh") {

    data_complete <- data_complete %>%
      filter(layer_type %in% c("forest_floor")) %>%
      filter(code_layer == "OH") %>%
      arrange(parameter) %>%
      pull(parameter)

  }

  if (layer_type == "peat") {

    data_complete <- data_complete %>%
      filter(layer_type %in% c("peat")) %>%
      arrange(parameter) %>%
      pull(parameter)

  }

  if (layer_type == "mineral") {

    data_complete <- data_complete %>%
      filter(layer_type %in% c("mineral")) %>%
      arrange(parameter) %>%
      pull(parameter)

  }

  quantiles_complete <-
    round(quantile(data_complete, c(quantile_low, quantile_up)))

  data_plausible <-
    data_complete[which(
      data_complete > as.numeric(quantiles_complete[1]) &
        data_complete < as.numeric(quantiles_complete[2]))]


  if (mode == "quantile") {
    result <- quantiles_complete
  }

  if (mode == "vec") {
    result <- data_plausible
  }

  if (mode == "stat") {

    result <- c(
      summary(data_plausible),
      quantile(data_plausible,
               probs = c(0.025, 0.05, 0.95, 0.975)),
      stdev = sd(data_plausible))

    if (print == TRUE) {

    cat(paste0("\n", parameter,
               " · layer type: '", layer_type, "':\n"))
    print(result)
    cat("\n")
    }
  }


  if (hist_plausible == TRUE) {

    source("./src/functions/hist_icpf.R")
    plot(hist_icpf(data = data_plausible,
              plot_title = paste0("**TOC · ",
                                  layer_type,
                                  "** (g kg<sup>-1</sup>)")))
  }


  return(result)

}
