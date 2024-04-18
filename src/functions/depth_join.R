
#' Depth_join function
#'
#' This function joins data from a dataframe `df2` (with one profile
#' per `plot_id`) with a dataframe `df1` by converting the data to the
#' depth ranges of each record in `df1`. Above-ground forest floor layers
#' are however joined based on the layer code, i.e. as in the equivalent
#' soil mass approach.
#'
#' @param df1 Dataframe representing depth ranges with columns:
#' `code_layer`, `layer_limit_superior`, `layer_limit_inferior`, `plot_id`,
#' `layer_type`.
#' @param df2 Dataframe containing data to be joined with `df1`. Should have
#' one profile per `plot_id`, which can be achieved using the function
#' ./src/functions/harmonise_per_plot_layer.
#' @param parameters A character vector specifying the parameters to be joined.
#' Default is `NULL`, which selects predefined parameters.
#' @param prefix_parameters_in_df1 A character string representing the prefix
#' to be added to the parameter names in `df1` (e.g. the source of the data
#' in `df2`). Default is `NULL`.
#' @param mode A character string representing the mode in which the dataframes
#' should be joined. There are two options: "constant_physical_parameters"
#' (default; this joins physical parameters regardless of the survey year in
#'  any relevant layers); or "time_specific_ff_concentrations" (this joins
#'  analytical parameters which are specific for a certain survey year, only
#'  in the forest floor); or "most_recent" (which selects the most recent
#'  data to join)
#'
#' @return A dataframe representing the joined data.
#'
#' @export
#'
#' @examples
#' # depth_join(df1, df2, prefix_parameters_in_df1 = "swc_")

depth_join <- function(df1,
                       df2,
                       parameters = NULL,
                       prefix_parameters_in_df1 = NULL,
                       mode = "constant_physical_parameters") {


  source("./src/functions/harmonise_layer_to_depths.R")

  assertthat::assert_that(
    all(c("code_layer", "layer_limit_superior", "layer_limit_inferior",
          "plot_id", "layer_type") %in% names(df1)))

  assertthat::assert_that(
    all(c("code_layer", "layer_limit_superior", "layer_limit_inferior",
          "plot_id", "layer_type") %in% names(df1)))

  if (mode == "time_specific_ff_concentrations") {
    assertthat::assert_that("survey_year" %in% names(df1) &&
                              "survey_year" %in% names(df2))
  }

  # There should be one profile per plot_id in df2

  # assertthat::assert_that(
  #   identical(
  #     which(duplicated(paste0(df2$plot_id, "_", df2$layer_limit_superior, "_",
  #                             df2$layer_limit_inferior))),
  #     integer(0)))


  if (all(c("horizon_master",
            "horizon_limit_up",
            "horizon_limit_low") %in% names(df2))) {

    df2 <- df2 %>%
      rename(code_layer = horizon_master) %>%
      rename(layer_limit_superior = horizon_limit_up) %>%
      rename(layer_limit_inferior = horizon_limit_low)

  }



  # Define parameters to join ----

  if (is.null(parameters)) {

    if (mode == "constant_physical_parameters") {

    possible_parameters <- c(
        "bulk_density",
        "bulk_density_survey_year",
        "bulk_density_min",
        "bulk_density_max",
        "coarse_fragment_vol",
        "coarse_fragment_vol_survey_year",
        "coarse_fragment_vol_min",
        "coarse_fragment_vol_max",
        "part_size_clay",
        "part_size_clay_min",
        "part_size_clay_max",
        "part_size_silt",
        "part_size_silt_min",
        "part_size_silt_max",
        "part_size_sand",
        "part_size_sand_min",
        "part_size_sand_max",
        "horizon_bulk_dens_measure",
        "horizon_bulk_dens_measure_survey_year",
        "horizon_bulk_dens_measure_min",
        "horizon_bulk_dens_measure_max",
        "horizon_bulk_dens_est",
        "horizon_bulk_dens_est_survey_year",
        "horizon_bulk_dens_est_min",
        "horizon_bulk_dens_est_max",
        "coarse_fragment_vol_converted",
        "coarse_fragment_vol_converted_survey_year",
        "coarse_fragment_vol_converted_min",
        "coarse_fragment_vol_converted_max",
        "coarse_fragment_vol_avg",
        "coarse_fragment_vol_avg_survey_year",
        "coarse_fragment_vol_avg_min",
        "coarse_fragment_vol_avg_max",
        "horizon_clay",
        "horizon_clay_min",
        "horizon_clay_max",
        "horizon_silt",
        "horizon_silt_min",
        "horizon_silt_max",
        "horizon_sand",
        "horizon_sand_min",
        "horizon_sand_max",
        "texture_survey_year")

    }

    if (mode == "time_specific_ff_concentrations") {
      possible_parameters <- "organic_carbon_total"
    }

    parameters <- unique(
      possible_parameters[which(possible_parameters %in% names(df2))])

  }


  # Add extra variables to df2 ----

  if (!"layer_thickness" %in% names(df2)) {

    df2 <- df2 %>%
      mutate(layer_thickness = ifelse(
        !is.na(layer_limit_superior) & !is.na(layer_limit_inferior),
        layer_limit_inferior - layer_limit_superior,
        NA))

  }

    # Harmonise coarse fragments

    if ("coarse_fragment_vol_converted" %in% names(df2) &&
        "coarse_fragment_vol_avg" %in% names(df2) &&
        !"coarse_fragment_vol" %in% names(df2)) {

      df2 <- df2 %>%
        mutate(cf = coalesce(coarse_fragment_vol_converted,
                             coarse_fragment_vol_avg),
               cf = ifelse(is.na(cf),
                           0,
                           cf))
    } else
    if ("coarse_fragment_vol" %in% names(df2)) {

      col <- names(df2)[which(
        names(df2) == "coarse_fragment_vol")]

      df2 <- df2 %>%
        mutate(cf = .data[[col]],
               cf = ifelse(is.na(cf),
                           0,
                           cf))

    } else {
      df2 <- df2 %>%
        mutate(cf = 0)
    }


    # Harmonise bulk density

    if ("horizon_bulk_dens_measure" %in% names(df2) &&
        "horizon_bulk_dens_est" %in% names(df2) &&
        !"bulk_density" %in% names(df2)) {

      df2 <- df2 %>%
        mutate(bd = coalesce(horizon_bulk_dens_measure,
                             horizon_bulk_dens_est))

    } else
      if ("bulk_density" %in% names(df2)) {

        col <- names(df2)[which(
          names(df2) == "bulk_density")]

        df2 <- df2 %>%
          mutate(bd = .data[[col]])

      }

      df2 <- df2 %>%
        mutate(bulk_density_total_soil =
                 .data$bd * (1 - 0.01 * .data$cf)) %>%
        select(-bd, -cf)



  # Add new columns to df1 ----

  if (!is.null(prefix_parameters_in_df1)) {

    # Check if the string ends with "_"
    if (!endsWith(prefix_parameters_in_df1, "_")) {
      # If not, add "_" at the end
      prefix_parameters_in_df1 <- paste0(prefix_parameters_in_df1, "_")
    }
  }

  for (i in seq_along(parameters)) {

    col_name <- if (is.null(prefix_parameters_in_df1)) {
      parameters[i]
    } else {
      paste0(prefix_parameters_in_df1, parameters[i])
    }

    assertthat::assert_that(!col_name %in% names(df1))

    df1[[col_name]] <- NA

    if (mode == "most_recent") {

      col_name_year <- paste0(col_name, "_year")
      df1[[col_name_year]] <- NA
    }

  }

  is_pfh <- FALSE

  if (all(c("horizon_master",
            "horizon_limit_up",
            "horizon_limit_low") %in% names(df1))) {

    is_pfh <- TRUE

    df1 <- df1 %>%
      rename(code_layer = horizon_master) %>%
      rename(layer_limit_superior = horizon_limit_up) %>%
      rename(layer_limit_inferior = horizon_limit_low)

  }





  if (!isTRUE(getOption("knitr.in.progress"))) {
    progress_bar <- txtProgressBar(min = 0,
                                   max = nrow(df1),
                                   style = 3)
  }


  # Evaluate for each layer ----

  for (i in seq_len(nrow(df1))) {

    # If redundant layer: go to next

    if (is.na(df1$layer_number[i])) {
      next
    }

    is_ff_i <- (df1$layer_type[i] == "forest_floor" ||
                  (is.na(df1$layer_limit_superior[i]) ||
                     df1$layer_limit_superior[i] < 0))

    # If below-ground layer in time_specific_ff_concentrations mode: go to next

    if (mode == "time_specific_ff_concentrations" &&
        !is_ff_i) {
      next
    }


    plot_id_i <- df1$plot_id[i]
    code_layer_i <- df1$code_layer[i]
    limit_sup_i <- df1$layer_limit_superior[i]
    limit_inf_i <- df1$layer_limit_inferior[i]



    if (!is_ff_i &&
        is.na(limit_inf_i)) {

      assertthat::assert_that(!is.na(limit_sup_i))

      limit_inf_i <- limit_sup_i + 20
    }

    depth_range_i <- NA

    if (!is.na(limit_sup_i) &&
        !is.na(limit_inf_i)) {

      depth_range_i <-
        seq(round(limit_sup_i, 1),
            round(limit_inf_i, 1),
            by = 0.1)
    }


    # Forest floor ----
    # "Equivalent soil mass" approach based on match in horizon (code_layer)

    if (is_ff_i) {

      df2_sub <- df2 %>%
        filter(plot_id == plot_id_i) %>%
        filter(layer_type == "forest_floor" &
                 (is.na(layer_limit_superior) |
                    layer_limit_superior < 0))

      if (nrow(df2_sub) == 0) {
        next # Go to next layer
      }



      if (mode == "time_specific_ff_concentrations") {

        survey_year_i <- df1$survey_year[i]

        df2_sub <- df2_sub %>%
          filter(.data$survey_year >= survey_year_i - 3 &
                   .data$survey_year <= survey_year_i + 3)

        if (nrow(df2_sub) == 0) {
          next # Go to next layer
        }
      }


      # Remove any numbers at the end of code_layer

      code_layer_i <- gsub("\\d+$", "", code_layer_i)

      # Option 1: same layer

      df2_selected <- df2_sub %>%
        filter(code_layer == code_layer_i)

      # Option 2: layer containing the same letters

      if (nrow(df2_selected) == 0) {

        letters <- paste(unique(strsplit(
          gsub("O", "", code_layer_i), "")[[1]]),
          collapse = "|")

        if (letters == "") {
          letters <- "O"
        }

        df2_selected <- df2_sub %>%
          filter(grepl(
            paste0("[", letters, "]"),
            code_layer))

        if (nrow(df2_selected) > 0 &&
            !grepl("L", letters)) {

          df2_selected <- df2_selected %>%
            filter(code_layer != "OL")
        }


        # Option 3: any forest floor layers
        # (If no "L", ignore OL layers)

        if (nrow(df2_selected) == 0) {

          df2_selected <- df2_sub

          if (nrow(df2_selected) > 0 &&
              !grepl("L", letters)) {

            df2_selected <- df2_selected %>%
              filter(code_layer != "OL")
          }
        }
      }

      if (nrow(df2_selected) == 0) {
        next
      }



      ## Evaluate for each of the parameters ----

      for (j in seq_along(parameters)) {

        parameter_j <- parameters[j]

        col_name_j <- if (is.null(prefix_parameters_in_df1)) {
          parameter_j
        } else {
          paste0(prefix_parameters_in_df1, parameter_j)
        }

        if (mode == "most_recent") {

          col_name_year_j <- paste0(col_name, "_year")
          most_recent_j <- NA
        }



        value_j <- NA

        # constant_physical_parameters: Only add bulk density!

        if (mode == "constant_physical_parameters" &&
            !grepl("bulk", parameter_j)) {
          next # Go to next parameter
        }

        df2_j <- df2_selected %>%
          filter(!is.na(.data[[parameter_j]]))

        if (nrow(df2_j) == 0) {
          next # Go to next parameter
        }

        # Most recent: select the most recent survey year

        if (mode == "most_recent") {

          most_recent_j <- max(df2_j$survey_year)

          df2_j <- df2_j %>%
            filter(survey_year == most_recent_j)
        }



        # If categorical variable (i.e. "year")

        if (grepl("year|code", parameter_j)) {

          value_j <- if (length(na.omit(df2_j[[parameter_j]])) == 1) {

            as.numeric(na.omit(df2_j[[parameter_j]]))

          } else {

            if (any(is.na(df2_j$layer_thickness))) {

              # Most abundant value

              as.numeric( names(table(df2_j[[parameter_j]]))[
                which.max(table(df2_j[[parameter_j]]))])

            } else {

              # Value representing biggest depth range

            df2_j %>%
              filter(!is.na(.data[[parameter_j]])) %>%
              group_by(.data[[parameter_j]]) %>%
              reframe(layer_thickness = sum(layer_thickness, na.rm = TRUE)) %>%
              arrange(-layer_thickness) %>%
              slice_head(n = 1) %>%
              pull(.data[[parameter_j]])
            }
          }


        } else {

          # If numeric variable


          # If constant_physical_parameters mode

          if (mode %in% c("constant_physical_parameters")) {

          # Bulk density, so no need to weight it

          value_j <- if (any(is.na(df2_j$layer_thickness))) {
            round(mean(df2_j[[parameter_j]]), 1)
          } else {
            round(weighted.mean(df2_j[[parameter_j]],
                                w = df2_j$layer_thickness), 1)
          }
          }


          # If time_specific_ff_concentrations/most_recent mode

          if (mode %in% c("time_specific_ff_concentrations", "most_recent")) {

            if (nrow(df2_j) == 1) {

              value_j <- round(df2_j[[parameter_j]], 1)
            }

            if (nrow(df2_j) > 1) {

              # Add weights
              # Only for gravimetric parameters

              assertthat::assert_that("organic_layer_weight_bd" %in%
                                        names(df2_j))

              if ("organic_layer_weight" %in% names(df2_j)) {

                df2_j <- df2_j %>%
                  mutate(weights = coalesce(organic_layer_weight,
                                            organic_layer_weight_bd),
                         weights_mean = mean(weights, na.rm = TRUE),
                         weights = case_when(
                           any(!is.na(weights)) & any(is.na(weights)) ~
                             coalesce(weights, weights_mean),
                           TRUE ~ weights),
                         weights_mean = mean(layer_thickness, na.rm = TRUE),
                         weights = case_when(
                           all(is.na(weights)) ~
                             coalesce(layer_thickness, weights_mean),
                           TRUE ~ weights),
                         weights = case_when(
                           all(is.na(weights)) ~ 1,
                           TRUE ~ weights))
              } else {

                df2_j <- df2_j %>%
                  mutate(weights = organic_layer_weight_bd,
                         weights_mean = mean(weights, na.rm = TRUE),
                         weights = case_when(
                           any(!is.na(weights)) & any(is.na(weights)) ~
                             coalesce(weights, weights_mean),
                           TRUE ~ weights),
                         weights_mean = mean(layer_thickness, na.rm = TRUE),
                         weights = case_when(
                           all(is.na(weights)) ~
                             coalesce(layer_thickness, weights_mean),
                           TRUE ~ weights),
                         weights = case_when(
                           all(is.na(weights)) ~ 1,
                           TRUE ~ weights))
              }

              value_j <- round(weighted.mean(df2_j[[parameter_j]],
                                             w = df2_j$weights), 1)

            }
          } # End of "if time_specific_ff_concentrations"/"most_recent"

        } # End of "if numeric"

        df1[[col_name_j]][i] <- value_j

        if (mode == "most_recent") {
          df1[[col_name_year_j]][i] <- most_recent_j
        }


      } # End of evaluation parameters





    } # End of "if forest floor"





    # Below-ground ----
    # Depth-based approach in the assumption that bulk density remains unchanged

    if (!is_ff_i &&
        mode %in% c("constant_physical_parameters", "most_recent")) {

      df2_sub <- df2 %>%
        filter(plot_id == plot_id_i) %>%
        filter(layer_type != "forest_floor") %>%
        filter(!is.na(layer_limit_superior) &
                 layer_limit_superior >= 0)

      if (nrow(df2_sub) == 0) {
        next # Go to next layer
      }

      assertthat::assert_that(
        all(!is.na(df2_sub$layer_limit_superior)))

      if (any(!is.na(df2_sub$layer_limit_inferior))) {

        df2_sub <- df2_sub %>%
          mutate(layer_limit_inferior = ifelse(is.na(layer_limit_inferior),
                                               layer_limit_superior + 20,
                                               layer_limit_inferior))
      }

      df2_sub <- df2_sub %>%
        rowwise() %>%
        filter(any(depth_range_i > .data$layer_limit_superior &
                     depth_range_i < .data$layer_limit_inferior))


      if (nrow(df2_sub) == 0) {
        next # Go to next layer
      }


      ## Evaluate for each of the parameters ----

      for (j in seq_along(parameters)) {

        parameter_j <- parameters[j]

        col_name_j <- if (is.null(prefix_parameters_in_df1)) {
          parameter_j
        } else {
          paste0(prefix_parameters_in_df1, parameter_j)
        }

        if (mode == "most_recent") {

          col_name_year_j <- paste0(col_name, "_year")
          most_recent_j <- NA
        }



        value_j <- NA


        df2_j <- df2_sub %>%
          filter(!is.na(.data[[parameter_j]]))

        if (nrow(df2_j) == 0) {
          next # Go to next parameter
        }

        # Most recent: select the most recent survey year

        if (mode == "most_recent") {

          most_recent_j <- max(df2_j$survey_year)

          df2_j <- df2_j %>%
            filter(survey_year == most_recent_j)
        }


        if (nrow(df2_j) == 1) {

          if (grepl("year|code", parameter_j)) {
            # Categorical
            value_j <- df2_j[[parameter_j]]
          } else {
            value_j <- round(df2_j[[parameter_j]], 1)
          }

        }

        if (nrow(df2_j) > 1) {

        # Add weights
        # Only for gravimetric parameters

        df2_j <- df2_j %>%
          mutate(weights = case_when(
            (!grepl("bulk_dens", parameter_j) &
              !grepl("coarse_fragment_vol", parameter_j)) ~
              .data$bulk_density_total_soil,
            .default = NA))


        # If categorical variable (i.e. "year")

        if (grepl("year|code", parameter_j)) {

          value_j <-
            harmonise_layer_to_depths(limit_sup = limit_sup_i,
                                      limit_inf = limit_inf_i,
                                      bulk_density =
                                        df2_j$weights,
                                      upper_depths =
                                        df2_j$layer_limit_superior,
                                      lower_depths =
                                        df2_j$layer_limit_inferior,
                                      variab =
                                        df2_j[[parameter_j]],
                                      parameter_name =
                                        "parameter_j",
                                      mode = "categorical")

        } else {

          # If numeric variable

          value_j <-
            harmonise_layer_to_depths(limit_sup = limit_sup_i,
                                      limit_inf = limit_inf_i,
                                      bulk_density =
                                        df2_j$weights,
                                      upper_depths =
                                        df2_j$layer_limit_superior,
                                      lower_depths =
                                        df2_j$layer_limit_inferior,
                                      variab =
                                        df2_j[[parameter_j]],
                                      parameter_name =
                                        "parameter_j",
                                      mode = "numeric") %>%
            round(1)
        }

        } # End of "if >1 rows"

        df1[[col_name_j]][i] <- value_j

        if (mode == "most_recent") {
          df1[[col_name_year_j]][i] <- most_recent_j
        }

        } # End of evaluation parameters

    } # End of "if below-ground" (constant_physical_parameters)

    # Update progress bar

    if (!isTRUE(getOption("knitr.in.progress"))) {
      setTxtProgressBar(progress_bar, i)
    }

  } # End of evaluation each layer



  if (!isTRUE(getOption("knitr.in.progress"))) {
    close(progress_bar)
  }



  if (is_pfh == TRUE) {

    df1 <- df1 %>%
      rename(horizon_master = code_layer) %>%
      rename(horizon_limit_up = layer_limit_superior) %>%
      rename(horizon_limit_low = layer_limit_inferior)
  }


return(df1)


}
