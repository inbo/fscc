
#' Harmonise layer towards pre-defined depths
#'
#' This function calculates harmonised values for a specified parameter
#' within a given depth range, by taking a combination of depth range
#' contributions and bulk densities of the original layers as weights.
#' If any bulk density value is missing, it assigns a harmonised weight
#' to all layers to account for missing data.
#'
#' @param limit_sup The upper depth limit of the wanted harmonisation range.
#' @param limit_inf The lower depth limit of the wanted harmonisation range.
#' @param df_sub_selected A data frame containing layer information. Can be
#' NULL, but then the parameters "bulk_density", "upper_depths", "lower_depths"
#' and "variab" should not be NULL.
#' @param bulk_density A vector containing bulk densities or organic layer
#' weights which serve as weights. Should follow the same layer sequence
#' as the parameters "upper_depths", "lower_depths", and "variab".
#' Should be NULL if df_sub_selected is not NULL.
#' @param upper_depths A vector containing the upper layer limits.
#' Should follow the same layer sequence as the parameters "bulk_density",
#' "lower_depths", and "variab". Should be NULL if df_sub_selected is not NULL.
#' @param lower_depths A vector containing the lower layer limits.
#' Should follow the same layer sequence as the parameters "upper_depths",
#' "bulk_density", and "variab". Should be NULL if df_sub_selected is not NULL.
#' @param variab A vector containing the actual parameter values to be
#' weighted. This vector can contain NAs. Should follow the same layer sequence
#' as the parameters "upper_depths", "lower_depths", and "bulk_density".
#' Should be NULL if df_sub_selected is not NULL.
#' @param parameter_name The name of the parameter to harmonise
#' (e.g., "bulk_density").
#' @param mode The mode for calculating harmonised values: "numeric"
#' (uses weights based on bulk density and depth range contributions) or
#' "categorical" (just takes the value for parameter_name with the highest
#' weight, i.e. usually the highest depth range contribution)
#'
#' @return The harmonised value for the specified parameter within the
#' depth range.
#'
#' @details This function computes harmonised values based on the depth
#' range and bulk density information provided in the input data frame.
#' It allows for both numeric and categorical harmonisation modes.
#'
#' @examples
#'
#' harmonise_layer_to_depths(limit_sup = 5, limit_inf = 70,
#'                 df_sub_selected, parameter_name = "bulk_density",
#'                 mode = "numeric")

harmonise_layer_to_depths <- function(limit_sup,
                                      limit_inf,
                                      df_sub_selected = NULL,
                                      bulk_density = NULL,
                                      coarse_fragment_vol_frac = NULL,
                                      upper_depths = NULL,
                                      lower_depths = NULL,
                                      variab = NULL,
                                      parameter_name,
                                      mode = c("numeric",
                                               "categorical")) {

  mode <- match.arg(mode)

  assertthat::assert_that(!is.null(df_sub_selected) ||
                            (!is.null(variab) &&
                               !is.null(upper_depths) &&
                               !is.null(lower_depths)))

  if (!is.null(df_sub_selected)) {

    is_som <- ("layer_limit_superior" %in% names(df_sub_selected) &&
                       "layer_limit_inferior" %in% names(df_sub_selected))

    if (is_som) {

      df_sub_selected <- df_sub_selected %>%
        rename(horizon_limit_up = layer_limit_superior) %>%
        rename(horizon_limit_low = layer_limit_inferior)

    }

  }

  # If different vectors are given instead of a dataframe
  # (this may be useful in a group_by() %>% summarise() construction)

  if (is.null(df_sub_selected)) {

    if (is.null(bulk_density)) {
      df_sub_selected <-
        data.frame(bulk_density = rep(1, length(variab)),
                   horizon_limit_up = upper_depths,
                   horizon_limit_low = lower_depths,
                   variab = variab)
    } else
      if (is.null(coarse_fragment_vol_frac)) {
    df_sub_selected <-
      data.frame(bulk_density = bulk_density,
                 horizon_limit_up = upper_depths,
                 horizon_limit_low = lower_depths,
                 variab = variab)
    } else {
      df_sub_selected <-
        data.frame(bulk_density = bulk_density,
                   coarse_fragment_vol_frac = coarse_fragment_vol_frac,
                   horizon_limit_up = upper_depths,
                   horizon_limit_low = lower_depths,
                   variab = variab)
    }

    if (parameter_name == "bulk_density") {
      df_sub_selected <- df_sub_selected %>%
        select(-bulk_density)
    }

    names(df_sub_selected)[which(names(df_sub_selected) == "variab")] <-
      parameter_name
  }

  # If "coarse_fragment_vol_frag" is unknown but "coarse_fragment_vol"
  # is known:

  if ("coarse_fragment_vol" %in% names(df_sub_selected) &&
      !"coarse_fragment_vol_frac" %in% names(df_sub_selected)) {

    df_sub_selected <- df_sub_selected %>%
      mutate(coarse_fragment_vol_frac = 0.01 * coarse_fragment_vol)
  }


  # If no "bulk_density_total_soil" is present
  # and if "coarse_fragment_vol_frac" is known:
  # Calculate the bulk density of the total soil (not only the fine earth)

  if (parameter_name != "bulk_density" &&
      !"bulk_density_total_soil" %in% names(df_sub_selected) &&
      "coarse_fragment_vol_frac" %in% names(df_sub_selected)) {

    assertthat::assert_that(
      all(na.omit(df_sub_selected$coarse_fragment_vol_frac) <= 1))

    df_sub_selected <- df_sub_selected %>%
      # Account for coarse fragments
      # Assume they are 0 if NA
      mutate(coarse_fragment_vol_frac =
               ifelse(is.na(.data$coarse_fragment_vol_frac),
                      0,
                      .data$coarse_fragment_vol_frac)) %>%
      # Introduce a new variable for bulk density
      # which represents the mass (kg) of fine earth per volume (m3) of total
      # soil (instead of "per volume of fine earth")
      # This is the variable to be used as a weighting factor
      mutate(bulk_density_total_soil =
               ifelse(!is.na(.data$bulk_density) &
                        !is.na(.data$coarse_fragment_vol_frac),
                      .data$bulk_density * (1 - .data$coarse_fragment_vol_frac),
                      NA)) %>%
      mutate(bulk_density = bulk_density_total_soil)
  }

  # If any bulk density value is missing:
  # give all layers relatively the same bulk density weight
  # (i.e. only the thickness contributions of the non-harmonised
  # layers to the fixed layer depth range matter)

  bulk_density_orig <- df_sub_selected$bulk_density

  df_sub_selected <- df_sub_selected %>%
    mutate(bd_gapfilled = ifelse(any(is.na(bulk_density_orig)) ||
                                   parameter_name == "bulk_density",
                                 1,
                                 bulk_density))

  df_sub_selected$weight <- NA
  df_sub_selected$weight_aid <- NA

  # Calculate relative weights for each layer (based on bulk density)

  if (nrow(df_sub_selected) == 1) {
    df_sub_selected$weight_aid <- 1
  }

  if (nrow(df_sub_selected) >= 2) {


    for (l in seq_len(nrow(df_sub_selected))) {

      # Case 1: original depth range too high but not too low
      # (as compared to target limit_sup and limit_inf)

      if (df_sub_selected$horizon_limit_up[l] < limit_sup &&
          !(df_sub_selected$horizon_limit_low[l] > limit_inf)) {

        df_sub_selected$weight_aid[l] <-
          diff(c(limit_sup, df_sub_selected$horizon_limit_low[l])) *
          df_sub_selected$bd_gapfilled[l]

      }

      # Case 2: original depth range too low but not too high
      # (as compared to target limit_sup and limit_inf)

      if (!(df_sub_selected$horizon_limit_up[l] < limit_sup) &&
          df_sub_selected$horizon_limit_low[l] > limit_inf) {

        df_sub_selected$weight_aid[l] <-
          diff(c(df_sub_selected$horizon_limit_up[l], limit_inf)) *
          df_sub_selected$bd_gapfilled[l]
      }

      # Case 3: original depth range too low and too high
      # (as compared to target limit_sup and limit_inf)

      if (df_sub_selected$horizon_limit_up[l] < limit_sup &&
          df_sub_selected$horizon_limit_low[l] > limit_inf) {

        df_sub_selected$weight_aid[l] <-
          diff(c(limit_sup, limit_inf)) *
          df_sub_selected$bd_gapfilled[l]
      }

      # Case 4: original depth range not too low and not too high
      # (as compared to target limit_sup and limit_inf)

      if (!(df_sub_selected$horizon_limit_up[l] < limit_sup) &&
          !(df_sub_selected$horizon_limit_low[l] > limit_inf)) {

        df_sub_selected$weight_aid[l] <-
          diff(c(df_sub_selected$horizon_limit_up[l],
                 df_sub_selected$horizon_limit_low[l])) *
          df_sub_selected$bd_gapfilled[l]
      }
    }
  }

  df_sub_selected <- df_sub_selected %>%
    filter(!is.na(.data[[parameter_name]]))

  if (nrow(df_sub_selected) == 0) {
    result <- NA
  } else {

    weight_sum <- sum(df_sub_selected$weight_aid)

    df_sub_selected <- df_sub_selected %>%
      mutate(weight = .data$weight_aid / weight_sum)

    # Calculate the final value

    # If numeric: by taking the sum of the product

    if (mode == "numeric") {

      result <- sum(df_sub_selected[[parameter_name]] * df_sub_selected$weight)

    } else

      if (mode == "categorical") {

        result <- df_sub_selected %>%
          # filter(!is.na(.data[[parameter_name]])) %>%
          arrange(desc(weight)) %>%
          head(1) %>%
          pull(.data[[parameter_name]])
      }
  }

  return(round(result, 2))

}
