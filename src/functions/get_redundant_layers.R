

#' Get redundant layers in profile
#'
#' This function analyses, for a given soil profile, the depths covered by
#' each soil layer/horizon, to figure out whether there are any layers/horizons
#' which can be considered redundant because of covering the same depths as
#' other layers/horizons of the given profile.
#'
#' @param layers Character string - this contains the unique horizon_master
#' values for records with available layer limit information in a given
#' profile. 
#' In case horizon_master values of the given profile are not unique,
#' attach a unique number.
#' For example, if 'vec' are the row indices of data frame 'df' for a
#' given profile:
#'
#'     vec_nonempty <- vec[which(!is.na(df$horizon_limit_up[vec]) &
#'     !is.na(df$horizon_limit_low[vec]))]
#'     layers <- df$horizon_master[vec_nonempty]
#'     if (length(unique(df$horizon_master[vec_nonempty])) <
#'     length(df$horizon_master[vec_nonempty])) {
#'     layers <- paste0(df$horizon_master[vec_nonempty], 1:length(layers))
#'     }
#'
#' @param superior_layer_limits Numeric vector - this contains the superior
#' layer limits for the same records with available layer limit information
#' in the given profile.
#'
#' superior_layer_limits <- df$horizon_limit_up[vec_nonempty]
#'
#' @param inferior_layer_limits Numeric vector - this contains the inferior
#' layer limits for the same records with available layer limit information
#' in the given profile.
#'
#' inferior_layer_limits <- df$horizon_limit_low[vec_nonempty]
#'
#' @param df_sub Data.frame - this data frame is a subset of the rows with
#' available layer limit information in the given profile.
#'
#' df_sub <- df[vec_nonempty, ]
#'
#' @details
#' This function as custom-made for the 'pfh' survey forms (with pedogenetic
#' horizons).
#' However, with minimal adjustments, it should also be applicable for
#' 'som' survey forms (with fixed-depth layers).
#'
#' This function identifies any layers/horizons which cover the same depths like
#' other layers from the same profile and can therefore be left out without
#' any effect on the depths which are covered by the data of the given profile.
#' If there are multiple options when identifying these redundant layers
#' (e.g. "M01" versus "M05" + "M51"): consider the least 'detailed' layer(s)
#' (i.e. layer(s) with largest depth range) as redundant (e.g. "M01").
#'
#' The following types of redundancy are distinguished:
#' - redundancy_type 1: this individual layer can be left out without any effect
#'                      on the depths covered by the remaining layers
#' - redundancy_type 2: this layer can also be left out along with another layer
#'                      without any effect on the depths covered by the
#'                      remaining layers (i.e. excluding these two layers).
#'                      The other 'co_redundant' layer is given under
#'                      'combined_layer'.
#' - redundancy_type 3: this layer can also be left out along with two other
#'                      layers without any effect on the depths covered by
#'                      the remaining layers (i.e. excluding these three
#'                      layers). The other two 'co_redundant' layers are given
#'                      under 'combined_layer'.
#' - redundancy_type 0: redundant layer/layer combination with priority to be
#'                      left out if one redundant layer (combination) has to
#'                      be selected. In case of multiple options:
#'                      redundancy_type 0 refers to
#'                      the layer (combination) which is the least 'detailed',
#'                      i.e. for which the individual layer(s) on average span
#'                      the largest depth range.
#'
#' WARNING - This function may not be optimally efficient and may ideally
#' require refactoring for better performance.
#'
#' @return A data.frame with three rows:
#' 'layer' refers to the 'layers' which were given as input argument;
#' 'redundancy_type' indicates the redundancy type of the given layers;
#' 'combined_layer' indicates the 'co-redundant layer' for layers with
#'  redundancy type 2
#'
#' @examples
#' redundant_layers <- get_redundant_layers(layers = layers,
#'     superior_layer_limits = superior_layer_limits,
#'     inferior_layer_limits = inferior_layer_limits,
#'     df_sub = df_sub)
#'

get_redundant_layers <- function(layers,
                                 superior_layer_limits,
                                 inferior_layer_limits,
                                 df_sub) {


depth_layers <- data.frame(
  layer = layers,
  upper = superior_layer_limits,
  lower = inferior_layer_limits)




# depth ranges for all layers combined ----

depth_ranges_total <- NULL

for (i in seq_len(nrow(depth_layers))) {
  if (depth_layers$lower[i] > depth_layers$upper[i]) {
    increment <- (-0.1)
    } else {
      increment <- 0.1
    }

  depth_ranges_total <- c(depth_ranges_total,
                          seq(depth_layers$lower[i],
                              depth_layers$upper[i],
                              by = increment))
  }

depth_ranges_total <- unique(round(sort(depth_ranges_total), digits = 1))





# identifying individual layers that can be left out (i.e. redundant layers):
# redundancy_type is 1 ----

redundant_layers <- data.frame(layer = NULL,
                               redundancy_type = NULL,
                               combined_layer = NULL)

for (i in seq_len(nrow(depth_layers))) {

  depth_layers_sub <- depth_layers[-i, ]
  depth_ranges_sub <- NULL

    for (j in seq_len(nrow(depth_layers_sub))) {
      if (depth_layers_sub$lower[j] > depth_layers_sub$upper[j]) {
        increment <- (-0.1)
      } else {
          increment <- 0.1
          }

      depth_ranges_sub <- c(depth_ranges_sub,
                            seq(depth_layers_sub$lower[j],
                                depth_layers_sub$upper[j],
                                by = increment))
      }

  depth_ranges_sub <- (unique(round(sort(depth_ranges_sub), digits = 1)))

  if (length(depth_ranges_total) == length(depth_ranges_sub)) {

    if (all(depth_ranges_total == depth_ranges_sub)) {
    redundant_layers <- rbind(redundant_layers,
                              data.frame(layer = depth_layers$layer[i],
                                         redundancy_type = 1,
                                         combined_layer = NA))
    }
  }
  }







if (nrow(redundant_layers) > 0) {

  lengths <- rep(NA, nrow(redundant_layers))

  for (i in seq_len(nrow(redundant_layers))) {
    ind <- which(depth_layers$layer == redundant_layers$layer[i])
    lengths[i] <- diff(c(depth_layers$upper[ind], depth_layers$lower[ind]))
    }

  if (any(lengths == 0.5)) {

    redundant_layers <- data.frame(layer = NULL,
                                   redundancy_type = NULL,
                                   combined_layer = NULL)

    # depth ranges for all layers combined
    depth_ranges_total_05 <- NULL

    for (i in seq_len(nrow(depth_layers))) {

      if (depth_layers$lower[i] > depth_layers$upper[i]) {
        increment <- (-0.05)
      } else {
          increment <- 0.05
          }

      depth_ranges_total_05 <- c(depth_ranges_total_05,
                              seq(depth_layers$lower[i],
                                  depth_layers$upper[i],
                                  by = increment))
      }

    depth_ranges_total_05 <-
      unique(round(sort(depth_ranges_total_05), digits = 1))

    
    # redundant layer type 1
    for (i in seq_len(nrow(depth_layers))) {

      depth_layers_sub <- depth_layers[-i, ]
      depth_ranges_sub <- NULL

      for (j in 1:nrow(depth_layers_sub)) {

        if (depth_layers_sub$lower[j] > depth_layers_sub$upper[j]) {
          increment <- (-0.05)
        } else {
            increment <- 0.05
            }

        depth_ranges_sub <- c(depth_ranges_sub,
                              seq(depth_layers_sub$lower[j],
                                  depth_layers_sub$upper[j],
                                  by = increment))
        }

      depth_ranges_sub <- (unique(round(sort(depth_ranges_sub), digits = 1)))

      if (length(depth_ranges_total_05) == length(depth_ranges_sub)) {

        if (all(depth_ranges_total_05 == depth_ranges_sub)) {
          redundant_layers <- rbind(redundant_layers,
                                    data.frame(layer = depth_layers$layer[i],
                                               redundancy_type = 1,
                                               combined_layer = NA))
        }
      }
    }
  }
  }

redundant_layers_1 <- redundant_layers






# Check whether the number of redundant layers is similar to nrow ----
# such as in partner 2704, plot 1202, survey year 2010

if ((nrow(redundant_layers) >= 0.6 * nrow(depth_layers)) &&
    (nrow(depth_layers) >= 5)) {

  redundant_layers <- data.frame(layer = NULL,
                                 redundancy_type = NULL,
                                 combined_layer = NULL)
  depth_layers <- cbind(depth_layers,
                        number_of_non_nas = rowSums(is.na(df_sub)))
  non_na_range <- unique(sort(depth_layers$number_of_non_nas))

  i <- 1
  vec_sub <- NULL

  for (j in 1:i) {
    vec_sub <- c(vec_sub,
                 which(depth_layers$number_of_non_nas == non_na_range[j]))
    }

  depth_ranges_sub_search <- NULL

  for (j in vec_sub) {

    if (depth_layers$lower[j] > depth_layers$upper[j]) {
      increment <- (-0.1)
    } else {
        increment <- 0.1
    }

    depth_ranges_sub_search <- c(depth_ranges_sub_search,
                                  seq(depth_layers$lower[j],
                                      depth_layers$upper[j],
                                      by = increment))
    }

  depth_ranges_sub_search <-
    unique(round(sort(depth_ranges_sub_search), digits = 1))

  while ((length(depth_ranges_sub_search) <
          0.95 * length(depth_ranges_total)) ||
         (any(diff(depth_ranges_sub_search) > 0.15))) {

    i <- i + 1
    vec_sub <- NULL

    for (j in 1:i) {
      vec_sub <- c(vec_sub,
                   which(depth_layers$number_of_non_nas == non_na_range[j]))
      }

    depth_ranges_sub_search <- NULL

    for (j in vec_sub) {

      if (depth_layers$lower[j] > depth_layers$upper[j]) {
        increment <- (-0.1)
        } else {
          increment <- 0.1
        }

      depth_ranges_sub_search <- c(depth_ranges_sub_search,
                                   seq(depth_layers$lower[j],
                                       depth_layers$upper[j],
                                       by = increment))
      }

    depth_ranges_sub_search <-
      unique(round(sort(depth_ranges_sub_search), digits = 1))
    }

  # If there is a part of the depth range that is not spanned with this unique
  # subselection,
  # we can search whether any of the remaining layers covers this depth range

  if (!identical(depth_ranges_total[!depth_ranges_total %in%
                                    depth_ranges_sub_search],
                 numeric(0))) {

    depth_range_missing <-
      depth_ranges_total[!depth_ranges_total %in% depth_ranges_sub_search]

    if ((abs(depth_range_missing[1]) -
         floor(abs(depth_range_missing[1]))) > 1e-10) {

      sign <- abs(depth_range_missing[1]) / depth_range_missing[1]

      decimal <-
        round((abs(depth_range_missing[1]) -
                 floor(abs(depth_range_missing[1]))),
              digits = 1)

      # if it decreases
      if ((sign == (-1) &&
         ((decimal == 0.1) || (decimal == 0.6))) ||
         ((sign == 1) &&
         ((decimal == 0.9) || (decimal == 0.4)))) {
        depth_range_missing <-
          c(depth_range_missing[1] + 0.1,depth_range_missing)
        }

      # if it increases
      if ((sign == (-1) &&
         ((decimal == 0.9) || (decimal == 0.4))) ||
         ((sign == 1) &&
         ((decimal == 0.1) || (decimal == 0.6)))) {

        depth_range_missing <-
          c(depth_range_missing[1] - 0.1,depth_range_missing)
      }
      }

    if ((abs(depth_range_missing[length(depth_range_missing)]) -
      floor(abs(depth_range_missing[length(depth_range_missing)]))) > 1e-10) {

      sign <-
        abs(depth_range_missing[length(depth_range_missing)]) /
        depth_range_missing[length(depth_range_missing)]

      decimal <-
        round((abs(depth_range_missing[length(depth_range_missing)]) -
                 floor(abs(depth_range_missing[length(depth_range_missing)]))),
              digits = 1)

      # if it decreases
      if ((sign == (-1) &&
         ((decimal == 0.9) || (decimal == 0.4))) ||
         ((sign == 1) &&
         ((decimal == 0.1) || (decimal == 0.6)))) {

        depth_range_missing <-
          c(depth_range_missing,
            depth_range_missing[
              depth_range_missing[length(depth_range_missing)]] - 0.1)
        }

      # if it increases
      if ((sign == (-1) &&
         ((decimal == 0.1) || (decimal == 0.6))) ||
         ((sign == 1) &&
         ((decimal == 0.9) || (decimal == 0.4)))) {

        depth_range_missing <-
          c(depth_range_missing,
            depth_range_missing[
              depth_range_missing[length(depth_range_missing)]] + 0.1)
      }
      }

    vec_redundant <-
      (seq_len(nrow(depth_layers)))[!(seq_len(nrow(depth_layers)) %in% vec_sub)]

    ind_extra <- NULL
    for (i in vec_redundant) {

      if (((depth_layers$upper[i] == depth_range_missing[1]) ||
           (depth_layers$lower[i] == depth_range_missing[1])) &&
          ((depth_layers$upper[i] ==
            depth_range_missing[length(depth_range_missing)]) ||
           (depth_layers$lower[i] ==
            depth_range_missing[length(depth_range_missing)]))) {

        ind_extra <- i
      }
      }

    if (!is.null(ind_extra)) {
      vec_sub <- c(vec_sub, ind_extra)
    }
    }


  vec_redundant <-
    (seq_len(nrow(depth_layers)))[!(seq_len(nrow(depth_layers)) %in% vec_sub)]

  if (!identical(vec_redundant, integer(0))) {

    redundant_layers <-
      rbind(redundant_layers,
            data.frame(layer = depth_layers$layer[vec_redundant],
                       redundancy_type = 0,
                       combined_layer = NA))

    depth_layers <- data.frame(
    layer = layers[vec_sub],
    upper = superior_layer_limits[vec_sub],
    lower = inferior_layer_limits[vec_sub])
  }

  # identifying individual layers that can be left out
  # (i.e. redundant layers):
  # redundancy_type is 1

  for (i in seq_len(nrow(depth_layers))) {

    depth_layers_sub <- depth_layers[-i, ]
    depth_ranges_sub <- NULL

    for (j in seq_len(nrow(depth_layers_sub))) {
      if (depth_layers_sub$lower[j] > depth_layers_sub$upper[j]) {
        increment <- (-0.1)
        } else {
          increment <- 0.1
        }

      depth_ranges_sub <- c(depth_ranges_sub,
                            seq(depth_layers_sub$lower[j],
                                depth_layers_sub$upper[j],
                                by = increment))
    }

    depth_ranges_sub <- (unique(round(sort(depth_ranges_sub), digits = 1)))

    if (length(depth_ranges_total) == length(depth_ranges_sub)) {

      if (all(depth_ranges_total == depth_ranges_sub)) {
        redundant_layers <- rbind(redundant_layers,
                                  data.frame(layer = depth_layers$layer[i],
                                             redundancy_type = 1,
                                             combined_layer = NA))
      }
    }
    }

  redundant_layers_1 <-
    redundant_layers[which(redundant_layers$redundancy_type == 1), ]
  }







# identifying combinations of two layers that can be left out
# (i.e. redundant layers):
# redundancy_type is 2 ----

if ((nrow(redundant_layers_1) >= 2)) {

  rows <- seq_len(nrow(redundant_layers_1))

  for (i in rows[1:(length(rows) - 1)]) {

    for (j in (i + 1):length(rows)) {
      layer_1 <- redundant_layers_1$layer[i]
      layer_2 <- redundant_layers_1$layer[j]
      ind_1 <- which(depth_layers$layer == layer_1)
      ind_2 <- which(depth_layers$layer == layer_2)
      depth_layers_sub <- depth_layers[-c(ind_1,ind_2), ]
      depth_ranges_sub <- NULL

      for (k in seq_len(nrow(depth_layers_sub))) {

        if (depth_layers_sub$lower[k] > depth_layers_sub$upper[k]) {
          increment <- (-0.1)
        } else {
            increment <- 0.1
            }

        depth_ranges_sub <- c(depth_ranges_sub,
                              seq(depth_layers_sub$lower[k],
                                  depth_layers_sub$upper[k],
                                  by = increment))
      }

      depth_ranges_sub <- (unique(round(sort(depth_ranges_sub), digits = 1)))

      if (length(depth_ranges_total) == length(depth_ranges_sub)) {
        if (all(depth_ranges_total == depth_ranges_sub)) {

          redundant_layers <- rbind(redundant_layers,
                                    data.frame(layer = layer_1,
                                               redundancy_type = 2,
                                               combined_layer = layer_2),
                                    data.frame(layer = layer_2,
                                               redundancy_type = 2,
                                               combined_layer = layer_1))
        }
      }
    }
  }
  }




# identifying combinations of three layers that can be left out
# (i.e. redundant layers):
# redundancy_type is 3 ----

if ((nrow(redundant_layers[
  which(redundant_layers$redundancy_type == 2), ]) >= 3)) {

  layers_2_unique <- unique(redundant_layers$layer[
    which(redundant_layers$redundancy_type == 2)])
  combn_result <- combn(layers_2_unique, 3)
  combinations <- seq_len(ncol(combn_result))

  for (i in combinations) {

    ind_1 <- which(depth_layers$layer == combn_result[1, i])
    ind_2 <- which(depth_layers$layer == combn_result[2, i])
    ind_3 <- which(depth_layers$layer == combn_result[3, i])
    depth_layers_sub <- depth_layers[-c(ind_1, ind_2, ind_3), ]
    depth_ranges_sub <- NULL

    for (k in seq_len(nrow(depth_layers_sub))) {

      if (depth_layers_sub$lower[k] > depth_layers_sub$upper[k]) {
        increment <- (-0.1)
        } else {
          increment <- 0.1
        }

      depth_ranges_sub <- c(depth_ranges_sub,
                            seq(depth_layers_sub$lower[k],
                                depth_layers_sub$upper[k],
                                by = increment))
      }

    depth_ranges_sub <- (unique(round(sort(depth_ranges_sub), digits = 1)))

    if (length(depth_ranges_total) == length(depth_ranges_sub)) {
      if (all(depth_ranges_total == depth_ranges_sub)) {

        redundant_layers <-
          rbind(redundant_layers,
                data.frame(layer = depth_layers$layer[ind_1],
                           redundancy_type = 3,
                           combined_layer = paste0(depth_layers$layer[ind_2],
                                                   "_",
                                                   depth_layers$layer[ind_3])),
                data.frame(layer = depth_layers$layer[ind_2],
                           redundancy_type = 3,
                           combined_layer = paste0(depth_layers$layer[ind_3],
                                                   "_",
                                                   depth_layers$layer[ind_1])),
                data.frame(layer = depth_layers$layer[ind_3],
                           redundancy_type = 3,
                           combined_layer = paste0(depth_layers$layer[ind_1],
                                                   "_",
                                                   depth_layers$layer[ind_2])))
      }
    }
  }
  }




# in case redundant combinations of two layers exist:
# check whether there are individual layers that are redundant...
# ...and that are not mentioned in the redundant two combined layers list
# since such individual layer probably overlaps with (some of the) ...
# ...redundant two combined layers and spans a bigger depth range,
# ...because of which it is less informative than two more detailed layers
# The objective is to exclude such a layer from the analysis.
# redundancy_type = 0

if ((!identical(which(redundant_layers$redundancy_type == 1 |
                     redundant_layers$redundancy_type == 2 |
                     redundant_layers$redundancy_type == 3),
               integer(0)))) {


  redundant_layers_3 <- NA
  redundant_layers_2 <- NA
  redundant_layers_1 <- NA

redundant_layers_3 <-
  redundant_layers[which(redundant_layers$redundancy_type == 3), ]

redundant_layers_2 <-
  redundant_layers[which(redundant_layers$redundancy_type == 2), ]
redundant_layers_2 <-
  redundant_layers_2[
    which(!redundant_layers_2$layer %in% redundant_layers_3$layer), ]

redundant_layers_1 <-
  redundant_layers[which(redundant_layers$redundancy_type == 1), ]
redundant_layers_1 <-
  redundant_layers_1[
    which(!redundant_layers_1$layer %in% c(redundant_layers_2$layer,
                                           redundant_layers_3$layer)), ]

redundant_layers_short <-
  rbind(redundant_layers_1, redundant_layers_2, redundant_layers_3)
redundant_layers_short$length <- NA
redundant_layers_short$horizon_limit_up <- NA
redundant_layers_short$horizon_limit_low <- NA

for (i in seq_len(nrow(redundant_layers_short))) {
  ind <- which(depth_layers$layer == redundant_layers_short$layer[i])
  length <- diff(c(depth_layers$upper[ind], depth_layers$lower[ind]))

  if (redundant_layers_short$redundancy_type[i] == 2) {

    ind_2 <- which(depth_layers$layer ==
                     redundant_layers_short$combined_layer[i])
    length <- length +
              diff(c(depth_layers$upper[ind_2], depth_layers$lower[ind_2]))
    }

  if (redundant_layers_short$redundancy_type[i] == 3) {

    ind_2_3 <-
      c(which(depth_layers$layer ==
                (unlist(str_split(
                  redundant_layers_short$combined_layer[i], "_"))[1])),
        which(depth_layers$layer ==
                (unlist(str_split(
                  redundant_layers_short$combined_layer[i], "_"))[2])))
    length <-
      length +
      diff(c(depth_layers$upper[ind_2_3[1]], depth_layers$lower[ind_2_3[1]])) +
      diff(c(depth_layers$upper[ind_2_3[2]], depth_layers$lower[ind_2_3[2]]))

    }

  redundant_layers_short$length[i] <- length
  redundant_layers_short$horizon_limit_up[i] <- depth_layers$upper[ind]
  redundant_layers_short$horizon_limit_low[i] <- depth_layers$lower[ind]
  }

ind_max <- which(redundant_layers_short$length ==
                   max(redundant_layers_short$length))

if ((length(ind_max) == 1)) {
  redundant_layers <-
    rbind(redundant_layers,
          data.frame(layer = redundant_layers_short$layer[ind_max],
                     redundancy_type = 0,
                     combined_layer = redundant_layers_short$combined_layer[
                       ind_max]))

} else if ((length(ind_max) == 2) &&
           (any(redundant_layers_short$redundancy_type[ind_max] == 2)) &&
           (redundant_layers_short$layer[ind_max[1]] ==
            redundant_layers_short$combined_layer[ind_max[2]]) &&
           (redundant_layers_short$layer[ind_max[2]] ==
            redundant_layers_short$combined_layer[ind_max[1]])) {

  redundant_layers <-
    rbind(redundant_layers,
          data.frame(layer = redundant_layers_short$layer[ind_max[1]],
                     redundancy_type = 0,
                     combined_layer = redundant_layers_short$combined_layer[
                       ind_max[1]]),
          data.frame(layer = redundant_layers_short$layer[ind_max[2]],
                     redundancy_type = 0,
                     combined_layer = redundant_layers_short$combined_layer
                     [ind_max[2]]))


} else if ((length(ind_max) == 3) &&
           (all(redundant_layers_short$redundancy_type[ind_max] == 3)) &&
           (length(unique(c(as.character(redundant_layers_short$layer[ind_max]),
                      as.vector(stringr::str_split(
                        redundant_layers_short$combined_layer[ind_max[1]],
                                                   "_", simplify = TRUE)),
                      as.vector(stringr::str_split(
                        redundant_layers_short$combined_layer[ind_max[2]],
                                                   "_", simplify = TRUE)),
                      as.vector(stringr::str_split(
                        redundant_layers_short$combined_layer[ind_max[3]],
                                            "_", simplify = TRUE))))) == 3)) {

  redundant_layers <-
    rbind(redundant_layers,
          data.frame(layer = redundant_layers_short$layer[ind_max[1]],
                     redundancy_type = 0,
                     combined_layer = redundant_layers_short$combined_layer[
                       ind_max[1]]),
          data.frame(layer = redundant_layers_short$layer[ind_max[2]],
                     redundancy_type = 0,
                     combined_layer = redundant_layers_short$combined_layer[
                       ind_max[2]]),
          data.frame(layer = redundant_layers_short$layer[ind_max[3]],
                     redundancy_type = 0,
                     combined_layer = redundant_layers_short$combined_layer[
                       ind_max[3]]))

} else if ((length(ind_max) == 2) &&
           (all(redundant_layers_short$redundancy_type[ind_max] == 1)) &&
           (redundant_layers_short$horizon_limit_up[ind_max[1]] ==
            redundant_layers_short$horizon_limit_up[ind_max[2]]) &&
           (redundant_layers_short$horizon_limit_low[ind_max[1]] ==
            redundant_layers_short$horizon_limit_low[ind_max[2]])) {
  redundant_layers <- 
    rbind(redundant_layers,
          data.frame(layer = redundant_layers_short$layer[ind_max[2]],
                     redundancy_type = 0,
                     combined_layer = redundant_layers_short$combined_layer[
                       ind_max[2]]))

  
} else if ((length(ind_max) == 3) &&
           (any(redundant_layers_short$redundancy_type[ind_max] == 1)) &&
           (any(redundant_layers_short$redundancy_type[ind_max] == 2))) {
  redundant_layers <-
    rbind(redundant_layers,
          data.frame(layer = redundant_layers_short$layer[
                       ind_max[which(
                         redundant_layers_short$redundancy_type[
                           ind_max] == 1)]],
                     redundancy_type = 0,
                     combined_layer = redundant_layers_short$combined_layer[
                       ind_max[which(redundant_layers_short$redundancy_type[
                         ind_max] == 1)]]))

} else if ((length(ind_max) > 3) &&
           all(redundant_layers_short$length[ind_max] == 0)) {

  redundant_layers_1 <- redundant_layers[
    which(redundant_layers$redundancy_type == 1), ]

  redundant_layers_1 <- redundant_layers_1[
    which(redundant_layers_1$layer %in%
            unique(redundant_layers_short$layer[ind_max])), ]

  redundant_layers <- rbind(redundant_layers,
                            data.frame(layer = redundant_layers_1$layer,
                                       redundancy_type = 0,
                                       combined_layer =
                                         redundant_layers_1$combined_layer))
} else {
    warning("ind_max longer than 3")
  }
}

if ((nrow(redundant_layers) >= 1 &&
    identical(which(redundant_layers$redundancy_type == 0), integer(0)))) {
  warning(paste0("no redundancy type 0 found for ",superior_layer_limits))
  }


return(redundant_layers)

}
