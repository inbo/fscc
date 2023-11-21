
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
                                      upper_depths = NULL,
                                      lower_depths = NULL,
                                      variab = NULL,
                                      parameter_name,
                                      mode = c("numeric",
                                               "categorical")) {
  
  mode <- match.arg(mode)
  
  assertthat::assert_that(!is.null(df_sub_selected) ||
                            (!is.null(bulk_density) &&
                               !is.null(variab) &&
                               !is.null(upper_depths) &&
                               !is.null(lower_depths)))
  
  # If different vectors are given instead of a dataframe
  # (this may be useful in a group_by() %>% summarise() construction)
  
  if (is.null(df_sub_selected)) {
    
    df_sub_selected <-
      data.frame(bulk_density = bulk_density,
                 horizon_limit_up = upper_depths,
                 horizon_limit_low = lower_depths,
                 variab = variab)
    
    names(df_sub_selected)[which(names(df_sub_selected) == "variab")] <-
      parameter_name
  }
  
  
  # If any bulk density value is missing:
  # give all layers relatively the same bulk density weight
  # (i.e. only the thickness contributions of the non-harmonised
  # layers to the fixed layer depth range matter)
  
  bulk_density_orig <- df_sub_selected$bulk_density
  
  df_sub_selected <- df_sub_selected %>%
    mutate(bd_gapfilled = ifelse(any(is.na(bulk_density_orig)),
                                 1,
                                 bulk_density))
  
  df_sub_selected$weight <- NA
  df_sub_selected$weight_aid <- NA
  
  # Calculate relative weights for each layer (based on bulk density)
  
  if (nrow(df_sub_selected) == 1) {
    df_sub_selected$weight_aid <- 1
  }
  
  if (nrow(df_sub_selected) >= 2) {
    
    ind_sup <-
      which(df_sub_selected$horizon_limit_up ==
              min(df_sub_selected$horizon_limit_up))
    
    ind_inf <-
      which(df_sub_selected$horizon_limit_low ==
              max(df_sub_selected$horizon_limit_low))
    
    
    for (l in seq_len(nrow(df_sub_selected))) {
      
      # Superior layer
      
      if (l == ind_sup) {
        
        df_sub_selected$weight_aid[l] <- 
          diff(c(limit_sup, df_sub_selected$horizon_limit_low[l])) *
          df_sub_selected$bd_gapfilled[l]
        
      } else
        
        # Inferior layer
        
        if (l == ind_inf) {
          
          df_sub_selected$weight_aid[l] <- 
            diff(c(df_sub_selected$horizon_limit_up[l], limit_inf)) *
            df_sub_selected$bd_gapfilled[l]
          
          # Layers in between
        } else {
          
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
  
  return(result)
  
}
