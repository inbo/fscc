

#' Summarise profile data over adjacent layer types
#'
#' This function summarizes profile data based on layer depth and layer type.
#'
#' @param df_profile The profile data as a dataframe.
#'
#' @return A tibble with summarized profile data per layer type.
#'
#' @details This function takes profile data,
#' groups layers adjacent to each other and with the same layer type together,
#' and summarises the total thickness, superior and 
#' inferior limits of each group of layers.
#'
#' @export
#'
#' @examples
#' # Load the package or function before running examples
#' summarise_profile_per_depth_layer_type(df)


summarise_profile_per_depth_layer_type <- function(df_profile) {
  
  is_pfh <- FALSE
  
  # if "pfh"
  
  if ("horizon_limit_up" %in% names(df_profile) &&
      "horizon_limit_low" %in% names(df_profile)) {
    
    is_pfh <- TRUE
    
    df_profile <- df_profile %>%
      rename(layer_limit_superior = horizon_limit_up) %>%
      rename(layer_limit_inferior = horizon_limit_low) %>%
      rename(unique_survey_repetition = unique_survey_profile) %>%
      rename(code_layer = horizon_master)
    
  }
  
  
  if ((!"layer_number" %in% names(df_profile)) |
      all(is.na(df_profile$layer_number))) {
    
    if (any(is.na(df_profile$layer_limit_superior)) ||
        any(is.na(df_profile$layer_limit_inferior))) {
      
      # Add theoretical layer limits
      
      d_depth_level_soil <-
        read.csv("./data/additional_data/d_depth_level_soil.csv",
                 sep = ";")
      
      df_profile <- df_profile %>%
        left_join(d_depth_level_soil %>%
                    select(code,
                           layer_limit_superior,
                           layer_limit_inferior) %>%
                    rename(layer_limit_superior_theory =
                             layer_limit_superior) %>%
                    rename(layer_limit_inferior_theory =
                             layer_limit_inferior),
                  by = join_by(code_layer == code)) %>%
        mutate(layer_limit_superior =
                 ifelse(!is.na(.data$layer_limit_superior),
                         .data$layer_limit_superior,
                         .data$layer_limit_superior_theory)) %>%
        mutate(layer_limit_inferior =
                 ifelse(!is.na(.data$layer_limit_inferior),
                        .data$layer_limit_inferior,
                        .data$layer_limit_inferior_theory))
      
    }
    
    df_profile <- df_profile %>%
      # A quick preliminary way to sort the available layers
      mutate(layer_number =
               rank(ifelse(.data$layer_type ==
                             "forest_floor" &
                             (is.na(.data$layer_limit_inferior) |
                                is.na(.data$layer_limit_superior)),
                           -1,
                           .data$layer_limit_superior),
                    na.last = "keep"))
  }
  
  
  assertthat::assert_that(
    length(unique(df_profile$unique_survey_repetition)) == 1,
    msg = "More than one profile has been given as input")
  
  
  # df_profile %>%
  #   select(code_layer,
  #          layer_type,
  #          layer_number,
  #          layer_limit_superior,
  #          layer_limit_inferior)
  
  df_sub <- df_profile %>%
    ungroup() %>%
    mutate(layer_thickness = ifelse(!is.na(layer_limit_superior) &
                                      !is.na(layer_limit_inferior),
                                    abs(.data$layer_limit_superior -
                                          .data$layer_limit_inferior),
                                    NA)) %>%
    filter(!is.na(layer_number)) %>%
    arrange(layer_number) %>%
    select(layer_number,
           code_layer,
           layer_limit_superior,
           layer_limit_inferior,
           layer_thickness,
           layer_type) %>%
    # Summarise the data based on the layer_type column and calculate the
    # total layer_thickness for contiguous rows having the same layer_type.
    mutate(grp =
             cumsum(layer_type != lag(layer_type,
                                      default = first(layer_type)))) %>%
    group_by(grp) %>%
    summarise(layer_type = first(layer_type),
              total_thickness =
                if(all(is.na(layer_thickness))) NA_real_ else
                  sum(layer_thickness, na.rm = TRUE),
              layer_limit_superior =
                if(all(is.na(layer_limit_superior))) NA_real_ else
                  min(layer_limit_superior, na.rm = TRUE),
              layer_limit_inferior =
                if(all(is.na(layer_limit_inferior))) NA_real_ else
                  max(layer_limit_inferior, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-grp) %>%
    filter(.data$total_thickness > 0)
  
  
  
  if (is_pfh) {
    
    df_sub <- df_sub %>%
      rename(horizon_limit_up = layer_limit_superior) %>%
      rename(horizon_limit_low = layer_limit_inferior)
  }
  
  return(df_sub)
  
}