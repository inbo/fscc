
harmonise_into_fixed_depth_layers <-
  function(survey_form,
           parameters = c("horizon_bulk_dens_measure",
                          "horizon_bulk_dens_est",
                          "horizon_coarse_weight",
                          "code_horizon_coarse_vol",
                          "horizon_c_organic_total",
                          "horizon_clay")) {
    
  # Arrange input data ----
  
  # Import theoretical depths in "som" survey forms
    
  fixed_depths <-
    read.csv2("./data/additional_data/d_depth_level_soil.csv") %>%
    rename(code_layer = code) %>%
    select(code_layer, layer_limit_superior, layer_limit_inferior) %>%
    filter(!is.na(layer_limit_superior) & !is.na(layer_limit_inferior))
  
  
  # If "pfh"
  
  if ("horizon_limit_up" %in% names(survey_form)) { 
  
  # Combine the two bulk density columns into one bulk_density column
  # Priority: horizon_bulk_dens_measure
  # Second: horizon_bulk_dens_est
  
  assertthat::assert_that("layer_number" %in%
                            names(survey_form),
                          msg = paste0("The column 'layer_number' ",
                                       "is missing but required for this ",
                                       "function,\nin order to be able to ",
                                       "remove redundant/overlapping layers.\n",
                                       "Kindly add this column using the ",
                                       "function ",
                                       "'get_layer_inconsistencies()'."))
  
  df <- survey_form %>%
    mutate(bulk_density = ifelse(!is.na(horizon_bulk_dens_measure),
                                 horizon_bulk_dens_measure,
                                 horizon_bulk_dens_est)) %>%
    mutate(horizon_master = as.character(horizon_master)) %>%
    filter(!is.na(layer_number))
  
  }
  
  
  # Split parameters into numeric and categorical ones
  
  assertthat::assert_that(all(parameters %in% names(df)))
  
  if (!"bulk_density" %in% parameters) {
    parameters <- c(parameters, "bulk_density")
  }
  
  parameters_category <- parameters[grep("code", parameters)]
  parameters_numeric <- parameters[grep("code", parameters, invert = TRUE)]
  
  
  # Initiate harmonised data frame to be filled ----
  
  df_fixed <- df %>%
    distinct(unique_survey_profile) %>%
    # Create fixed depth layers for each unique profile
    group_by(unique_survey_profile) %>%
    do(expand.grid(code_layer =
                     c("O", "M05", "M51", "M01", "M12", "M24", "M48"))) %>%
    ungroup() %>%
    # Add depths
    left_join(fixed_depths, by = "code_layer") %>%
    # Split unique_survey_profile
    mutate(unique_survey_profile_to_separate = unique_survey_profile) %>%
    separate(unique_survey_profile_to_separate,
             into = c("code_country", "survey_year",
                      "code_plot", "profile_pit_id"),
             sep = "_") %>%
    # Add logical for forest floor as layer type
    mutate(forest_floor = FALSE)
  
  # Add new empty columns to the data frame
  df_fixed[parameters] <- NA
  
  # Convert specified columns to numeric
  df_fixed <- df_fixed %>%
    mutate_at(vars(all_of(parameters_numeric)), as.numeric)
  
  df <- df %>%
    mutate_at(vars(all_of(parameters_numeric)), as.numeric)
  
  # Adjust harmonised data frame ----
  
  # For each of the profiles
  
  unique_profiles <- unique(df_fixed$unique_survey_profile)
  
  for (i in seq_along(unique_profiles)) {
    
    profile_id <- unique_profiles[i]
    vec_prof <- which(df_fixed$unique_survey_profile == profile_id)
    
    vec_prof_df <- which(df$unique_survey_profile == profile_id)
    df_sub <- df[vec_prof_df, ] %>%
      ungroup() %>%
      as.data.frame %>%
      as_tibble
    
    ind_to_remove <- NULL
    extra_rows <- NULL
    
    
    # If no layer limits: remove profile
    
    if (all(is.na(df_sub$horizon_limit_up)) ||
        all(is.na(df_sub$horizon_limit_low))) {
      ind_to_remove <- vec_prof
    } else {
    
    
    # Adjust forest floor layers
    
    ind_ff <- vec_prof[which(df_fixed$code_layer[vec_prof] == "O")]
    
    forest_floor_layers <- df_sub %>%
      filter(layer_type == "forest_floor") %>%
      group_by(horizon_master) %>%
      summarize(
        horizon_limit_up = ifelse(all(is.na(horizon_limit_up)),
                                  NA,
                                  min(horizon_limit_up, na.rm = TRUE)),
        horizon_limit_low = ifelse(all(is.na(horizon_limit_low)),
                                   NA,
                                   max(horizon_limit_low, na.rm = TRUE)))
      # summarize(
      #   horizon_limit_up = min(horizon_limit_up),
      #   horizon_limit_low = max(horizon_limit_low))
      # select(horizon_master, horizon_limit_up, horizon_limit_low)
    
    if (nrow(forest_floor_layers) == 0) {
      ind_to_remove <- ind_ff
    }
    
    if (nrow(forest_floor_layers) == 1) {
      df_fixed$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_fixed$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_fixed$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_fixed$forest_floor[ind_ff] <- TRUE
    }
    
    
    
    if (nrow(forest_floor_layers) == 2) {
      
      extra_rows <- df_fixed[ind_ff, ]
      
      df_fixed$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_fixed$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_fixed$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_fixed$forest_floor[ind_ff] <- TRUE
      
      extra_rows$code_layer[1] <-
        forest_floor_layers$horizon_master[2]
      extra_rows$layer_limit_superior[1] <-
        forest_floor_layers$horizon_limit_up[2]
      extra_rows$layer_limit_inferior[1] <-
        forest_floor_layers$horizon_limit_low[2]
      extra_rows$forest_floor[1] <- TRUE
      
    }
    
    
    
    if (nrow(forest_floor_layers) == 3) {
      
      extra_rows <- rbind(df_fixed[ind_ff, ],
                          df_fixed[ind_ff, ])
      
      df_fixed$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_fixed$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_fixed$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_fixed$forest_floor[ind_ff] <- TRUE
      
      extra_rows$code_layer[1] <-
        forest_floor_layers$horizon_master[2]
      extra_rows$layer_limit_superior[1] <-
        forest_floor_layers$horizon_limit_up[2]
      extra_rows$layer_limit_inferior[1] <-
        forest_floor_layers$horizon_limit_low[2]
      extra_rows$forest_floor[1] <- TRUE
      
      extra_rows$code_layer[2] <-
        forest_floor_layers$horizon_master[3]
      extra_rows$layer_limit_superior[2] <-
        forest_floor_layers$horizon_limit_up[3]
      extra_rows$layer_limit_inferior[2] <-
        forest_floor_layers$horizon_limit_low[3]
      extra_rows$forest_floor[2] <- TRUE
      
    }
    
    
    
    
    if (nrow(forest_floor_layers) == 4) {
      
      extra_rows <- rbind(df_fixed[ind_ff, ],
                          df_fixed[ind_ff, ],
                          df_fixed[ind_ff, ])
      
      df_fixed$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_fixed$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_fixed$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_fixed$forest_floor[ind_ff] <- TRUE
      
      extra_rows$code_layer[1] <-
        forest_floor_layers$horizon_master[2]
      extra_rows$layer_limit_superior[1] <-
        forest_floor_layers$horizon_limit_up[2]
      extra_rows$layer_limit_inferior[1] <-
        forest_floor_layers$horizon_limit_low[2]
      extra_rows$forest_floor[1] <- TRUE
      
      extra_rows$code_layer[2] <-
        forest_floor_layers$horizon_master[3]
      extra_rows$layer_limit_superior[2] <-
        forest_floor_layers$horizon_limit_up[3]
      extra_rows$layer_limit_inferior[2] <-
        forest_floor_layers$horizon_limit_low[3]
      extra_rows$forest_floor[2] <- TRUE
      
      extra_rows$code_layer[3] <-
        forest_floor_layers$horizon_master[4]
      extra_rows$layer_limit_superior[3] <-
        forest_floor_layers$horizon_limit_up[4]
      extra_rows$layer_limit_inferior[3] <-
        forest_floor_layers$horizon_limit_low[4]
      extra_rows$forest_floor[3] <- TRUE
      
    }
    
    
    if (nrow(forest_floor_layers) == 5) {
      
      extra_rows <- rbind(df_fixed[ind_ff, ],
                          df_fixed[ind_ff, ],
                          df_fixed[ind_ff, ],
                          df_fixed[ind_ff, ])
      
      df_fixed$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_fixed$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_fixed$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_fixed$forest_floor[ind_ff] <- TRUE
      
      extra_rows$code_layer[1] <-
        forest_floor_layers$horizon_master[2]
      extra_rows$layer_limit_superior[1] <-
        forest_floor_layers$horizon_limit_up[2]
      extra_rows$layer_limit_inferior[1] <-
        forest_floor_layers$horizon_limit_low[2]
      extra_rows$forest_floor[1] <- TRUE
      
      extra_rows$code_layer[2] <-
        forest_floor_layers$horizon_master[3]
      extra_rows$layer_limit_superior[2] <-
        forest_floor_layers$horizon_limit_up[3]
      extra_rows$layer_limit_inferior[2] <-
        forest_floor_layers$horizon_limit_low[3]
      extra_rows$forest_floor[2] <- TRUE
      
      extra_rows$code_layer[3] <-
        forest_floor_layers$horizon_master[4]
      extra_rows$layer_limit_superior[3] <-
        forest_floor_layers$horizon_limit_up[4]
      extra_rows$layer_limit_inferior[3] <-
        forest_floor_layers$horizon_limit_low[4]
      extra_rows$forest_floor[3] <- TRUE
      
      extra_rows$code_layer[4] <-
        forest_floor_layers$horizon_master[5]
      extra_rows$layer_limit_superior[4] <-
        forest_floor_layers$horizon_limit_up[5]
      extra_rows$layer_limit_inferior[4] <-
        forest_floor_layers$horizon_limit_low[5]
      extra_rows$forest_floor[4] <- TRUE
      
    }
    
    
    # Adjust below-ground layers
    
    max_depth <-
      max(df_sub$horizon_limit_low[which(df_sub$horizon_limit_low < 300)],
          na.rm = TRUE)
    
    if (max_depth <= 40) {
      ind_to_remove <-
        c(ind_to_remove,
          vec_prof[which(df_fixed$code_layer[vec_prof] == "M48")])
    }
    
    if (max_depth <= 20) {
      ind_to_remove <-
        c(ind_to_remove,
          vec_prof[which(df_fixed$code_layer[vec_prof] == "M24")])
    }
    
    if (max_depth <= 10) {
      ind_to_remove <-
        c(ind_to_remove,
          vec_prof[which(df_fixed$code_layer[vec_prof] == "M12")])
    }
    }
    
    
    # Prepare to remove rows
    
    if (!is.null(ind_to_remove)) {
      layer_to_remove <- df_fixed$code_layer[ind_to_remove]
    }
    
    
    # Add rows
    
    if (!is.null(extra_rows)) {
      df_fixed <- rbind(df_fixed[seq_len(ind_ff), ],
                    extra_rows,
                    df_fixed[seq(from = (ind_ff + 1),
                                 to = nrow(df_fixed)), ])
    }
    
    
    # Remove rows
    
    if (!is.null(ind_to_remove)) {
      ind_to_remove <-
        which(df_fixed$unique_survey_profile == profile_id &
                df_fixed$code_layer %in% layer_to_remove)
      
      df_fixed <- df_fixed[-ind_to_remove, ]
    }
  }
  
  
  
  
  
  
  
  source("./src/functions/harmonise_layer_to_depths.R")
  
  
  # Harmonise data ----
  
  # For each of the profiles
  
  unique_profiles <- unique(df_fixed$unique_survey_profile)
  
  # Set progress bar
  progress_bar <- txtProgressBar(min = 0,
                                 max = length(unique_profiles), style = 3)
  
  
  for (i in seq_along(unique_profiles)) {
    
    profile_id <- unique_profiles[i]
    vec_prof <- which(df_fixed$unique_survey_profile == profile_id)
    
    vec_prof_df <- which(df$unique_survey_profile == profile_id)
    df_sub <- df[vec_prof_df, ]
  
    # For each of the fixed layers
    
    for (j in vec_prof) {
      
      ind_match <- NULL
      
      # If forest floor
      
      if (df_fixed$forest_floor[j] == TRUE) {
        
        ind_match <-
          which(df_sub$horizon_master == df_fixed$code_layer[j])
        
        # Fill in parameters
        for (k in seq_along(parameters)) {
          
          col_fixed <- which(names(df_fixed) == parameters[k])
          col_df <- which(names(df_sub) == parameters[k])
          
          if (length(ind_match) == 1) {
          
          df_fixed[j, col_fixed] <- df_sub[ind_match, col_df]
          
          
          # If there is more than one match (e.g. three "O" layers)
          
          } else
            if (length(ind_match) > 1) {
              
              # if all data are NA
              
              if (all(is.na(df_sub[ind_match, col_df]))) {
                df_fixed[j, col_fixed] <- NA
                
              # if not all data are NA
              } else {
              
              # if all layer limits are NA
              
              if (all(is.na(df_sub$horizon_limit_up[ind_match])) &&
                  all(is.na(df_sub$horizon_limit_low[ind_match]))) {
                
              df_fixed[j, col_fixed] <-
                ifelse((parameters[k] %in% parameters_numeric),
                       # mean
                       mean(as.numeric(unlist(df_sub[ind_match, col_df])),
                            na.rm = TRUE),
                       # most abundant
                       df_sub[ind_match, ] %>%
                         select(all_of(parameter_name)) %>%
                         #slice(ind_match) %>%
                         na.omit() %>%
                         count(.data[[parameter_name]]) %>%
                         arrange(desc(n)) %>%
                         head(1) %>%
                         pull(.data[[parameter_name]]))
              } else {
              
              # if not all layer limits are NA
              
              df_sub_selected <- df_sub[ind_match, ] %>%
                #slice(ind_match) %>%
                filter(!is.na(horizon_limit_up) &
                         !is.na(horizon_limit_low))
              
              limit_sup <- min(df_sub_selected$horizon_limit_up)
              limit_inf <- max(df_sub_selected$horizon_limit_low)
              
              # Numeric parameters
              
              if (parameters[k] %in% parameters_numeric) {
                
                df_fixed[j, col_fixed] <-
                  harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = parameters[k],
                                  mode = "numeric")
              }
              
              # Categorical parameters
              
              if (parameters[k] %in% parameters_category) {
                
                df_fixed[j, col_fixed] <-
                  harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = parameters[k],
                                  mode = "categorical")
              }
              }
              
              }
            }
          
        }
        
        
      # If below-ground
        
      } else {
      
        limit_sup <- df_fixed$layer_limit_superior[j]
        limit_inf <- df_fixed$layer_limit_inferior[j]
        limit_range <- seq(limit_sup, limit_inf)
        
        df_sub_selected <- df_sub %>%
          filter(!is.na(horizon_limit_up) &
                   !is.na(horizon_limit_low)) %>%
          rowwise() %>%
          filter(any(limit_range > .data$horizon_limit_up &
                       limit_range < .data$horizon_limit_low))
        
        # Fill in parameters
        for (k in seq_along(parameters)) {
        
          col_fixed <- which(names(df_fixed) == parameters[k])

          # Numeric parameters
          
          if (parameters[k] %in% parameters_numeric) {
            
            df_fixed[j, col_fixed] <-
              harmonise_layer_to_depths(limit_sup = limit_sup,
                              limit_inf = limit_inf,
                              df_sub_selected = df_sub_selected,
                              parameter_name = parameters[k],
                              mode = "numeric")
          }
          
          # Categorical parameters
          
          if (parameters[k] %in% parameters_category) {
            
            df_fixed[j, col_fixed] <-
              harmonise_layer_to_depths(limit_sup = limit_sup,
                              limit_inf = limit_inf,
                              df_sub_selected = df_sub_selected,
                              parameter_name = parameters[k],
                              mode = "categorical")
          }
        }

      }
    }
  
    # Update progress bar
    setTxtProgressBar(progress_bar, i)
    
  } # All profiles completed
  
  close(progress_bar)
  
  return(df_fixed)
  
}
