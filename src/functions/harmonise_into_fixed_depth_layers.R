
harmonise_into_fixed_depth_layers <-
  function(data_frame,
           survey_form_df,
           target_layers_df = NULL,
           parameters = c("horizon_bulk_dens_measure",
                          "horizon_bulk_dens_est",
                          "horizon_coarse_weight",
                          "code_horizon_coarse_vol",
                          "horizon_c_organic_total",
                          "horizon_clay",
                          "horizon_silt",
                          "horizon_sand")) {

  source("./src/functions/expand_unique_survey_vec_adjacent_years.R")
  source("./src/functions/harmonise_layer_to_depths.R")

  code_survey <- unlist(str_split(survey_form_df, "_"))[1]
  survey_form_type <- unlist(str_split(survey_form_df, "_"))[2]

  # pfh or som or swc?

  if (survey_form_type == "pfh") {
    assertthat::assert_that(is.null(target_layers_df))
  } else

  if (survey_form_type == "som") {
      assertthat::assert_that(!is.null(target_layers_df))

      if (identical(parameters,
                    c("horizon_bulk_dens_measure",
                       "horizon_bulk_dens_est",
                       "horizon_coarse_weight",
                       "code_horizon_coarse_vol",
                       "horizon_c_organic_total",
                       "horizon_clay",
                       "horizon_silt",
                       "horizon_sand"))) {

        parameters <- c("bulk_density",
                        "coarse_fragment_vol",
                      #  "organic_layer_weight",
                        "organic_carbon_total",
                        "part_size_clay",
                        "part_size_silt",
                        "part_size_sand")
      }
  } else

  if (survey_form_type == "swc") {

    if (identical(parameters,
                  c("horizon_bulk_dens_measure",
                    "horizon_bulk_dens_est",
                    "horizon_coarse_weight",
                    "code_horizon_coarse_vol",
                    "horizon_c_organic_total",
                    "horizon_clay",
                    "horizon_silt",
                    "horizon_sand"))) {

      parameters <- c("bulk_density")
    }
  }



  # Arrange input data ----

  # If "pfh"

  if (survey_form_type == "pfh") {

  # Combine the two bulk density columns into one bulk_density column
  # Priority: horizon_bulk_dens_measure
  # Second: horizon_bulk_dens_est

  assertthat::assert_that("layer_number" %in%
                            names(data_frame),
                          msg = paste0("The column 'layer_number' ",
                                       "is missing but required for this ",
                                       "function,\nin order to be able to ",
                                       "remove redundant/overlapping layers.\n",
                                       "Kindly add this column using the ",
                                       "function ",
                                       "'get_layer_inconsistencies()'."))

  data_frame <- data_frame %>%
    mutate(bulk_density = ifelse(!is.na(horizon_bulk_dens_measure),
                                 horizon_bulk_dens_measure,
                                 horizon_bulk_dens_est)) %>%
    mutate(horizon_master = as.character(horizon_master))

  }


  if (survey_form_type == "pfh" || survey_form_type == "som") {

  # Filter for non-redundant layers

  data_frame <- data_frame %>%
    filter(!is.na(layer_number))


  # Split parameters into numeric and categorical ones

  assertthat::assert_that(all(parameters %in% names(data_frame)))

  if (!"bulk_density" %in% parameters) {
    parameters <- c(parameters, "bulk_density")
  }

  parameters_category <- parameters[grep("code", parameters)]
  parameters_numeric <- parameters[grep("code", parameters, invert = TRUE)]

  }

  if (survey_form_type == "swc") {
    parameters_numeric <- parameters
    parameters_category <- NULL
  }





  # Initiate harmonised data frame to be filled ----

  ## Target: fixed-depth layers ----

  if ((survey_form_type == "pfh" || survey_form_type == "swc") &&
      is.null(target_layers_df)) {

  # Import theoretical depths in "som" survey forms

  fixed_depths <-
    read.csv2("./data/additional_data/d_depth_level_soil.csv") %>%
    rename(code_layer = code) %>%
    select(code_layer, layer_limit_superior, layer_limit_inferior) %>%
    filter(!is.na(layer_limit_superior) & !is.na(layer_limit_inferior))

  if ("unique_survey_profile" %in% names(data_frame)) {

    df_target <- data_frame %>%
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

  } else {

    df_target <- data_frame %>%
      distinct(unique_survey) %>%
      # Create fixed depth layers for each unique profile
      group_by(unique_survey) %>%
      do(expand.grid(code_layer =
                       c("O", "M05", "M51", "M01", "M12", "M24", "M48"))) %>%
      ungroup() %>%
      # Add depths
      left_join(fixed_depths, by = "code_layer") %>%
      # Split unique_survey
      mutate(unique_survey_to_separate = unique_survey) %>%
      separate(unique_survey_to_separate,
               into = c("code_country", "survey_year",
                        "code_plot"),
               sep = "_") %>%
      # Add logical for forest floor as layer type
      mutate(forest_floor = FALSE)

  }

  if (!"unique_survey_profile" %in% names(df_target)) {

    data_frame <- data_frame %>%
      rename(horizon_limit_up = ring_depth_upper) %>%
      rename(horizon_limit_low = ring_depth_lower) %>%
      rename(horizon_master = code_depth_layer) %>%
      rename(unique_survey_profile = unique_survey)

    df_target <- df_target %>%
      rename(unique_survey_profile = unique_survey)

  }


  # Add new empty columns to the data frame
  df_target[parameters] <- NA

  # Convert specified columns to numeric
  df_target <- df_target %>%
    mutate_at(vars(all_of(parameters_numeric)), as.numeric)

  data_frame <- data_frame %>%
    mutate_at(vars(all_of(parameters_numeric)), as.numeric)

  ### Adjust harmonised data frame ----

  # For each of the profiles


  unique_profiles <- unique(df_target$unique_survey_profile)


  for (i in seq_along(unique_profiles)) {

    profile_id <- unique_profiles[i]
    vec_prof <- which(df_target$unique_survey_profile == profile_id)

    vec_prof_df <- which(data_frame$unique_survey_profile == profile_id)
    df_sub <- data_frame[vec_prof_df, ]

    ind_to_remove <- NULL
    extra_rows <- NULL


    # If no layer limits: remove profile

    if (all(is.na(df_sub$horizon_limit_up)) ||
        all(is.na(df_sub$horizon_limit_low))) {

      ind_to_remove <- vec_prof

    } else {


    # Adjust forest floor layers

    ind_ff <- vec_prof[which(df_target$code_layer[vec_prof] == "O")]

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
      df_target$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_target$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_target$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_target$forest_floor[ind_ff] <- TRUE
    }



    if (nrow(forest_floor_layers) == 2) {

      extra_rows <- df_target[ind_ff, ]

      df_target$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_target$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_target$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_target$forest_floor[ind_ff] <- TRUE

      extra_rows$code_layer[1] <-
        forest_floor_layers$horizon_master[2]
      extra_rows$layer_limit_superior[1] <-
        forest_floor_layers$horizon_limit_up[2]
      extra_rows$layer_limit_inferior[1] <-
        forest_floor_layers$horizon_limit_low[2]
      extra_rows$forest_floor[1] <- TRUE

    }



    if (nrow(forest_floor_layers) == 3) {

      extra_rows <- rbind(df_target[ind_ff, ],
                          df_target[ind_ff, ])

      df_target$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_target$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_target$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_target$forest_floor[ind_ff] <- TRUE

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

      extra_rows <- rbind(df_target[ind_ff, ],
                          df_target[ind_ff, ],
                          df_target[ind_ff, ])

      df_target$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_target$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_target$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_target$forest_floor[ind_ff] <- TRUE

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

      extra_rows <- rbind(df_target[ind_ff, ],
                          df_target[ind_ff, ],
                          df_target[ind_ff, ],
                          df_target[ind_ff, ])

      df_target$code_layer[ind_ff] <-
        forest_floor_layers$horizon_master[1]
      df_target$layer_limit_superior[ind_ff] <-
        forest_floor_layers$horizon_limit_up[1]
      df_target$layer_limit_inferior[ind_ff] <-
        forest_floor_layers$horizon_limit_low[1]
      df_target$forest_floor[ind_ff] <- TRUE

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
          vec_prof[which(df_target$code_layer[vec_prof] == "M48")])
    }

    if (max_depth <= 20) {
      ind_to_remove <-
        c(ind_to_remove,
          vec_prof[which(df_target$code_layer[vec_prof] == "M24")])
    }

    if (max_depth <= 10) {
      ind_to_remove <-
        c(ind_to_remove,
          vec_prof[which(df_target$code_layer[vec_prof] == "M12")])
    }
    }


    # Prepare to remove rows

    if (!is.null(ind_to_remove)) {
      layer_to_remove <- df_target$code_layer[ind_to_remove]
    }


    # Add rows

    if (!is.null(extra_rows)) {
      df_target <- rbind(df_target[seq_len(ind_ff), ],
                    extra_rows,
                    df_target[seq(from = (ind_ff + 1),
                                 to = nrow(df_target)), ])
    }


    # Remove rows

    if (!is.null(ind_to_remove)) {
      ind_to_remove <-
        which(df_target$unique_survey_profile == profile_id &
                df_target$code_layer %in% layer_to_remove)

      df_target <- df_target[-ind_to_remove, ]
    }
  }
}







  ## Target: pedogenic layers ----

  if (survey_form_type == "som" &&
      !is.null(target_layers_df)) {

    # Retrieve links between forest floor layers "som" versus "pfh"

    link_forest_floors <-
      read.csv2(paste0("./data/additional_data/",
                       code_survey,
                       "_link_forest_floors.csv")) %>%
      mutate(unique_survey_link = NA) %>%
      mutate_all(~ifelse((.) == "", NA, .))

    if (!"code_country" %in% names(link_forest_floors) &&
        !"group" %in% names(link_forest_floors)) {

    link_forest_floors <- link_forest_floors %>%
      mutate(plot_id_to_split = plot_id) %>%
      separate(plot_id_to_split, into = c("code_country", "code_plot")) %>%
      mutate(group =
               cumsum(.data$survey_form %in% c("so_som", "s1_som") &
                      lag(.data$survey_form, default = "") %in%
                        c("so_pfh", "s1_pfh")))
    }

    for (i in seq_along(unique(link_forest_floors$group))) {

      vec_som <-
        which(link_forest_floors$group ==
                unique(link_forest_floors$group)[i] &
              grepl("som", link_forest_floors$survey_form))

      vec_pfh <-
        which(link_forest_floors$group ==
                unique(link_forest_floors$group)[i] &
                grepl("pfh", link_forest_floors$survey_form))

      link_forest_floors$unique_survey_link[vec_pfh] <-
        unique(link_forest_floors$unique_survey[vec_som])

    }

    link_forest_floors <- link_forest_floors %>%
      filter(!is.na(code_layers_link))


    # Prepare target dataframe

    df_target <- target_layers_df %>%
      filter(!is.na(.data$layer_number)) %>%
      select(unique_survey_profile,
             horizon_master,
             horizon_limit_up,
             horizon_limit_low,
             code_country,
             survey_year,
             code_plot,
             profile_pit_id,
             layer_type,
             layer_number) %>%
      # At the moment, we can ignore the different repetitions in "som"
      # and immediately calculate the average for each "pfh" horizon
      mutate(unique_survey_som = NA,
             code_layers_som = NA,
             to_remove = FALSE)

    rows_to_add <- NULL

    # Add the matching "som" unique_survey and code_layer

    for (i in seq_along(unique(df_target$unique_survey_profile))) {

      profile_i <- unique(df_target$unique_survey_profile)[i]
      vec <- which(df_target$unique_survey_profile == profile_i)

      rows_to_add_i <- NULL

      # Retrieve the corresponding unique_survey (with maximum 3
      # survey years difference) in som

      unique_survey_i <-
        paste0(unique(df_target$code_country[vec]), "_",
               unique(df_target$survey_year[vec]), "_",
               unique(df_target$code_plot[vec]))

      unique_survey_i_expanded <-
          expand_unique_survey_vec_adjacent_years(unique_survey_i, 3)

      unique_survey_som_i <-
        unique_survey_i_expanded[unique_survey_i_expanded %in%
                                   data_frame$unique_survey]

      if (identical(unique_survey_som_i, character(0))) {

        df_target$to_remove[vec] <- TRUE

      } else {


      df_target$unique_survey_som[vec] <- unique_survey_som_i[1]

      if (length(unique_survey_som_i) > 1) {
        # For example two distinct surveys for plot_id 4_4000259
        # in 2006 and 2007

        rows_to_add_i <- df_target[vec,]
        rows_to_add_i$unique_survey_som <- unique_survey_som_i[2]

        if (length(unique_survey_som_i) > 2) {

          # For example 4_4000267
          rows_to_add_i2 <- df_target[vec,]
          rows_to_add_i2$unique_survey_som <- unique_survey_som_i[3]

          rows_to_add_i <- rbind(rows_to_add_i,
                                 rows_to_add_i2)

          assertthat::assert_that(length(unique_survey_som_i) == 3)

        }

      }

      # Retrieve the matching forest floor layers

      for (j in vec) {

        if (df_target$layer_type[j] == "forest_floor" &&
            ((is.na(df_target$horizon_limit_up[j]) ||
              df_target$horizon_limit_up[j] <= 0) &&
             (is.na(df_target$horizon_limit_low[j]) ||
              df_target$horizon_limit_low[j] <= 0))) {

          ind <- which(link_forest_floors$profile_id ==
                         df_target$unique_survey_profile[j] &
                       ((link_forest_floors$code_layer_unique ==
                             df_target$horizon_master[j])))

          if (!identical(ind, integer(0))) {

            assertthat::assert_that(
              length(unique(link_forest_floors$code_layers_link[ind])) == 1)

            df_target$code_layers_som[j] <-
              link_forest_floors$code_layers_link[ind[1]]

            # assertthat::assert_that(
            #   df_target$unique_survey_som[j] ==
            #     link_forest_floors$unique_survey_link[ind])

            # Add to the extra rows too

            if (!is.null(rows_to_add_i)) {

              rows_to_add_i$code_layers_som[
                which(rows_to_add_i$horizon_master ==
                        df_target$horizon_master[j])] <-
                link_forest_floors$code_layers_link[ind[1]]
            }

          }
        }
      }

      if (!is.null(rows_to_add_i)) {
      rows_to_add <- rbind(rows_to_add,
                           rows_to_add_i)
      }
    }
    }

    # Remove "pfh" surveys without matching "som" survey

    df_target <- df_target %>%
      bind_rows(rows_to_add) %>%
      filter(.data$to_remove == FALSE) %>%
      select(-to_remove) %>%
      mutate(forest_floor = ifelse(.data$layer_type == "forest_floor",
                                   TRUE,
                                   FALSE))

    # Add new empty columns to the data frame
    df_target[parameters] <- NA


  }







  # Harmonise data ----


  ## Target: fixed-depth layers ----

  if (survey_form_type == "pfh" || survey_form_type == "swc") {

  # For each of the profiles

  unique_profiles <- unique(df_target$unique_survey_profile)

  # Set progress bar
  progress_bar <- txtProgressBar(min = 0,
                                 max = length(unique_profiles), style = 3)


  for (i in seq_along(unique_profiles)) {

    profile_id <- unique_profiles[i]
    vec_prof <- which(df_target$unique_survey_profile == profile_id)

    vec_prof_df <- which(data_frame$unique_survey_profile == profile_id)
    df_sub <- data_frame[vec_prof_df, ]

    # For each of the fixed layers

    for (j in vec_prof) {

      ind_match <- NULL

      # If forest floor

      if (df_target$forest_floor[j] == TRUE) {

        ind_match <-
          which(df_sub$horizon_master == df_target$code_layer[j])

        # Fill in parameters
        for (k in seq_along(parameters)) {

          col_fixed <- which(names(df_target) == parameters[k])
          col_df <- which(names(df_sub) == parameters[k])

          if (length(ind_match) == 1) {

          df_target[j, col_fixed] <- df_sub[ind_match, col_df]


          # If there is more than one match (e.g. three "O" layers)

          } else
            if (length(ind_match) > 1) {

              # if all data are NA

              if (all(is.na(df_sub[ind_match, col_df]))) {
                df_target[j, col_fixed] <- NA

              # if not all data are NA
              } else {

              # if all layer limits are NA

              if (all(is.na(df_sub$horizon_limit_up[ind_match])) &&
                  all(is.na(df_sub$horizon_limit_low[ind_match]))) {

              df_target[j, col_fixed] <-
                ifelse((parameters[k] %in% parameters_numeric),
                       # mean
                       mean(as.numeric(pull(df_sub[ind_match, col_df])),
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

                df_target[j, col_fixed] <-
                  harmonise_layer_to_depths(limit_sup = limit_sup,
                                  limit_inf = limit_inf,
                                  df_sub_selected = df_sub_selected,
                                  parameter_name = parameters[k],
                                  mode = "numeric")
              }

              # Categorical parameters

              if (!is.null(parameters_category) &&
                  parameters[k] %in% parameters_category) {

                df_target[j, col_fixed] <-
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

        limit_sup <- df_target$layer_limit_superior[j]
        limit_inf <- df_target$layer_limit_inferior[j]
        limit_range <- seq(limit_sup, limit_inf)

        df_sub_selected <- df_sub %>%
          filter(!is.na(horizon_limit_up) &
                   !is.na(horizon_limit_low)) %>%
          rowwise() %>%
          filter(any(limit_range > .data$horizon_limit_up &
                       limit_range < .data$horizon_limit_low))

        # Fill in parameters
        for (k in seq_along(parameters)) {

          col_fixed <- which(names(df_target) == parameters[k])

          # Numeric parameters

          if (parameters[k] %in% parameters_numeric) {

            df_target[j, col_fixed] <-
              harmonise_layer_to_depths(limit_sup = limit_sup,
                              limit_inf = limit_inf,
                              df_sub_selected = df_sub_selected,
                              parameter_name = parameters[k],
                              mode = "numeric")
          }

          # Categorical parameters

          if (parameters[k] %in% parameters_category) {

            df_target[j, col_fixed] <-
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

  }








  ## Target: pedogenic layers ----

  if (survey_form_type == "som") {

    # For each of the profiles

    unique_surveys_som <- unique(df_target$unique_survey_som)

    # Set progress bar
    progress_bar <- txtProgressBar(min = 0,
                                   max = length(unique_surveys_som), style = 3)


    for (i in seq_along(unique_surveys_som)) {

      unique_survey_som_i <- unique_surveys_som[i]
      vec_target <- which(df_target$unique_survey_som == unique_survey_som_i)

      vec_som <- which(data_frame$unique_survey == unique_survey_som_i)
      df_sub <- data_frame[vec_som, ]

      # For each of the fixed layers

      for (j in vec_target) {

        ind_match <- NULL

        # If forest floor

        if (df_target$forest_floor[j] == TRUE) {

          ind_match <-
            which(df_sub$code_layer %in%
                    unlist(str_split(df_target$code_layers_som[j], "_")))

          # Fill in parameters
          for (k in seq_along(parameters)) {

            col_target <- which(names(df_target) == parameters[k])
            col_df <- which(names(df_sub) == parameters[k])

            if (length(ind_match) == 1) {

              df_target[j, col_target] <- df_sub[ind_match, col_df]


              # If there is more than one match

            } else
              if (length(ind_match) > 1) {

                # if all data are NA

                if (all(is.na(df_sub[ind_match, col_df]))) {
                  df_target[j, col_target] <- NA

                  # if not all data are NA
                } else {

                  # if all layer limits are NA

                  if (all(is.na(df_sub$layer_limit_superior[ind_match])) &&
                      all(is.na(df_sub$layer_limit_inferior[ind_match]))) {

                    df_target[j, col_target] <-
                      ifelse((parameters[k] %in% parameters_numeric),
                             # mean
                             mean(as.numeric(pull(df_sub[ind_match, col_df])),
                                  na.rm = TRUE),
                             # most abundant
                             df_sub[ind_match, ] %>%
                               select(all_of(parameter_name)) %>%
                               na.omit() %>%
                               count(.data[[parameter_name]]) %>%
                               arrange(desc(n)) %>%
                               head(1) %>%
                               pull(.data[[parameter_name]]))
                  } else {

                    # if not all layer limits are NA

                    df_sub_selected <- df_sub[ind_match, ] %>%
                      #slice(ind_match) %>%
                      filter(!is.na(layer_limit_superior) &
                               !is.na(layer_limit_inferior))

                    limit_sup <- min(df_sub_selected$layer_limit_superior)
                    limit_inf <- max(df_sub_selected$layer_limit_inferior)

                    # Numeric parameters

                    if (parameters[k] %in% parameters_numeric) {

                      df_target[j, col_target] <-
                        harmonise_layer_to_depths(limit_sup = limit_sup,
                                                  limit_inf = limit_inf,
                                                  df_sub_selected =
                                                    df_sub_selected,
                                                  parameter_name =
                                                    parameters[k],
                                                  mode = "numeric")
                    }

                    # Categorical parameters

                    if (parameters[k] %in% parameters_category) {

                      df_target[j, col_target] <-
                        harmonise_layer_to_depths(limit_sup = limit_sup,
                                                  limit_inf = limit_inf,
                                                  df_sub_selected =
                                                    df_sub_selected,
                                                  parameter_name =
                                                    parameters[k],
                                                  mode = "categorical")
                    }
                  }

                }
              }

          }


          # If below-ground

        } else {

          limit_sup <- df_target$horizon_limit_up[j]
          limit_inf <- df_target$horizon_limit_low[j]

          if (!is.na(limit_sup) &&
              !is.na(limit_inf)) {

          limit_range <- seq(limit_sup, limit_inf)

          df_sub_selected <- df_sub %>%
            filter(!is.na(layer_limit_superior) &
                     !is.na(layer_limit_inferior)) %>%
            rowwise() %>%
            filter(any(limit_range > .data$layer_limit_superior &
                         limit_range < .data$layer_limit_inferior))

          # Fill in parameters
          for (k in seq_along(parameters)) {

            col_target <- which(names(df_target) == parameters[k])

            # Numeric parameters

            if (parameters[k] %in% parameters_numeric) {

              df_target[j, col_target] <-
                harmonise_layer_to_depths(limit_sup = limit_sup,
                                          limit_inf = limit_inf,
                                          df_sub_selected = df_sub_selected,
                                          parameter_name = parameters[k],
                                          mode = "numeric")
            }

            # Categorical parameters

            if (parameters[k] %in% parameters_category) {

              df_target[j, col_target] <-
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

      # Update progress bar
      setTxtProgressBar(progress_bar, i)

    } # All profiles completed

    close(progress_bar)

  }


  df_target <- df_target %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))



  return(df_target)

}
