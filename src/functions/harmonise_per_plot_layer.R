

harmonise_per_plot_layer <- function(survey_form_input,
                                     data_frame_input = NULL) {

    # This function creates a dataframe with different depth layers per
    # plot_id with data for only timeless variables, i.e.
    # physical parameters that are considered constant over time

    # - bulk density
    # - coarse fragments
    # - texture

    survey_form <- survey_form_input
    data_frame <- data_frame_input


    source("./src/functions/harmonise_layer_to_depths.R")

    code_survey <- unlist(str_split(survey_form, "_"))[1]
    survey_form_type <- unlist(str_split(survey_form, "_"))[2]

    # Parameter ranges

    d_soil_coarse_fragments <-
      read.csv("./data/additional_data/d_soil_coarse_fragments.csv",
               sep = ";")

    ranges_qaqc <-
      read.csv("./data/additional_data/ranges_qaqc.csv", sep = ";")

    quant_bd <- c(6, 1991) # Based on minimum and maximum in
                           # bd_upper and bd_lower functions

    parameter_ranges <- bind_rows(
      data.frame(parameter = "bulk_density",
                 range_min = quant_bd[1],
                 range_max = quant_bd[2],
                 # Also generate a bulk density value for the forest floor
                 # (can be based on layer weight)
                 # While we can't assume that the organic_layer_weight
                 # remains constant over time,
                 # it does make sense that the bulk density of the forest floor
                 # remains constant.
                 incl_ff = TRUE),
      data.frame(parameter = "coarse_fragment_vol",
                 range_min = 0,
                 range_max = 100,
                 incl_ff = FALSE),
      data.frame(parameter = "part_size_clay",
                 range_min = 0,
                 range_max = 100,
                 incl_ff = FALSE),
      data.frame(parameter = "part_size_silt",
                 range_min = 0,
                 range_max = 100,
                 incl_ff = FALSE),
      data.frame(parameter = "part_size_sand",
                 range_min = 0,
                 range_max = 100,
                 incl_ff = FALSE))



    source("./src/functions/bulk_density_ptf.R")





    # Parameters

    if (survey_form_type == "som") {

      parameters <- c("bulk_density",
                      "coarse_fragment_vol",
                      "part_size_clay",
                      "part_size_silt",
                      "part_size_sand")

    }

    if (survey_form_type == "pfh") {

      # Since volumetric coarse fragment content based on horizon_coarse_weight
      # (i.e. coarse_fragment_vol_converted) can be improved with better
      # gap-filling of bulk density throughout this script,
      # We will recalculate coarse_fragment_vol_converted in the end of this
      # script

      parameters <- c("horizon_bulk_dens_measure",
                      "horizon_bulk_dens_est",
                      "horizon_coarse_weight",
                      "coarse_fragment_vol_avg",
                      "horizon_clay",
                      "horizon_silt",
                      "horizon_sand")

      parameters_conv <- data.frame(
        pfh = parameters,
        som = c("bulk_density", "bulk_density", "coarse_fragment_vol",
                "coarse_fragment_vol", "part_size_clay", "part_size_silt",
                "part_size_sand"))

      parameter_ranges <- parameters_conv %>%
        left_join(parameter_ranges,
                  by = join_by("som" == "parameter")) %>%
        rename(parameter = pfh)

    }


    if (survey_form_type == "swc") {

      parameters <- c("bulk_density")

      parameters_conv <- data.frame(
        swc = parameters,
        som = c("bulk_density"))

      parameter_ranges <- parameters_conv %>%
        left_join(parameter_ranges,
                  by = join_by("som" == "parameter")) %>%
        rename(parameter = swc)

    }



    if (survey_form_type == "pfh") {

    # Retrieve links between forest floor layers "som" versus "pfh" ----

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

      for (grp in seq_along(unique(link_forest_floors$group))) {

        vec_som <-
          which(link_forest_floors$group ==
                  unique(link_forest_floors$group)[grp] &
                  grepl("som", link_forest_floors$survey_form))

        vec_pfh <-
          which(link_forest_floors$group ==
                  unique(link_forest_floors$group)[grp] &
                  grepl("pfh", link_forest_floors$survey_form))

        link_forest_floors$unique_survey_link[vec_pfh] <-
          unique(link_forest_floors$unique_survey[vec_som])

      }

    link_forest_floors <- link_forest_floors %>%
      filter(!is.na(code_layers_link))

    }




    # Retrieve the survey_form data ----

    if (is.null(data_frame)) {
      df <- get_env(survey_form)
    } else {
      df <- data_frame
    }

    if (survey_form_type == "pfh") {

      # Harmonise the variable names with those of "som"

      df <- df %>%
        rename(layer_limit_superior = horizon_limit_up) %>%
        rename(layer_limit_inferior = horizon_limit_low) %>%
        rename(code_layer = horizon_master) %>%
        rename(unique_survey_repetition = unique_survey_profile) %>%
        rename(organic_carbon_total = horizon_c_organic_total)

      # In order to retrieve forest floor bulk density data from "som"
      # (in order to calculate organic_layer_weight in "pfh",
      #  as this variable is not present in "pfh"):
      # Retrieve "som" data too

      som <- get_env(paste0(code_survey, "_som")) %>%
        mutate(bulk_density = coalesce(
          bulk_density,
          bulk_density_layer_weight)) %>%
        filter(!is.na(bulk_density)) %>%
        filter(!is.na(layer_number))

    }


    if (survey_form_type == "swc") {

      d_depth_level_soil <-
        read.csv("./data/additional_data/d_depth_level_soil.csv",
                 sep = ";") %>%
        rename(code_layer = code) %>%
        select(code_layer, layer_limit_superior, layer_limit_inferior)

      # Harmonise the variable names with those of "som"

      df <- df %>%
        rename(layer_limit_superior = ring_depth_upper) %>%
        rename(layer_limit_inferior = ring_depth_lower) %>%
        rename(code_layer = code_depth_layer) %>%
        mutate(organic_carbon_total = NA) %>%
        mutate(layer_thickness =
                 (layer_limit_inferior - layer_limit_superior)) %>%
        mutate(unique_survey_repetition = paste0(unique_survey, "_",
                                                 pfh_id, "_",
                                                 sw_id, "_",
                                                 replicate)) %>%
        # Obvious typo in one Slovak layer limit
        mutate(layer_limit_inferior =
                 ifelse(!is.na(code_line) &
                          code_line == "SWSK2009-540054-000055",
                        13,
                        layer_limit_inferior)) %>%
        # Thicknesses covered by Austrian records are always 4 cm in Austria,
        # except in some records of plot 14_17 and one record of 14_15 which
        # are obviously mistakes of layer_limit_superior
        mutate(layer_limit_superior =
                 ifelse(plot_id %in% c("14_17", "14_15") &
                          ((layer_limit_inferior - layer_limit_superior) != 4),
                        (layer_limit_inferior - 4),
                        layer_limit_superior)) %>%
        mutate(layer_thickness =
                 (layer_limit_inferior - layer_limit_superior))

      # No peat layers are reported (based on code_layer)

      assertthat::assert_that(nrow(filter(df, layer_type == "peat")) == 0)

      # All plots have maximally just one forest floor layer

      assertthat::assert_that(
        (df %>%
            filter(layer_limit_superior < 0) %>%
            arrange(plot_id, layer_limit_superior) %>%
            group_by(plot_id) %>%
            reframe(count = n_distinct(code_layer)) %>%
            filter(count > 1) %>%
            nrow) == 0)

      ff <- df %>%
        filter(layer_limit_superior < 0) %>%
        arrange(plot_id, layer_limit_superior) %>%
        group_by(plot_id) %>%
        reframe(code_layer_ff = unique(code_layer))

      df_target <- df %>%
        distinct(plot_id) %>%
        # Create fixed depth layers for each unique profile
        group_by(plot_id) %>%
        do(expand.grid(code_layer =
                         c("O",
                           "M05", "M51", "M12", "M24", "M48", "M815"))) %>%
        ungroup() %>%
        # Add depths
        left_join(d_depth_level_soil, by = "code_layer") %>%
        mutate(
          layer_limit_superior = ifelse(
            code_layer == "M815", 80, layer_limit_superior),
          layer_limit_inferior = ifelse(
            code_layer == "M815", 150, layer_limit_inferior)) %>%
        mutate(
          layer_limit_superior = ifelse(
            code_layer == "O", -20, layer_limit_superior),
          layer_limit_inferior = ifelse(
            code_layer == "O", 0, layer_limit_inferior)) %>%
        rename(layer_limit_superior_theor = layer_limit_superior) %>%
        rename(layer_limit_inferior_theor = layer_limit_inferior) %>%
        # Split unique_survey_profile
        mutate(plot_id_to_separate = plot_id) %>%
        separate(plot_id_to_separate,
                 into = c("code_country",
                          "code_plot"),
                 sep = "_") %>%
        mutate(code_country = as.numeric(code_country)) %>%
        left_join(d_country %>%
                    select(code, lib_country) %>%
                    rename(country = lib_country),
                  by = join_by("code_country" == "code")) %>%
        left_join(ff,
                  by = "plot_id") %>%
        mutate(layer_type = ifelse(code_layer == "O",
                                   "forest_floor",
                                   "mineral")) %>%
        mutate(code_layer = ifelse(code_layer == "O",
                                   code_layer_ff,
                                   code_layer)) %>%
        filter(!is.na(code_layer)) %>%
        group_by(plot_id) %>%
        mutate(layer_limit_superior = NA,
               layer_limit_inferior = NA) %>%
        mutate(layer_number = rank(layer_limit_superior_theor)) %>%
        select(country, plot_id, code_layer, layer_number, layer_type,
               layer_limit_superior, layer_limit_inferior,
               layer_limit_superior_theor, layer_limit_inferior_theor) %>%
        # Add columns to fill
        mutate(
          bulk_density = NA,
          bulk_density_survey_year = NA,
          bulk_density_min = NA,
          bulk_density_max = NA) %>%
        arrange(country, plot_id, layer_number)

      }



    # som and pfh ----


    if (survey_form_type == "pfh" || survey_form_type == "som") {

      # Filter for non-redundant layers

      df <- df %>%
        filter(!is.na(layer_number))


    assertthat::assert_that(all(parameters %in% names(df)))

    df_target <- NULL

    if (!isTRUE(getOption("knitr.in.progress"))) {
      progress_bar <- txtProgressBar(min = 0,
                                     max = length(unique(df$plot_id)),
                                     style = 3)
    }

    # Evaluate each plot_id ----

   for (i in seq_along(unique(df$plot_id))) {

      plot_id_i <- unique(df$plot_id)[i]

      df_i <- df %>%
        filter(plot_id == plot_id_i) %>%
        ungroup

      if (all(df_i$layer_number <= 3) &&
          all(df_i$layer_type == "forest_floor") &&
          all(is.na(df_i$layer_limit_superior))) {
        next # Go to next plot
      }

      # if below-ground layer limits are unknown (e.g. in s1_pfh 51_328)
      if (all(is.na(df_i$layer_limit_superior[
        which(df_i$layer_type != "forest_floor")]))) {
        next # Go to next plot
      }

      if (survey_form_type == "som") {

        df_i <- df_i %>%
          mutate(bulk_density = coalesce(bulk_density,
                                         bulk_density_layer_weight),
                 cf = ifelse(is.na(coarse_fragment_vol),
                             0,
                             coarse_fragment_vol),
                 bulk_density_total_soil =
                   .data$bulk_density *
                   (1 - 0.01 * .data$cf))
      }

      if (survey_form_type == "pfh") {

        df_i <- df_i %>%
          mutate(bd = coalesce(horizon_bulk_dens_measure,
                               horizon_bulk_dens_est),
                 cf = coalesce(coarse_fragment_vol_converted,
                               coarse_fragment_vol_avg),
                 cf = ifelse(is.na(cf),
                             0,
                             cf),
                 bulk_density_total_soil =
                   .data$bd * (1 - 0.01 * .data$cf))
      }

      # Sometimes there are redundant profiles

      profiles_to_be_removed <- df_i %>%
        group_by(unique_survey_repetition) %>%
        reframe(all_unknown_limits = all(is.na(layer_limit_superior))) %>%
                                           # (layer_type == "forest_floor") &
                                           # (layer_number <= 3))) %>%
        filter(all_unknown_limits == TRUE) %>%
        pull(unique_survey_repetition)

      if (!identical(profiles_to_be_removed, character(0))) {

        df_i <- df_i %>%
          filter(!unique_survey_repetition %in% profiles_to_be_removed)
      }

      # Prepare a selection of target layers to be filled
      # By taking the profile of the most recent survey year covering the
      # biggest depth range

      survey_years_target_i <- df_i %>%
        distinct(survey_year) %>%
        filter(survey_year >= 2000)

      if (nrow(survey_years_target_i) == 0) {
        survey_years_target_i <- df_i %>%
          distinct(survey_year)
      }

      survey_years_target_i <- survey_years_target_i$survey_year

      selected_prof_i <- df_i %>%
        filter(survey_year %in% survey_years_target_i) %>%
        group_by(unique_survey_repetition, survey_year) %>%
        reframe(top = min(layer_limit_superior, na.rm = TRUE),
                bottom = max(layer_limit_inferior, na.rm = TRUE)) %>%
        ungroup() %>%
        # Truncate to decimeters to avoid little differences in
        # forest floor
        mutate(depth_range = 10 * trunc(0.1 * (.data$bottom - .data$top))) %>%
        arrange(-survey_year) %>%
        filter(depth_range == max(.data$depth_range)) %>%
        # select any if multiple options
        slice_head() %>%
        pull(unique_survey_repetition)

      if (survey_form_type == "som") {

      df_target_i <- df_i %>%
        filter(unique_survey_repetition == selected_prof_i) %>%
        select(country, plot_id, code_layer, layer_number, layer_type,
               layer_limit_superior, layer_limit_inferior) %>%
        # Add columns to fill
        mutate(
          bulk_density = NA,
          bulk_density_survey_year = NA,
          bulk_density_min = NA,
          bulk_density_max = NA,
          coarse_fragment_vol = NA,
          coarse_fragment_vol_survey_year = NA,
          coarse_fragment_vol_min = NA,
          coarse_fragment_vol_max = NA,
          part_size_clay = NA,
          part_size_clay_min = NA,
          part_size_clay_max = NA,
          part_size_silt = NA,
          part_size_silt_min = NA,
          part_size_silt_max = NA,
          part_size_sand = NA,
          part_size_sand_min = NA,
          part_size_sand_max = NA,
          texture_survey_year = NA)
      }

      if (survey_form_type == "pfh") {

        df_target_i <- df_i %>%
          filter(unique_survey_repetition == selected_prof_i) %>%
          select(country, plot_id, code_layer, layer_number, layer_type,
                 layer_limit_superior, layer_limit_inferior) %>%
          # Add columns to fill
          mutate(
            horizon_bulk_dens_measure = NA,
            horizon_bulk_dens_measure_survey_year = NA,
            horizon_bulk_dens_measure_min = NA,
            horizon_bulk_dens_measure_max = NA,
            horizon_bulk_dens_est = NA,
            horizon_bulk_dens_est_survey_year = NA,
            horizon_bulk_dens_est_min = NA,
            horizon_bulk_dens_est_max = NA,
            horizon_coarse_weight = NA,
            horizon_coarse_weight_survey_year = NA,
            horizon_coarse_weight_min = NA,
            horizon_coarse_weight_max = NA,
            coarse_fragment_vol_avg = NA,
            coarse_fragment_vol_avg_survey_year = NA,
            coarse_fragment_vol_avg_min = NA,
            coarse_fragment_vol_avg_max = NA,
            horizon_clay = NA,
            horizon_clay_min = NA,
            horizon_clay_max = NA,
            horizon_silt = NA,
            horizon_silt_min = NA,
            horizon_silt_max = NA,
            horizon_sand = NA,
            horizon_sand_min = NA,
            horizon_sand_max = NA,
            texture_survey_year = NA)
      }



      # Evaluate per parameter ----

      for (j in seq_along(parameters)) {

        parameter_j <- parameters[j]
        parameter_min_j <- paste0(parameter_j, "_min")
        parameter_max_j <- paste0(parameter_j, "_max")
        parameter_survey_year_j <- paste0(parameter_j, "_survey_year")

        if (grepl("(clay|silt|sand)", parameter_j)) {
          parameter_survey_year_j <- "texture_survey_year"
        }


        parameter_incl_ff <- parameter_ranges$incl_ff[
          which(parameter_ranges$parameter == parameter_j)]

        range_min_j <- parameter_ranges$range_min[
          which(parameter_ranges$parameter == parameter_j)]
        range_max_j <- parameter_ranges$range_max[
          which(parameter_ranges$parameter == parameter_j)]

        df_summ_i <- df_i %>%
          filter(!is.na(.data[[parameter_j]])) %>%
          # Data on texture are gravimetric and need to be weighted
          mutate(weights = case_when(
            grepl("(clay|silt|sand)", parameter_j) ~
              .data$bulk_density_total_soil,
            .default = NA)) %>%
          select(survey_year, unique_survey_repetition,
                 code_layer, layer_number, layer_type,
                 layer_limit_superior, layer_limit_inferior, weights,
                 organic_carbon_total,
                 {{ parameter_j }}) %>%
          filter(.data[[parameter_j]] >= range_min_j &
                   .data[[parameter_j]] <= range_max_j)

        if (grepl("bulk_dens", parameter_j)) {

          df_summ_i <- df_summ_i %>%
            mutate(
              bd_up = case_when(
                layer_type == "mineral" ~ bd_upper(.data$organic_carbon_total),
                layer_type == "peat" ~ bd_peat_upper_95,
                layer_type == "forest_floor" & !grepl("L", code_layer) ~
                  bd_ff_upper_95,
                TRUE ~ quant_bd[2]),
              bd_low = case_when(
                layer_type == "mineral" ~ bd_lower(.data$organic_carbon_total),
                layer_type == "peat" ~ bd_peat_lower_5,
                layer_type == "forest_floor" & !grepl("L", code_layer) ~
                  bd_ff_lower_5,
                TRUE ~ quant_bd[1])) %>%
            filter(.data[[parameter_j]] > bd_low &
                        .data[[parameter_j]] < bd_up) %>%
            select(-bd_low, -bd_up)
        }

        if (nrow(df_summ_i) == 0) {
          # No values of given parameter in given plot
          # Jump to next parameter
          next # Skip to the next iteration of the inner loop (j)
        }


        # Select the best survey_year with data for this parameter
        # Take the survey year covering the biggest depth range after 2000
        # Else, if all equal,
        # Take the survey year with the max number of
        # unique observations per layer after 2000
        # Else, if no data after 2000,
        # Take the most recent survey year

        selection_crit_i <- df_summ_i %>%
          mutate(year_layer = paste0(survey_year, "_", code_layer)) %>%
          group_by(year_layer, layer_limit_superior, layer_limit_inferior,
                   survey_year) %>%
          reframe(count_unique = n_distinct(round(!!sym(parameter_j)))) %>%
          ungroup() %>%
          group_by(survey_year) %>%
          reframe(top = min(layer_limit_superior, na.rm = TRUE),
                  bottom = max(layer_limit_inferior, na.rm = TRUE),
                  count = max(count_unique)) %>%
          ungroup() %>%
          # Truncate to decimeters to avoid little differences in
          # forest floor
          mutate(depth_range = 10 * trunc(0.1 * (.data$bottom - .data$top)))

        after_2000_i <-
          selection_crit_i[selection_crit_i$survey_year >= 2000, ]

        best_year <- if (any(selection_crit_i$survey_year >= 2000)) {
          # If all survey years have data covering the same depth range
          if (length(unique(after_2000_i$depth_range)) == 1) {
            # Use year with max number of repetitions
            after_2000_i %>%
              filter(count == max(.data$count)) %>%
              pull(survey_year) %>%
              max
          } else {
            # Use year with max depth range
            after_2000_i %>%
              filter(depth_range == max(.data$depth_range)) %>%
              pull(survey_year) %>%
              max
          }
        } else {
          # If no survey years are after 2000, find the most recent survey year
          max(selection_crit_i$survey_year)
        }



        # Evaluate per layer ----
        # Summarise values in target dataframe

        for (k in seq_len(nrow(df_target_i))) {

          mean_k <- NA
          rep_k <- NA
          best_year_k <- NA

          if (df_target_i$layer_type[k] != "forest_floor" ||
              (df_target_i$layer_type[k] == "forest_floor" &&
               parameter_incl_ff == TRUE)) {

            ## Forest floor ----

            ## som ----

            if (survey_form_type == "som" &&
                df_target_i$layer_type[k] == "forest_floor" &
                (is.na(df_target_i$layer_limit_superior[k]) |
                 (df_target_i$layer_limit_superior[k] < 0))) {

              # Get the letters to search for in the forest floor

              letters <- paste(unique(strsplit(
                gsub("O", "", df_target_i$code_layer[k]), "")[[1]]),
                collapse = "|")

              if (letters == "") {
                letters <- "O"
              }

              # Priority 1: search for the same code_layer

              mean_k <- df_summ_i %>%
                filter(.data$layer_type == "forest_floor") %>%
                filter(is.na(.data$layer_limit_superior) |
                         .data$layer_limit_superior < 0) %>%
                filter(.data$survey_year == best_year) %>%
                filter(.data$code_layer == df_target_i$code_layer[k]) %>%
                # Take mean across repetitions
                pull(.data[[parameter_j]]) %>%
                round(1) %>%
                unique %>%
                mean %>%
                round(1)

              # Priority 2:
              # If nothing, look for layers containing the letters in FF
              # in same survey_year

              if (is.nan(mean_k)) {

                mean_k <- df_summ_i %>%
                  filter(.data$layer_type == "forest_floor") %>%
                  filter(is.na(.data$layer_limit_superior) |
                           .data$layer_limit_superior < 0) %>%
                  filter(.data$survey_year == best_year) %>%
                  filter(grepl(
                    paste0("[", letters, "]"),
                    code_layer)) %>%
                  # Take mean across repetitions
                  pull(.data[[parameter_j]]) %>%
                  round(1) %>%
                  unique %>%
                  mean %>%
                  round(1)
              }

              # Priority 3:
              # If nothing, look for layers containing the letters in FF

              if (is.nan(mean_k)) {

                mean_k <- df_summ_i %>%
                  filter(.data$layer_type == "forest_floor") %>%
                  filter(is.na(.data$layer_limit_superior) |
                           .data$layer_limit_superior < 0) %>%
                  filter(grepl(
                    paste0("[", letters, "]"),
                    code_layer)) %>%
                  # Take mean across repetitions
                  pull(.data[[parameter_j]]) %>%
                  round(1) %>%
                  unique %>%
                  mean %>%
                  round(1)
              }

              # Priority 4:
              # If nothing, look for any forest floor layer
              # (except of the target layer is "OL")

              if (is.nan(mean_k) &&
                  !identical(letters, "L")) {

                mean_k <- df_summ_i %>%
                  filter(.data$layer_type == "forest_floor") %>%
                  filter(is.na(.data$layer_limit_superior) |
                           .data$layer_limit_superior < 0) %>%
                  # Take mean across repetitions
                  pull(.data[[parameter_j]]) %>%
                  round(1) %>%
                  unique %>%
                  mean %>%
                  round(1)
              }

              if (is.nan(mean_k)) {
                mean_k <- NA
                rep_k <- NA
              } else {

                best_year_k <- best_year

              # Range

              match_types_i <- df_summ_i %>%
                filter(.data$layer_type == "forest_floor") %>%
                filter(is.na(.data$layer_limit_superior) |
                         .data$layer_limit_superior < 0) %>%
                group_by(unique_survey_repetition) %>%
                reframe(match_code_layer =
                          any(.data$code_layer == df_target_i$code_layer[k]),
                        match_letter =
                          any(grepl(
                            paste0("[", letters, "]"),
                            code_layer)),
                        match_ff =
                          any(.data$layer_type == "forest_floor")) %>%
                mutate(match_type = case_when(
                  match_code_layer == TRUE ~ "match_code_layer",
                  match_letter == TRUE ~ "match_letter",
                  match_ff == TRUE ~ "match_ff")) %>%
                select(-match_code_layer, -match_letter, -match_ff)

              rep_k <- df_summ_i %>%
                filter(.data$layer_type == "forest_floor") %>%
                filter(is.na(.data$layer_limit_superior) |
                         .data$layer_limit_superior < 0) %>%
                left_join(match_types_i, by = "unique_survey_repetition") %>%
                filter((match_type == "match_code_layer" &
                          code_layer == df_target_i$code_layer[k]) |
                         (match_type == "match_letter" &
                            grepl(paste0("[", letters, "]"), code_layer)) |
                         (match_type == "match_ff"))

              # If litter layer: exclude general layers

              if (nrow(rep_k) > 0 &&
                  identical("L", letters)) {

                rep_k <- rep_k %>%
                  filter(match_type != "match_ff")
              }

              if (nrow(rep_k) > 0) {

                rep_k <- rep_k %>%
                  # Summarise per repetition
                  group_by(unique_survey_repetition) %>%
                  reframe(variab =
                            mean(.data[[parameter_j]])) %>%
                  rename_with(~ parameter_j, variab) %>%
                  # Take mean across repetitions
                  pull(.data[[parameter_j]]) %>%
                  round(1) %>%
                  unique
              }
              }
            }



            ## pfh ----

            if (survey_form_type == "pfh" &&
                df_target_i$layer_type[k] == "forest_floor" &
                (is.na(df_target_i$layer_limit_superior[k]) |
                 (df_target_i$layer_limit_superior[k] < 0))) {

              # Retrieve the corresponding layers in "som" with which
              # the given layer was manually matched

              matching_layers_som <-
                link_forest_floors %>%
                filter(profile_id == selected_prof_i) %>%
                filter(code_layer == df_target_i$code_layer[k]) %>%
                pull(code_layers_link) %>%
                unique

              matching_survey_som <-
                link_forest_floors %>%
                filter(profile_id == selected_prof_i) %>%
                filter(code_layer == df_target_i$code_layer[k]) %>%
                pull(unique_survey_link) %>%
                unique

              if (!identical(matching_layers_som, character(0)) &&
                  !identical(matching_survey_som, character(0))) {

                rep_k <- som %>%
                  filter(unique_survey %in% matching_survey_som) %>%
                  filter(code_layer %in%
                           unlist(strsplit(matching_layers_som, "_")))

                if (nrow(rep_k) == 0) {
                  next # Skip to the next iteration of the inner loop (k)
                }

                best_year_k <-
                  as.numeric(unlist(strsplit(matching_survey_som, "_"))[2])

                rep_k <- rep_k %>%
                  group_by(unique_survey_repetition) %>%
                  reframe(bulk_density = case_when(
                    # If any layer thickness of the forest floor is unknown
                    # Just take the summary
                    any(is.na(layer_thickness)) ~ mean(bulk_density),
                    # If all layer thicknesses of the forest floor are known
                    # Take a weighted average using the layer thicknesses
                    all(!is.na(layer_thickness)) ~
                      weighted.mean(bulk_density,
                                    w = layer_thickness))) %>%
                  # Take unique values because it is common to copy paste
                  mutate(var_round = round(bulk_density, 1)) %>%
                  distinct(var_round, .keep_all = TRUE) %>%
                  pull(bulk_density)

                mean_k <- mean(rep_k)
                rep_k <- round(rep_k, 1)

              }
            }

            ## Below-ground ----

            if (df_target_i$layer_type[k] != "forest_floor" ||
                (!is.na(df_target_i$layer_limit_superior[k]) &&
                  df_target_i$layer_limit_superior[k] >= 0)) {

              if (is.na(df_target_i$layer_limit_superior[k]) ||
                  is.na(df_target_i$layer_limit_inferior[k])) {

                rep_k <- df_summ_i %>%
                  filter(code_layer == df_target_i$code_layer[k]) %>%
                  # Take unique values because it is common to copy paste
                  mutate(var_round = round(.data[[parameter_j]], 1)) %>%
                  distinct(var_round, .keep_all = TRUE) %>%
                  pull(.data[[parameter_j]])

                if (identical(rep_k, integer(0)) ||
                    identical(rep_k, numeric(0))) {
                  rep_k <- NA
                  mean_k <- NA
                } else {
                  mean_k <- mean(rep_k)
                  rep_k <- round(rep_k, 1)
                  best_year_k <- best_year
                }
              } else {


              depth_range_k <-
                seq(round(df_target_i$layer_limit_superior[k], 1),
                    round(df_target_i$layer_limit_inferior[k], 1),
                    by = 0.1)

              # Data for year that is selected

              mean_k <- df_summ_i %>%
                filter(.data$survey_year == best_year) %>%
                filter(!is.na(layer_limit_superior) &
                         !is.na(layer_limit_inferior)) %>%
                filter(layer_limit_inferior > 0) %>%
                rowwise() %>%
                filter(any(depth_range_k > .data$layer_limit_superior &
                             depth_range_k < .data$layer_limit_inferior))

              if (nrow(mean_k) == 0) {

                mean_k <- NA

              } else if (nrow(mean_k) == 1) {

                mean_k <- mean_k %>%
                  pull(.data[[parameter_j]]) %>%
                  mean %>%
                  round(1) %>%
                  # Because sometimes values are clearly just copy-pasted
                  unique

                best_year_k <- best_year

              } else {

              mean_k <- mean_k %>%
                # Summarise per repetition
                group_by(unique_survey_repetition) %>%
                reframe(variab =
                          harmonise_layer_to_depths(
                            limit_sup =
                              round(df_target_i$layer_limit_superior[k], 1),
                            limit_inf =
                              round(df_target_i$layer_limit_inferior[k], 1),
                            bulk_density = .data$weights,
                            upper_depths =
                              .data$layer_limit_superior,
                            lower_depths =
                              .data$layer_limit_inferior,
                            variab =
                              .data[[parameter_j]],
                            parameter_name =
                              "parameter_j"
                          )) %>%
                rename_with(~ parameter_j, variab) %>%
                # Take mean across repetitions
                pull(.data[[parameter_j]]) %>%
                mean %>%
                round(1) %>%
                # Because sometimes values are clearly just copy-pasted
                unique

              best_year_k <- best_year

              }

              # Replicate data across all profiles and survey years
              # for given depth

              rep_k <- df_summ_i %>%
                filter(!is.na(layer_limit_superior) &
                         !is.na(layer_limit_inferior)) %>%
                rowwise() %>%
                filter(any(depth_range_k > .data$layer_limit_superior &
                             depth_range_k < .data$layer_limit_inferior)) %>%
                filter(layer_limit_inferior > 0)

              # If no data exist for the best survey year but there are any
              # data for other survey years:
              # use information from other survey years

              if (is.na(mean_k) &&
                  nrow(rep_k) > 0) {

                best_year_k <- max(unique(rep_k$survey_year))

                mean_k <- rep_k %>%
                  # Summarise per repetition
                  group_by(unique_survey_repetition) %>%
                  reframe(variab =
                            harmonise_layer_to_depths(
                              limit_sup =
                                round(df_target_i$layer_limit_superior[k], 1),
                              limit_inf =
                                round(df_target_i$layer_limit_inferior[k], 1),
                              bulk_density = weights,
                              upper_depths =
                                .data$layer_limit_superior,
                              lower_depths =
                                .data$layer_limit_inferior,
                              variab =
                                .data[[parameter_j]],
                              parameter_name =
                                "parameter_j"
                            )) %>%
                  rename_with(~ parameter_j, variab) %>%
                  # Take mean across repetitions
                  pull(.data[[parameter_j]]) %>%
                  mean %>%
                  round(1) %>%
                  # Because sometimes values are clearly just copy-pasted
                  unique

              }

              if (nrow(rep_k) == 0) {

                rep_k <- NA

              } else if (nrow(rep_k) == 1) {

                rep_k <- rep_k %>%
                  pull(.data[[parameter_j]]) %>%
                  round(1)

              } else {

              rep_k <- rep_k %>%
                # Summarise per repetition
                group_by(unique_survey_repetition) %>%
                reframe(variab =
                          harmonise_layer_to_depths(
                            limit_sup =
                              round(df_target_i$layer_limit_superior[k], 1),
                            limit_inf =
                              round(df_target_i$layer_limit_inferior[k], 1),
                            bulk_density = weights,
                            upper_depths =
                              .data$layer_limit_superior,
                            lower_depths =
                              .data$layer_limit_inferior,
                            variab =
                              .data[[parameter_j]],
                            parameter_name =
                              "parameter_j"
                          )) %>%
                rename_with(~ parameter_j, variab) %>%
                # Take unique values because it is common to copy paste
                mutate(var_round = round(.data[[parameter_j]], 1)) %>%
                distinct(var_round, .keep_all = TRUE) %>%
                pull(.data[[parameter_j]]) %>%
                round(1)

              }
              }

              } # End of "if k is not forest floor"

              if (parameter_j %in% c("bulk_density",
                                     "horizon_bulk_dens_measure",
                                     "horizon_bulk_dens_est")) {

                mean_k <- round(mean_k)
                rep_k <- round(rep_k)
              }

              if (survey_form_type == "pfh" &&
                  parameter_j == "coarse_fragment_vol_avg" &&
                  !identical(rep_k, NA)) {

                # Ideally, all values for this are the same
                # since this variable is derived from the categorical
                # code_horizon_coarse_vol

                # assertthat::assert_that(
                #   length(unique(rep_k)) == 1)

                # Add minimum and maximum of class
                # for variation

                d_soil_coarse_fragments_k <- d_soil_coarse_fragments %>%
                  rowwise() %>%
                  filter(any(unique(rep_k) >= .data$coarse_fragment_vol_min &
                           unique(rep_k) <= .data$coarse_fragment_vol_max))

                rep_k <- c(
                  rep_k,
                  # Minimum
                  d_soil_coarse_fragments_k$coarse_fragment_vol_min,
                  # Maximum
                  d_soil_coarse_fragments_k$coarse_fragment_vol_max)

              }

              df_target_i[[parameter_j]][k] <- mean_k
              df_target_i[[parameter_min_j]][k] <- min(rep_k)
              df_target_i[[parameter_max_j]][k] <- max(rep_k)
              df_target_i[[parameter_survey_year_j]][k] <- best_year_k

          }

        } # End of layer k

      } # End of for loop parameters within plot_id

      df_target <- bind_rows(df_target,
                             df_target_i)

      # Update progress bar

      if (!isTRUE(getOption("knitr.in.progress"))) {
        setTxtProgressBar(progress_bar, i)
      }

    } # End of for loop plot_ids


    if (!isTRUE(getOption("knitr.in.progress"))) {
      close(progress_bar)
    }

} # End of "if pfh/som"



    if (survey_form_type == "pfh") {

      df_target <- df_target %>%
        mutate(
          bulk_density_harm = coalesce(
            horizon_bulk_dens_measure, horizon_bulk_dens_est),
          bulk_density_harm_min = coalesce(
            horizon_bulk_dens_measure_min, horizon_bulk_dens_est_min),
          bulk_density_harm_max = coalesce(
            horizon_bulk_dens_measure_max, horizon_bulk_dens_est_max)) %>%
        mutate(
          # Coarse fragments: vol% converted from weight%
          # Mean
          coarse_fragment_aid =
            ifelse(!is.na(bulk_density_harm) & !is.na(horizon_coarse_weight),
                   (.data$bulk_density_harm *
                      (.data$horizon_coarse_weight /
                         (100 - .data$horizon_coarse_weight))) / 2650,
                   NA),
          coarse_fragment_vol_converted =
            ifelse(!is.na(.data$coarse_fragment_aid),
                   round(as.numeric((.data$coarse_fragment_aid /
                                       (1 + .data$coarse_fragment_aid)) * 100),
                         3),
                   NA),
          # Minimum:
          # Use min bulk density to obtain the lowest volumetric CF estimate
          coarse_fragment_aid =
            ifelse(!is.na(bulk_density_harm_min) &
                     !is.na(horizon_coarse_weight_min),
                   (.data$bulk_density_harm_min *
                      (.data$horizon_coarse_weight_min /
                         (100 - .data$horizon_coarse_weight_min))) / 2650,
                   NA),
          coarse_fragment_vol_converted_min =
            ifelse(!is.na(.data$coarse_fragment_aid),
                   round(as.numeric((.data$coarse_fragment_aid /
                                       (1 + .data$coarse_fragment_aid)) * 100),
                         3),
                   NA),
          # Maximum:
          # Use max bulk density to obtain the highest volumetric CF estimate
          coarse_fragment_aid =
            ifelse(!is.na(bulk_density_harm_max) &
                     !is.na(horizon_coarse_weight_max),
                   (.data$bulk_density_harm_max *
                      (.data$horizon_coarse_weight_max /
                         (100 - .data$horizon_coarse_weight_max))) / 2650,
                   NA),
          coarse_fragment_vol_converted_max =
            ifelse(!is.na(.data$coarse_fragment_aid),
                   round(as.numeric((.data$coarse_fragment_aid /
                                       (1 + .data$coarse_fragment_aid)) * 100),
                         3),
                   NA)) %>%
        select(-coarse_fragment_aid, -horizon_coarse_weight,
               -horizon_coarse_weight_min, -horizon_coarse_weight_max,
               -bulk_density_harm, -bulk_density_harm_min,
               -bulk_density_harm_max) %>%
        rename(coarse_fragment_vol_converted_survey_year =
                 horizon_coarse_weight_survey_year) %>%
        relocate(coarse_fragment_vol_converted,
                 .before = coarse_fragment_vol_converted_survey_year) %>%
        relocate(coarse_fragment_vol_converted_min,
                 .after = coarse_fragment_vol_converted_survey_year) %>%
        relocate(coarse_fragment_vol_converted_max,
                 .after = coarse_fragment_vol_converted_min)

    }


      # swc ----

      if (survey_form_type == "swc") {

        if (!isTRUE(getOption("knitr.in.progress"))) {
          progress_bar <- txtProgressBar(min = 0,
                                         max = length(unique(df$plot_id)),
                                         style = 3)
        }

        for (i in seq_along(unique(df$plot_id))) {

          plot_id_i <- unique(df$plot_id)[i]

          df_i <- df %>%
            filter(plot_id == plot_id_i) %>%
            ungroup

          vec_target <- which(df_target$plot_id == plot_id_i)

          for (j in seq_along(parameters)) {

            parameter_j <- parameters[j]
            parameter_min_j <- paste0(parameter_j, "_min")
            parameter_max_j <- paste0(parameter_j, "_max")
            parameter_survey_year_j <- paste0(parameter_j, "_survey_year")

            range_min_j <- parameter_ranges$range_min[
              which(parameter_ranges$parameter == parameter_j)]
            range_max_j <- parameter_ranges$range_max[
              which(parameter_ranges$parameter == parameter_j)]

            df_summ_i <- df_i %>%
              filter(!is.na(.data[[parameter_j]])) %>%
              # Data on texture are gravimetric and need to be weighted
              mutate(weights = NA) %>%
              select(survey_year, unique_survey_repetition,
                     code_layer, layer_type,
                     layer_limit_superior, layer_limit_inferior, weights,
                     {{ parameter_j }}) %>%
              # As we don't know the TOC content for these samples,
              # we just use the minimum and maximum plausible values
              # of the formulas to create the upper and lower boundaries
              # (bd_upper and bd_lower)
              filter(.data[[parameter_j]] >= range_min_j &
                       .data[[parameter_j]] <= range_max_j)

            if (nrow(df_summ_i) == 0) {
              next
            }

            best_year <- max(df_summ_i$survey_year)


            # Evaluate per layer ----
            # Summarise values in target dataframe

            for (k in vec_target) {

              mean_k <- NA
              rep_k <- NA
              best_year_k <- NA
              top_k <- NA
              bottom_k <- NA

              depth_range_k <-
                seq(round(df_target$layer_limit_superior_theor[k], 1),
                    round(df_target$layer_limit_inferior_theor[k], 1),
                    by = 0.1)

              # Data for year that is selected

              mean_k <- df_summ_i %>%
                filter(.data$survey_year == best_year) %>%
                rowwise() %>%
                filter(any(depth_range_k > .data$layer_limit_superior &
                             depth_range_k < .data$layer_limit_inferior))

              if (nrow(mean_k) == 0) {

                mean_k <- NA

              } else {

                top_k <- max(min(mean_k$layer_limit_superior),
                             df_target$layer_limit_superior_theor[k])
                bottom_k <- min(max(mean_k$layer_limit_inferior),
                                df_target$layer_limit_inferior_theor[k])

                mean_k <- mean_k %>%
                  # Summarise per repetition
                  group_by(unique_survey_repetition) %>%
                  reframe(variab =
                            harmonise_layer_to_depths(
                              limit_sup =
                                round(df_target$layer_limit_superior_theor[k],
                                      1),
                              limit_inf =
                                round(df_target$layer_limit_inferior_theor[k],
                                      1),
                              bulk_density = weights,
                              upper_depths =
                                .data$layer_limit_superior,
                              lower_depths =
                                .data$layer_limit_inferior,
                              variab =
                                .data[[parameter_j]],
                              parameter_name =
                                "parameter_j"
                            )) %>%
                  rename_with(~ parameter_j, variab) %>%
                  # Take mean across repetitions
                  pull(.data[[parameter_j]]) %>%
                  mean %>%
                  round(1) %>%
                  # Because sometimes values are clearly just copy-pasted
                  unique

                best_year_k <- best_year

              }

              # Replicate data across all profiles and survey years
              # for given depth

              rep_k <- df_summ_i %>%
                rowwise() %>%
                filter(any(depth_range_k > .data$layer_limit_superior &
                             depth_range_k < .data$layer_limit_inferior))

              # If no data exist for the best survey year but there are any
              # data for other survey years:
              # use information from other survey years

              if (is.na(mean_k) &&
                  nrow(rep_k) > 0) {

                best_year_k <- max(unique(rep_k$survey_year))

                top_k <- max(min(rep_k$layer_limit_superior),
                             df_target$layer_limit_superior_theor[k])
                bottom_k <- min(max(rep_k$layer_limit_inferior),
                                df_target$layer_limit_inferior_theor[k])

                mean_k <- rep_k %>%
                  # Summarise per repetition
                  group_by(unique_survey_repetition) %>%
                  reframe(variab =
                            harmonise_layer_to_depths(
                              limit_sup =
                                round(df_target$layer_limit_superior_theor[k],
                                      1),
                              limit_inf =
                                round(df_target$layer_limit_inferior_theor[k],
                                      1),
                              bulk_density = weights,
                              upper_depths =
                                .data$layer_limit_superior,
                              lower_depths =
                                .data$layer_limit_inferior,
                              variab =
                                .data[[parameter_j]],
                              parameter_name =
                                "parameter_j"
                            )) %>%
                  rename_with(~ parameter_j, variab) %>%
                  # Take mean across repetitions
                  pull(.data[[parameter_j]]) %>%
                  mean %>%
                  round(1) %>%
                  # Because sometimes values are clearly just copy-pasted
                  unique

              }

              if (nrow(rep_k) == 0) {

                rep_k <- NA

              } else {

                rep_k <- rep_k %>%
                  # Summarise per repetition
                  group_by(unique_survey_repetition) %>%
                  reframe(variab =
                            harmonise_layer_to_depths(
                              limit_sup =
                                round(df_target$layer_limit_superior_theor[k],
                                      1),
                              limit_inf =
                                round(df_target$layer_limit_inferior_theor[k],
                                      1),
                              bulk_density = weights,
                              upper_depths =
                                .data$layer_limit_superior,
                              lower_depths =
                                .data$layer_limit_inferior,
                              variab =
                                .data[[parameter_j]],
                              parameter_name =
                                "parameter_j"
                            )) %>%
                  rename_with(~ parameter_j, variab) %>%
                  # Take unique values because it is common to copy paste
                  mutate(var_round = round(.data[[parameter_j]], 1)) %>%
                  distinct(var_round, .keep_all = TRUE) %>%
                  pull(.data[[parameter_j]]) %>%
                  round(1)

              }


              df_target[[parameter_j]][k] <- mean_k
              df_target[[parameter_min_j]][k] <- min(rep_k)
              df_target[[parameter_max_j]][k] <- max(rep_k)
              df_target[[parameter_survey_year_j]][k] <- best_year_k
              df_target$layer_limit_superior[k] <- top_k
              df_target$layer_limit_inferior[k] <- bottom_k





              } # End of layer k

          }

          # Update progress bar

          if (!isTRUE(getOption("knitr.in.progress"))) {
            setTxtProgressBar(progress_bar, i)
          }

        } # End of for loop plot_ids


        if (!isTRUE(getOption("knitr.in.progress"))) {
          close(progress_bar)
        }


        df_target <- df_target %>%
          filter(!is.na(bulk_density)) %>%
          group_by(plot_id) %>%
          mutate(layer_number = rank(layer_number)) %>%
          ungroup() %>%
          select(-layer_limit_superior_theor, -layer_limit_inferior_theor)



      } # End of "if sw_swc"


    return(df_target)

  }
