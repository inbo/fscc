
check_stock_plausibility <- function(data_frame,
                                     parameter = "organic_carbon_total",
                                     shorter_var_name = "oc",
                                     survey_form_orig) {

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("mpspline2"),
            require("ggplot2"),
            require("rempsyc"),
            require("patchwork"),
            require("xgboost"),
            require("boot"),
            require("bootstrap"),
            require("triangle"),
            require("broom"))

  if (length(unlist(strsplit(survey_form_orig, "_"))) == 2) {

    s1_pre <- paste0("s1_", unlist(strsplit(survey_form_orig, "_"))[2], "_")
    so_pre <- paste0("so_", unlist(strsplit(survey_form_orig, "_"))[2], "_")

  } else if (length(unlist(strsplit(survey_form_orig, "_"))) == 1) {

    s1_pre <- "s1_"
    so_pre <- "so_"
  }




  cat(paste0(" \nCheck plausibility of '", parameter, "' stocks\n"))

  source("./src/functions/get_env.R")

  df <- data_frame %>%
    mutate(
      plaus_total_fscc = TRUE,
      plaus_forest_floor_fscc = TRUE,
      use_stock_topsoil = FALSE,
      implaus_fscc_reason = NA)

  # Assert that these variables exist

  assertthat::assert_that(
    all(c("plot_id", "survey_year", "survey_form") %in% names(df)))

  # There should be one record per survey year and per survey form for each plot

  assertthat::assert_that(
    nrow(df) ==
      df %>%
      mutate(unique_surveys = paste0(plot_id, "_",
                                    survey_year, "_",
                                    survey_form)) %>%
      pull(unique_surveys) %>%
      n_distinct)

  # Retrieve code_survey

  code_survey <- unique(sapply(str_split(unique(df$survey_form), "_"),
                               function(x) x[1]))

  assertthat::assert_that(length(code_survey) == 1)

  # Retrieve data per layer

  df_layer <- get_env(paste0(survey_form_orig, "_layers"))


  # If TOC ----

  if (parameter == "organic_carbon_total") {


    # Retrieve data per profile

    profile_stocks <- get_env(paste0(code_survey, "_profile_oc_stocks"))

    # Define plausible limits ----

    if (exists("plot_oc_stocks_summ")) {

      plaus_range_stock <- get_env("plot_oc_stocks_summ") %>%
        pull(stock) %>%
        quantile(c(0.005, 0.995), na.rm = TRUE) %>%
        round(1)

      plaus_range_stock_change <- get_env("plot_oc_stocks_summ") %>%
        pull(stock_change) %>%
        quantile(c(0.05, 0.95), na.rm = TRUE) %>%
        round(1) # approximately +-5 t C ha-1 year-1

      plaus_range_stock_change_rel <- get_env("plot_oc_stocks_summ") %>%
        pull(stock_change_rel) %>%
        quantile(c(0.005, 0.995), na.rm = TRUE) %>%
        round(1) # approximately +-10 % year-1
    }

    plaus_range_stock <- c(21, 894)
    plaus_range_stock_change <- c(-5, 5)
    plaus_range_stock_change_rel <- c(-10, 10)


    if (exists("so_layers") &&
        exists("s1_layers")) {

      plaus_range_olw <- bind_rows(get_env("so_layers"),
                                   get_env("s1_layers")) %>%
        group_by(plot_id, survey_form, survey_year) %>%
        reframe(organic_layer_weight =
                  sum(organic_layer_weight, na.rm = TRUE)) %>%
        arrange(organic_layer_weight) %>%
        pull(organic_layer_weight) %>%
        quantile(c(0.975), na.rm = TRUE) %>%
        round(1)

      plaus_range_toc_ff <- bind_rows(get_env("so_layers"),
                                      get_env("s1_layers")) %>%
        filter(layer_type == "forest_floor") %>%
        pull(parameter_for_stock) %>%
        quantile(c(0.005, 0.995), na.rm = TRUE) %>%
        round(1)

      plaus_range_toc_bg <- bind_rows(get_env("so_layers"),
                                      get_env("s1_layers")) %>%
        group_by(plot_id, survey_form, survey_year) %>%
        reframe(toc_max = ifelse(
          any(!is.na(parameter_for_stock[which(layer_type != "forest_floor")])),
          max(parameter_for_stock[which(layer_type != "forest_floor")],
              na.rm = TRUE),
          NA_real_)) %>%
        pull(toc_max) %>%
        quantile(c(0.005), na.rm = TRUE) %>%
        round(1)

      plaus_range_oc_density <- bind_rows(get_env("so_layers"),
                                         get_env("s1_layers")) %>%
        pull(density) %>%
        quantile(c(0.975), na.rm = TRUE) %>%
        round(1)

      plaus_range_toc_ff_change <- bind_rows(get_env("so_layers"),
                                             get_env("s1_layers")) %>%
        group_by(plot_id, survey_form, survey_year) %>%
        reframe(toc_avg = ifelse(
          any(!is.na(parameter_for_stock[which(layer_type == "forest_floor")])),
          mean(parameter_for_stock[which(layer_type == "forest_floor")],
               na.rm = TRUE),
          NA_real_)) %>%
        filter(!is.na(toc_avg)) %>%
        # Calculate slope of linear regression
        # (the TOc should not change too drastically)
        group_by(plot_id, survey_form) %>%
        filter(n() >= 2) %>%  # Filter groups with at least two records
        do(broom::tidy(lm(toc_avg ~ survey_year, data = .))) %>%
        filter(term == "survey_year") %>%
        select(plot_id, survey_form, estimate) %>%
        rename(slope_toc_ff = estimate) %>%
        pull(slope_toc_ff) %>%
        quantile(c(0.025, 0.975), na.rm = TRUE) %>%
        round(1)

    }

    plaus_range_olw <- 64
    plaus_range_toc_ff <- c(42, 567)
    plaus_range_toc_bg <- 3
    plaus_range_oc_density <- 8
    plaus_range_toc_ff_change <- c(-25, 25)

    # Define rules ----

    ## Total/below-ground stock ----

    # If these rules are not met, the total stock or at least the below-ground
    # stock is considered implausible, which implies that FSCC recommends not
    # to use the stock data from the given plot survey.

    # First set of rules: plausibility of a values
    # within a certain plot x survey_year.
    # Stock flagged as implausible if not met.

    rule_1 <- "Stock should be between 21 and 894 t C ha-1 (99 % quantile)."

    rule_2 <- paste0("Max. below-ground total organic carbon should be ",
                     "above 3 g kg-1 (99 % quantile).")

    rule_3 <- "Carbon density should be below 8 t C ha-1 cm-1 (~95 % quantile)."

    # Second set of rules: plausibility of changes between survey years
    # from a certain plot.
    # Stock of earliest survey year flagged as implausible if not met.

    rule_4 <- paste0("Annual sequestration rate should be ",
                     "between -5 and 5 t C ha-1 year-1 (~90 % quantile).")

    rule_5 <- paste0("Rel. annual sequestration rate should be ",
                     "between -10 and 10 % year-1 (99 % quantile).")




    ## Subsoil stock ----

    rule_6 <- paste0("Observation depth should be >= 30 cm or ",
                     ">= 70 % of eff_soil_depth.")




    ## Forest floor stock ----

    # If these rules are not met, the forest floor stock is considered
    # implausible, which implies that below-ground stocks (stock_below_ground
    # and stock_below_ground_topsoil) can still be used if plaus_total_fscc is
    # TRUE.
    # FSCC only recommends not to use the stock data from the forest floor
    # (stock_forest_floor) and total stock (stock and stock_topsoil), as these
    # include forest floor stocks.

    # First set of rules: plausibility of a values
    # within a certain plot x survey_year.
    # Stock flagged as implausible if not met.

    rule_7 <- paste0("Plots should have non-OL forest floor layers, ",
                     "unless their humus form is Mull.")

    rule_8 <- "Non-OL layers should have a known organic layer weight."

      # Note that OLW is not included in "pfh" survey forms. However, it
      # is gap-filled based on the layer thickness if possible, and without
      # this, we can still not consider the total stock as plausible;
      # hence, we still flag "pfh" plot surveys as implausible if the
      # forest floor cannot be calculated.

    rule_9 <- "Organic layer weight should be below 64 kg m-2 (95 % quantile)."

    rule_10 <- paste0("Total organic carbon of forest floor should be ",
                      "between 42 and 567 g kg-1 (99 % quantile).")

    # Second set of rules: plausibility of changes between survey years
    # from a certain plot.
    # Stock of earliest survey year flagged as implausible if not met.

    rule_11 <- paste0("Change rate of mean forest floor total organic carbon ",
                      "should be between -25 and 25 g kg-1 year-1 ",
                      "(~95 % quantile).")





    # plausible_fscc
    # implausible_fscc_reason

    # Evaluate for different plots ----

    plot_ids <- unique(df$plot_id)

    # Set progress bar
    progress_bar <- txtProgressBar(min = 0,
                                   max = length(plot_ids), style = 3)

    for (i in seq_along(plot_ids)) {

      vec_i <- which(df$plot_id == plot_ids[i])

      vec_som_i <- which(df$plot_id == plot_ids[i] &
                           grepl("som", df$survey_form))

      vec_pfh_i <- which(df$plot_id == plot_ids[i] &
                           grepl("pfh", df$survey_form))

      df_i <- df %>%
        filter(plot_id == plot_ids[i])

      ## Total/below-ground stock ----

      # If these rules are not met, the total stock or at least the below-ground
      # stock is considered implausible, which implies that FSCC recommends not
      # to use the stock data from the given plot survey.


      ## Individual plot surveys ----

      for (j in vec_i) {

        implausible_reasons_j <- NULL

        ### rule_1 ----
        # "Stock should be between 21 and 894 t C ha-1 (99 % quantile)"

        if (df$stock[j] < plaus_range_stock[1] ||
            df$stock[j] > plaus_range_stock[2]) {

          df$plaus_total_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_1,
                                         implausible_reasons_j)
        }


        ### rule_2 ----
        # "Max. below-ground total organic carbon should be
        #  above 3 g kg-1 (99 % quantile)."

        toc_bg_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type != "forest_floor") %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
          reframe(toc_max = ifelse(
            any(!is.na(parameter_for_stock[which(layer_type != "forest_floor")])),
            max(parameter_for_stock[which(layer_type != "forest_floor")],
                na.rm = TRUE),
            NA_real_)) %>%
          filter(!is.na(toc_max)) %>%
          pull(toc_max)

        if (any(toc_bg_j < plaus_range_toc_bg)) {

          df$plaus_total_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_2,
                                         implausible_reasons_j)
        }



        ### rule_3 ----
        # "Carbon density should be below 8 t C ha-1 cm-1 (~95 % quantile)."

        c_dens_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(!is.na(density)) %>%
          pull(density)

        if (any(c_dens_j > plaus_range_oc_density)) {

          df$plaus_total_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_3,
                                         implausible_reasons_j)
        }



        if (!identical(implausible_reasons_j, NULL)) {

          df$implaus_fscc_reason[j] <- implausible_reasons_j

        }

      } # End of j in vec_i



      ## Change between surveys for individual plot ----

      for (vec_selected_i in c("vec_som_i", "vec_pfh_i")) {

        vec_sel_i <- get(vec_selected_i)

        if (identical(vec_sel_i, integer(0))) {
          next
        }

        if (length(vec_sel_i) <= 1) {
          next
        }

        vec_sel_i <- vec_sel_i[order(df$survey_year[vec_sel_i])]

        for (j in vec_sel_i) {

          implausible_reasons_j <- NULL

          # Evaluate based on the earliest survey,
          # assuming that this one is overall less reliable

          if (which(j == vec_sel_i) == length(vec_sel_i)) {
            next
          }

          # Row index of next survey year to compare with

          j_next <- vec_sel_i[which(j == vec_sel_i) + 1]


          ### rule_4 ----
          # "Annual sequestration rate should be
          #  between -5 and 5 t C ha-1 year-1 (~90 % quantile)."

          seq_rate_j <- (df$stock[j_next] - df$stock[j]) /
            (df$survey_year[j_next] - df$survey_year[j])

          if (seq_rate_j < plaus_range_stock_change[1] ||
              seq_rate_j > plaus_range_stock_change[2]) {

            df$plaus_total_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_4,
                                           implausible_reasons_j)
          }



          ### rule_5 ----
          # "Rel. annual sequestration rate should be
          # between -10 and 10 % year-1 (99 % quantile)."

          seq_rate_rel_j <-
            ((df$stock[j_next] - df$stock[j]) /
               (df$survey_year[j_next] - df$survey_year[j])) / df$stock[j] * 100

          if (seq_rate_rel_j < plaus_range_stock_change_rel[1] ||
              seq_rate_rel_j > plaus_range_stock_change_rel[2]) {

            df$plaus_total_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_5,
                                           implausible_reasons_j)
          }



          if (!identical(implausible_reasons_j, NULL)) {

            if (!is.na(df$implaus_fscc_reason[j])) {

              df$implaus_fscc_reason[j] <-
                paste(df$implaus_fscc_reason[j],
                      implausible_reasons_j)
            } else {

              df$implaus_fscc_reason[j] <- implausible_reasons_j

            }
          }

        } # End of j (with j_next) in vec_i

      } # End of som versus pfh





      ## Subsoil stock ----

      for (j in vec_i) {

        implausible_reasons_j <- NULL

        ### rule_6 ----
        # "Observation depth should be >= 30 cm or >= 70 % of eff_soil_depth."

        obs_depth_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type != "forest_floor")

        if ("gapfilled_post_layer1" %in% names(obs_depth_j)) {

          obs_depth_j <- obs_depth_j %>%
            filter(is.na(gapfilled_post_layer1) |
                     gapfilled_post_layer1 == "")
        }

        use_stock_topsoil_j <- obs_depth_j %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
          reframe(obs_depth = max(depth_bottom, na.rm = TRUE),
                  depth_stock = unique(depth_stock)) %>%
          rowwise() %>%
          mutate(
            use_stock_topsoil =
              (.data$obs_depth < 30 &
                 .data$obs_depth < 0.7 * .data$depth_stock)) %>%
          ungroup() %>%
          pull(use_stock_topsoil)


        # If one repetition is too shallow, while others are fine,
        # it is still fine to use the subsoil stock
        # i.e., only flag the plot's stock as implausible if all repetitions
        # are too shallow
        # (e.g. 5_22 in Level II)

        if (all(use_stock_topsoil_j == TRUE)) {

          df$use_stock_topsoil[j] <- TRUE
          implausible_reasons_j <- paste(rule_6,
                                         implausible_reasons_j)
        }



        if (!identical(implausible_reasons_j, NULL)) {

          if (!is.na(df$implaus_fscc_reason[j])) {

            df$implaus_fscc_reason[j] <-
              paste(df$implaus_fscc_reason[j],
                    implausible_reasons_j)
          } else {

            df$implaus_fscc_reason[j] <- implausible_reasons_j

          }
        }

      }






      ## Forest floor stock ----

      # If these rules are not met, the forest floor stock is considered
      # implausible, which implies that below-ground stocks (stock_below_ground
      # and stock_below_ground_topsoil) can still be used if plaus_total_fscc
      # is TRUE.
      # FSCC only recommends not to use the stock data from the forest floor
      # (stock_forest_floor) and total stock (stock and stock_topsoil), as these
      # include forest floor stocks.

      ## Individual plot surveys ----

      for (j in vec_i) {

        implausible_reasons_j <- NULL


        ### rule_7 ----
        # "Plots should have non-OL forest floor layers
        #  unless their humus form is Mull."

        ff_layers_j <- df_layer %>%
          # Layers containing "L" are already filtered out
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type == "forest_floor")

        # No need to further group it per profile and then per plot, because
        # any non-OL forest floor layers in whichever profile are already
        # sufficient

        if (nrow(ff_layers_j) == 0 &&
            !grepl("mull", df$humus_form[j], ignore.case = TRUE)) {

          df$plaus_forest_floor_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_7,
                                         implausible_reasons_j)

        }



        ### rule_8 ----
        # "Non-OL layers should have a known organic layer weight."

        ff_olw_pres_j <- df_layer %>%
          # Layers containing "L" are already filtered out
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type == "forest_floor") %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
          reframe(
            all_pres_olw = all(!is.na(organic_layer_weight))) %>%
          ungroup()

        # As soon as one of the profiles has a complete set of OLW data,
        # we can estimate a forest floor carbon stock

        if (nrow(ff_olw_pres_j) > 0) {

          if (all(ff_olw_pres_j$all_pres_olw == FALSE)) {

            df$plaus_forest_floor_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_8,
                                           implausible_reasons_j)

          }
        }


        ### rule_9 ----
        # "Organic layer weight should be below 64 kg m-2 (95 % quantile)."

        olw_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type == "forest_floor") %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
          reframe(organic_layer_weight = ifelse(
            any(!is.na(organic_layer_weight)),
            sum(organic_layer_weight, na.rm = TRUE),
            NA_real_)) %>%
          filter(!is.na(organic_layer_weight)) %>%
          pull(organic_layer_weight)

        if (!identical(olw_j, numeric(0))) {

          if (any(olw_j > plaus_range_olw)) {

            df$plaus_forest_floor_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_9,
                                           implausible_reasons_j)
          }
        }


        ### rule_10 ----
        # "Total organic carbon of forest floor should be
        #  between 42 and 567 g kg-1 (99 % quantile)."

        toc_ff_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type == "forest_floor") %>%
          filter(!is.na(parameter_for_stock)) %>%
          pull(parameter_for_stock)

        if (any(toc_ff_j < plaus_range_toc_ff[1]) ||
            any(toc_ff_j > plaus_range_toc_ff[2])) {

          df$plaus_forest_floor_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_10,
                                         implausible_reasons_j)
        }



        if (!identical(implausible_reasons_j, NULL)) {

          if (!is.na(df$implaus_fscc_reason[j])) {

            df$implaus_fscc_reason[j] <-
              paste(df$implaus_fscc_reason[j],
                    implausible_reasons_j)
          } else {

            df$implaus_fscc_reason[j] <- implausible_reasons_j

          }
        }

      } # End of j in vec_i




      ## Change between surveys for individual plot ----

      for (vec_selected_i in c("vec_som_i", "vec_pfh_i")) {

        vec_sel_i <- get(vec_selected_i)

        if (identical(vec_sel_i, integer(0))) {
          next
        }

        if (length(vec_sel_i) <= 1) {
          next
        }

        vec_sel_i <- vec_sel_i[order(df$survey_year[vec_sel_i])]

        for (j in vec_sel_i) {

          implausible_reasons_j <- NULL

          # Evaluate based on the earliest survey,
          # assuming that this one is overall less reliable

          if (which(j == vec_sel_i) == length(vec_sel_i)) {
            next
          }

          # Row index of next survey year to compare with

          j_next <- vec_sel_i[which(j == vec_sel_i) + 1]



          ### rule_11 ----
          # "Change rate of mean forest floor total organic carbon
          # should be between -25 and 25 g kg-1 year-1 (~95 % quantile)."

          toc_ff_rate_j <- df_layer %>%
            filter(plot_id == plot_ids[i]) %>%
            filter(survey_form == df$survey_form[j]) %>%
            filter(survey_year %in% df$survey_year[c(j, j_next)]) %>%
            filter(layer_type == "forest_floor") %>%
            group_by(plot_id, profile_id_form,
                     survey_form, survey_year) %>%
            mutate(
              organic_layer_weight = ifelse(
                any(is.na(organic_layer_weight)),
                1,
                organic_layer_weight)) %>%
            reframe(toc_avg = ifelse(
              any(!is.na(
                parameter_for_stock[which(layer_type == "forest_floor")])),
              weighted.mean(
                parameter_for_stock[which(layer_type == "forest_floor")],
                w = organic_layer_weight,
                na.rm = TRUE),
              NA_real_)) %>%
            group_by(plot_id, survey_form, survey_year) %>%
            reframe(toc_avg = mean(toc_avg, na.rm = TRUE)) %>%
            filter(!is.na(toc_avg))

          if (n_distinct(toc_ff_rate_j$survey_year) == 2) {

            toc_ff_rate_j <- toc_ff_rate_j %>%
              group_by(plot_id, survey_form) %>%
              do(broom::tidy(lm(toc_avg ~ survey_year, data = .))) %>%
              filter(term == "survey_year") %>%
              pull(estimate)

            if (toc_ff_rate_j < plaus_range_toc_ff_change[1] ||
                toc_ff_rate_j > plaus_range_toc_ff_change[2]) {

              df$plaus_forest_floor_fscc[j] <- FALSE
              implausible_reasons_j <- paste(rule_11,
                                             implausible_reasons_j)
            }
          }




          if (!identical(implausible_reasons_j, NULL)) {

            if (!is.na(df$implaus_fscc_reason[j])) {

              df$implaus_fscc_reason[j] <-
                paste(df$implaus_fscc_reason[j],
                      implausible_reasons_j)
            } else {

              df$implaus_fscc_reason[j] <- implausible_reasons_j

            }
          }

        } # End of j (with j_next) in vec_i

      } # End of som versus pfh




      # Update progress bar
      setTxtProgressBar(progress_bar, i)

    } # End of evaluation plot_ids


    close(progress_bar)




  } else # End of "if organic_carbon_total"


  # . ----
  # If not TOC ----

  {


    # Retrieve data per profile

    profile_stocks <- get_env(paste0(survey_form_orig, "_profile_",
                                     shorter_var_name,
                                     "_stocks"))

    # Define plausible limits ----

    if (exists(paste0("plot_", shorter_var_name, "_stocks_summ"))) {

      plaus_range_stock <-
        get_env(paste0("plot_", shorter_var_name, "_stocks_summ")) %>%
        pull(stock) %>%
        quantile(c(0.005, 0.995), na.rm = TRUE) %>%
        round(1)

      plaus_range_stock_change <-
        get_env(paste0("plot_", shorter_var_name, "_stocks_summ")) %>%
        pull(stock_change) %>%
        quantile(c(0.05, 0.95), na.rm = TRUE) %>%
        round(1)

      plaus_range_stock_change_rel <-
        get_env(paste0("plot_", shorter_var_name, "_stocks_summ")) %>%
        pull(stock_change_rel) %>%
        quantile(c(0.005, 0.995), na.rm = TRUE) %>%
        round(1) # approximately +-10 % year-1
    }

    if (parameter == "n_total") {

      plaus_range_stock <- c(1, 30) # To correct
      plaus_range_stock_change <- c(-0.3, 0.3) # To correct

    } else if (parameter == "extrac_p") {

      plaus_range_stock <- c(143, 8255) # To correct
      plaus_range_stock_change <- c(-110, 110) # To correct

    } else {

      warning("Plausible limits for this parameter unknown!")
    }

    plaus_range_stock_change_rel <- c(-10, 10)




    if (exists(paste0(so_pre, "layers")) &&
        exists(paste0(s1_pre, "layers"))) {

      plaus_range_olw <- bind_rows(get_env(paste0(so_pre, "layers")) # ,
                                   # get_env(paste0(s1_pre, "layers"))
                                   ) %>%
        group_by(plot_id, survey_form, survey_year) %>%
        reframe(organic_layer_weight =
                  sum(organic_layer_weight, na.rm = TRUE)) %>%
        arrange(organic_layer_weight) %>%
        pull(organic_layer_weight) %>%
        quantile(c(0.975), na.rm = TRUE) %>%
        round(1)

      plaus_range_par_ff <- bind_rows(get_env(paste0(so_pre, "layers")) # ,
                                      # get_env(paste0(s1_pre, "layers"))
                                      ) %>%
        filter(layer_type == "forest_floor") %>%
        pull(parameter_for_stock) %>%
        quantile(c(0.005, 0.995), na.rm = TRUE) %>%
        round(1)

      plaus_range_par_bg <- bind_rows(get_env(paste0(so_pre, "layers")) # ,
                                      # get_env(paste0(s1_pre, "layers"))
                                      ) %>%
        group_by(plot_id, survey_form, survey_year) %>%
        reframe(toc_max = ifelse(
          any(!is.na(parameter_for_stock[which(layer_type != "forest_floor")])),
          max(parameter_for_stock[which(layer_type != "forest_floor")],
              na.rm = TRUE),
          NA_real_)) %>%
        pull(toc_max) %>%
        quantile(c(0.005), na.rm = TRUE) %>%
        round(1)

      plaus_range_par_density <- bind_rows(get_env(paste0(so_pre, "layers")) # ,
                                         # get_env(paste0(s1_pre, "layers"))
                                         ) %>%
        pull(density) %>%
        quantile(c(0.975), na.rm = TRUE) %>%
        round(1)

      plaus_range_par_ff_change <- bind_rows(get_env(paste0(so_pre, "layers"))
                                             # ,
                                             # get_env(paste0(s1_pre, "layers"))
                                             ) %>%
        group_by(plot_id, survey_form, survey_year) %>%
        reframe(par_avg = ifelse(
          any(!is.na(parameter_for_stock[which(layer_type == "forest_floor")])),
          mean(parameter_for_stock[which(layer_type == "forest_floor")],
               na.rm = TRUE),
          NA_real_)) %>%
        filter(!is.na(par_avg)) %>%
        # Calculate slope of linear regression
        # (the TOc should not change too drastically)
        group_by(plot_id, survey_form) %>%
        filter(n() >= 2) %>%  # Filter groups with at least two records
        do(broom::tidy(lm(par_avg ~ survey_year, data = .))) %>%
        filter(term == "survey_year") %>%
        select(plot_id, survey_form, estimate) %>%
        rename(slope_par_ff = estimate) %>%
        pull(slope_par_ff) %>%
        quantile(c(0.025, 0.975), na.rm = TRUE) %>%
        round(1)

    }

    plaus_range_olw <- 64

    if (parameter == "n_total") {

      plaus_range_par_ff <- c(3, 24) # To correct
      plaus_range_par_bg <- 0.2 # To correct
      plaus_range_par_density <- 0.5 # To correct
      plaus_range_par_ff_change <- c(-1.5, 1.5) # To correct

    } else if (parameter == "extrac_p") {

      plaus_range_par_ff <- c(132, 1552) # To correct
      plaus_range_par_bg <- 18 # To correct
      plaus_range_par_density <- 91 # To correct
      plaus_range_par_ff_change <- c(-35, 35) # To correct

    } else {

      warning("Plausible limits for this parameter unknown!")

    }



    # Define rules ----

    ## Total/below-ground stock ----

    # If these rules are not met, the total stock or at least the below-ground
    # stock is considered implausible, which implies that FSCC recommends not
    # to use the stock data from the given plot survey.

    # First set of rules: plausibility of a values
    # within a certain plot x survey_year.
    # Stock flagged as implausible if not met.

    rule_1 <- paste0("Stock should be between ", plaus_range_stock[1],
                     "and ", plaus_range_stock[2],
                     " t C ha-1 (99 % quantile).")

    rule_2 <- paste0("Max. below-ground ", parameter, " should be ",
                     "above ", plaus_range_par_bg,
                     " g kg-1 (99 % quantile).")

    rule_3 <- paste0(str_to_title(shorter_var_name),
                     " density should be below ", plaus_range_par_density,
                     " t C ha-1 cm-1 (~95 % quantile).")

    # Second set of rules: plausibility of changes between survey years
    # from a certain plot.
    # Stock of earliest survey year flagged as implausible if not met.

    rule_4 <- paste0("Annual sequestration rate should be ",
                     "between ", plaus_range_stock_change[1], " and ",
                     plaus_range_stock_change[2],
                     " t C ha-1 year-1 (~90 % quantile).")

    rule_5 <- paste0("Rel. annual sequestration rate should be ",
                     "between ", plaus_range_stock_change_rel[1],
                     " and ", plaus_range_stock_change_rel[2],
                     " % year-1 (99 % quantile).")




    ## Subsoil stock ----

    rule_6 <- paste0("Observation depth should be >= 30 cm or ",
                     ">= 70 % of eff_soil_depth.")




    ## Forest floor stock ----

    # If these rules are not met, the forest floor stock is considered
    # implausible, which implies that below-ground stocks (stock_below_ground
    # and stock_below_ground_topsoil) can still be used if plaus_total_fscc is
    # TRUE.
    # FSCC only recommends not to use the stock data from the forest floor
    # (stock_forest_floor) and total stock (stock and stock_topsoil), as these
    # include forest floor stocks.

    # First set of rules: plausibility of a values
    # within a certain plot x survey_year.
    # Stock flagged as implausible if not met.

    rule_7 <- paste0("Plots should have non-OL forest floor layers, ",
                     "unless their humus form is Mull.")

    rule_8 <- "Non-OL layers should have a known organic layer weight."

        # Note that OLW is not included in "pfh" survey forms. However, it
        # is gap-filled based on the layer thickness if possible, and without
        # this, we can still not consider the total stock as plausible;
        # hence, we still flag "pfh" plot surveys as implausible if the
        # forest floor cannot be calculated.

    rule_9 <- paste0("Organic layer weight should be below ", plaus_range_olw,
                     " kg m-2 (95 % quantile).")

    rule_10 <- paste0(str_to_title(parameter), " of forest floor should be ",
                      "between ", plaus_range_par_ff[1], " and ",
                      plaus_range_par_ff[2],
                      " g kg-1 (99 % quantile).")

    # Second set of rules: plausibility of changes between survey years
    # from a certain plot.
    # Stock of earliest survey year flagged as implausible if not met.

    rule_11 <- paste0("Change rate of mean forest floor ", parameter, " ",
                      "should be between ", plaus_range_par_ff_change[1],
                      " and ", plaus_range_par_ff_change[2], " g kg-1 year-1 ",
                      "(~95 % quantile).")





    # plausible_fscc
    # implausible_fscc_reason

    # Evaluate for different plots ----

    plot_ids <- unique(df$plot_id)

    # Set progress bar
    progress_bar <- txtProgressBar(min = 0,
                                   max = length(plot_ids), style = 3)

    for (i in seq_along(plot_ids)) {

      vec_i <- which(df$plot_id == plot_ids[i])

      vec_som_i <- which(df$plot_id == plot_ids[i] &
                           grepl("som", df$survey_form))

      vec_pfh_i <- which(df$plot_id == plot_ids[i] &
                           grepl("pfh", df$survey_form))

      df_i <- df %>%
        filter(plot_id == plot_ids[i])

      ## Total/below-ground stock ----

      # If these rules are not met, the total stock or at least the below-ground
      # stock is considered implausible, which implies that FSCC recommends not
      # to use the stock data from the given plot survey.


      ## Individual plot surveys ----

      for (j in vec_i) {

        implausible_reasons_j <- NULL

        ### rule_1 ----
        # "Stock should be between 21 and 894 t C ha-1 (99 % quantile)"

        if (df$stock[j] < plaus_range_stock[1] ||
            df$stock[j] > plaus_range_stock[2]) {

          df$plaus_total_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_1,
                                         implausible_reasons_j)
        }


        ### rule_2 ----
        # "Max. below-ground total organic carbon should be
        #  above 3 g kg-1 (99 % quantile)."

        par_bg_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type != "forest_floor") %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
          reframe(toc_max = ifelse(
            any(!is.na(parameter_for_stock[which(layer_type != "forest_floor")])),
            max(parameter_for_stock[which(layer_type != "forest_floor")],
                na.rm = TRUE),
            NA_real_)) %>%
          filter(!is.na(toc_max)) %>%
          pull(toc_max)

        if (any(par_bg_j < plaus_range_par_bg)) {

          df$plaus_total_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_2,
                                         implausible_reasons_j)
        }



        ### rule_3 ----
        # "Carbon density should be below 8 t C ha-1 cm-1 (~95 % quantile)."

        par_dens_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(!is.na(density)) %>%
          pull(density)

        if (any(par_dens_j > plaus_range_par_density)) {

          df$plaus_total_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_3,
                                         implausible_reasons_j)
        }



        if (!identical(implausible_reasons_j, NULL)) {

          df$implaus_fscc_reason[j] <- implausible_reasons_j

        }

      } # End of j in vec_i



      ## Change between surveys for individual plot ----

      for (vec_selected_i in c("vec_som_i", "vec_pfh_i")) {

        vec_sel_i <- get(vec_selected_i)

        if (identical(vec_sel_i, integer(0))) {
          next
        }

        if (length(vec_sel_i) <= 1) {
          next
        }

        vec_sel_i <- vec_sel_i[order(df$survey_year[vec_sel_i])]

        for (j in vec_sel_i) {

          implausible_reasons_j <- NULL

          # Evaluate based on the earliest survey,
          # assuming that this one is overall less reliable

          if (which(j == vec_sel_i) == length(vec_sel_i)) {
            next
          }

          # Row index of next survey year to compare with

          j_next <- vec_sel_i[which(j == vec_sel_i) + 1]


          ### rule_4 ----
          # "Annual sequestration rate should be
          #  between -5 and 5 t C ha-1 year-1 (~90 % quantile)."

          seq_rate_j <- (df$stock[j_next] - df$stock[j]) /
            (df$survey_year[j_next] - df$survey_year[j])

          if (seq_rate_j < plaus_range_stock_change[1] ||
              seq_rate_j > plaus_range_stock_change[2]) {

            df$plaus_total_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_4,
                                           implausible_reasons_j)
          }



          ### rule_5 ----
          # "Rel. annual sequestration rate should be
          # between -10 and 10 % year-1 (99 % quantile)."

          seq_rate_rel_j <- ((df$stock[j_next] - df$stock[j]) /
                               (df$survey_year[j_next] - df$survey_year[j])) /
            df$stock[j] * 100

          if (seq_rate_rel_j < plaus_range_stock_change_rel[1] ||
              seq_rate_rel_j > plaus_range_stock_change_rel[2]) {

            df$plaus_total_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_5,
                                           implausible_reasons_j)
          }



          if (!identical(implausible_reasons_j, NULL)) {

            if (!is.na(df$implaus_fscc_reason[j])) {

              df$implaus_fscc_reason[j] <-
                paste(df$implaus_fscc_reason[j],
                      implausible_reasons_j)
            } else {

              df$implaus_fscc_reason[j] <- implausible_reasons_j

            }
          }

        } # End of j (with j_next) in vec_i

      } # End of som versus pfh





      ## Subsoil stock ----

      for (j in vec_i) {

        implausible_reasons_j <- NULL

        ### rule_6 ----
        # "Observation depth should be >= 30 cm or >= 70 % of eff_soil_depth."

        obs_depth_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type != "forest_floor")

        if ("gapfilled_post_layer1" %in% names(obs_depth_j)) {

          obs_depth_j <- obs_depth_j %>%
            filter(is.na(gapfilled_post_layer1) |
                     gapfilled_post_layer1 == "")
        }

        use_stock_topsoil_j <- obs_depth_j %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
          reframe(obs_depth = max(depth_bottom, na.rm = TRUE),
                  depth_stock = unique(depth_stock)) %>%
          rowwise() %>%
          mutate(
            use_stock_topsoil =
              (.data$obs_depth < 30 &
                 .data$obs_depth < 0.7 * .data$depth_stock)) %>%
          ungroup() %>%
          pull(use_stock_topsoil)


        if (all(use_stock_topsoil_j == TRUE)) {

          df$use_stock_topsoil[j] <- TRUE
          implausible_reasons_j <- paste(rule_6,
                                         implausible_reasons_j)
        }



        if (!identical(implausible_reasons_j, NULL)) {

          if (!is.na(df$implaus_fscc_reason[j])) {

            df$implaus_fscc_reason[j] <-
              paste(df$implaus_fscc_reason[j],
                    implausible_reasons_j)
          } else {

            df$implaus_fscc_reason[j] <- implausible_reasons_j

          }
        }

      }






      ## Forest floor stock ----

      # If these rules are not met, the forest floor stock is considered
      # implausible, which implies that below-ground stocks (stock_below_ground
      # and stock_below_ground_topsoil) can still be used if plaus_total_fscc is
      # TRUE.
      # FSCC only recommends not to use the stock data from the forest floor
      # (stock_forest_floor) and total stock (stock and stock_topsoil), as these
      # include forest floor stocks.

      ## Individual plot surveys ----

      for (j in vec_i) {

        implausible_reasons_j <- NULL


        ### rule_7 ----
        # "Plots should have non-OL forest floor layers
        #  unless their humus form is Mull."

        ff_layers_j <- df_layer %>%
          # Layers containing "L" are already filtered out
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type == "forest_floor")

        # No need to further group it per profile and then per plot, because
        # any non-OL forest floor layers in whichever profile are already
        # sufficient

        if (nrow(ff_layers_j) == 0 &&
            !grepl("mull", df$humus_form[j], ignore.case = TRUE)) {

          df$plaus_forest_floor_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_7,
                                         implausible_reasons_j)

        }



        ### rule_8 ----
        # "Non-OL layers should have a known organic layer weight."

        ff_olw_pres_j <- df_layer %>%
          # Layers containing "L" are already filtered out
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type == "forest_floor") %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
          reframe(
            all_pres_olw = all(!is.na(organic_layer_weight))) %>%
          ungroup()

        # As soon as one of the profiles has a complete set of OLW data,
        # we can estimate a forest floor carbon stock

        if (nrow(ff_olw_pres_j) > 0) {

          if (all(ff_olw_pres_j$all_pres_olw == FALSE)) {

            df$plaus_forest_floor_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_8,
                                           implausible_reasons_j)

          }
        }


        ### rule_9 ----
        # "Organic layer weight should be below 64 kg m-2 (95 % quantile)."

        olw_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type == "forest_floor") %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
          reframe(organic_layer_weight = ifelse(
            any(!is.na(organic_layer_weight)),
            sum(organic_layer_weight, na.rm = TRUE),
            NA_real_)) %>%
          filter(!is.na(organic_layer_weight)) %>%
          pull(organic_layer_weight)

        if (!identical(olw_j, numeric(0))) {

          if (any(olw_j > plaus_range_olw)) {

            df$plaus_forest_floor_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_9,
                                           implausible_reasons_j)
          }
        }


        ### rule_10 ----
        # "Total organic carbon of forest floor should be
        #  between 42 and 567 g kg-1 (99 % quantile)."

        par_ff_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year == df$survey_year[j]) %>%
          filter(layer_type == "forest_floor") %>%
          filter(!is.na(parameter_for_stock)) %>%
          pull(parameter_for_stock)

        if (any(par_ff_j < plaus_range_par_ff[1]) ||
            any(par_ff_j > plaus_range_par_ff[2])) {

          df$plaus_forest_floor_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_10,
                                         implausible_reasons_j)
        }



        if (!identical(implausible_reasons_j, NULL)) {

          if (!is.na(df$implaus_fscc_reason[j])) {

            df$implaus_fscc_reason[j] <-
              paste(df$implaus_fscc_reason[j],
                    implausible_reasons_j)
          } else {

            df$implaus_fscc_reason[j] <- implausible_reasons_j

          }
        }

      } # End of j in vec_i




      ## Change between surveys for individual plot ----

      for (vec_selected_i in c("vec_som_i", "vec_pfh_i")) {

        vec_sel_i <- get(vec_selected_i)

        if (identical(vec_sel_i, integer(0))) {
          next
        }

        if (length(vec_sel_i) <= 1) {
          next
        }

        vec_sel_i <- vec_sel_i[order(df$survey_year[vec_sel_i])]

        for (j in vec_sel_i) {

          implausible_reasons_j <- NULL

          # Evaluate based on the earliest survey,
          # assuming that this one is overall less reliable

          if (which(j == vec_sel_i) == length(vec_sel_i)) {
            next
          }

          # Row index of next survey year to compare with

          j_next <- vec_sel_i[which(j == vec_sel_i) + 1]



          ### rule_11 ----
          # "Change rate of mean forest floor total organic carbon
          # should be between -25 and 25 g kg-1 year-1 (~95 % quantile)."

          par_ff_rate_j <- df_layer %>%
            filter(plot_id == plot_ids[i]) %>%
            filter(survey_form == df$survey_form[j]) %>%
            filter(survey_year %in% df$survey_year[c(j, j_next)]) %>%
            filter(layer_type == "forest_floor") %>%
            group_by(plot_id, profile_id_form,
                     survey_form, survey_year) %>%
            mutate(
              organic_layer_weight = ifelse(
                any(is.na(organic_layer_weight)),
                1,
                organic_layer_weight)) %>%
            reframe(par_avg = ifelse(
              any(!is.na(
                parameter_for_stock[which(layer_type == "forest_floor")])),
              weighted.mean(
                parameter_for_stock[which(layer_type == "forest_floor")],
                w = organic_layer_weight,
                na.rm = TRUE),
              NA_real_)) %>%
            group_by(plot_id, survey_form, survey_year) %>%
            reframe(par_avg = mean(par_avg, na.rm = TRUE)) %>%
            filter(!is.na(par_avg))

          if (n_distinct(par_ff_rate_j$survey_year) == 2) {

            par_ff_rate_j <- par_ff_rate_j %>%
              group_by(plot_id, survey_form) %>%
              do(broom::tidy(lm(par_avg ~ survey_year, data = .))) %>%
              filter(term == "survey_year") %>%
              pull(estimate)

            if (par_ff_rate_j < plaus_range_par_ff_change[1] ||
                par_ff_rate_j > plaus_range_par_ff_change[2]) {

              df$plaus_forest_floor_fscc[j] <- FALSE
              implausible_reasons_j <- paste(rule_11,
                                             implausible_reasons_j)
            }
          }




          if (!identical(implausible_reasons_j, NULL)) {

            if (!is.na(df$implaus_fscc_reason[j])) {

              df$implaus_fscc_reason[j] <-
                paste(df$implaus_fscc_reason[j],
                      implausible_reasons_j)
            } else {

              df$implaus_fscc_reason[j] <- implausible_reasons_j

            }
          }

        } # End of j (with j_next) in vec_i

      } # End of som versus pfh




      # Update progress bar
      setTxtProgressBar(progress_bar, i)

    } # End of evaluation plot_ids


    close(progress_bar)




  } # End of "if not TOC"









  # Summarise different filters into clear filters per stock type ----

  # Current filters:

  # - plaus_total_fscc
  # - plaus_forest_floor_fscc
  # - use_stock_topsoil

  # - implaus_fscc_reason


  # New filters:

  # - stock_plaus
  # - stock_topsoil_plaus
  # - stock_below_ground_plaus
  # - stock_below_ground_topsoil_plaus
  # - stock_forest_floor_plaus

  # - implaus_fscc_reason


  df <- df %>%
    relocate(use_stock_topsoil, .before = plaus_total_fscc) %>%
    mutate(
      # stock_plaus
      stock_plaus = case_when(
        (plaus_total_fscc == TRUE &
           plaus_forest_floor_fscc == TRUE &
           use_stock_topsoil == FALSE) ~ TRUE,
        .default = FALSE),
      # stock_topsoil_plaus
      stock_topsoil_plaus = case_when(
        (plaus_total_fscc == TRUE &
           plaus_forest_floor_fscc == TRUE) ~ TRUE,
        .default = FALSE),
      # stock_below_ground_plaus
      stock_below_ground_plaus = case_when(
        (plaus_total_fscc == TRUE &
           use_stock_topsoil == FALSE) ~ TRUE,
        .default = FALSE),
      # stock_below_ground_topsoil_plaus
      stock_below_ground_topsoil_plaus = case_when(
        (plaus_total_fscc == TRUE) ~ TRUE,
        .default = FALSE),
      # stock_forest_floor_plaus
      stock_forest_floor_plaus = case_when(
        (plaus_total_fscc == TRUE &
           plaus_forest_floor_fscc == TRUE) ~ TRUE,
        .default = FALSE)) %>%
    relocate(stock_plaus, .after = stock_max) %>%
    relocate(stock_topsoil_plaus, .after = stock_topsoil_max) %>%
    relocate(stock_below_ground_plaus, .after = stock_below_ground_max) %>%
    relocate(stock_below_ground_topsoil_plaus,
             .after = stock_below_ground_topsoil_max) %>%
    relocate(stock_forest_floor_plaus, .after = stock_forest_floor_max) %>%
    select(-plaus_forest_floor_fscc,
           -plaus_total_fscc,
           -use_stock_topsoil)





return(df)


}
