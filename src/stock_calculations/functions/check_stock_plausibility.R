
check_stock_plausibility <- function(data_frame) {

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

  cat(paste0(" \nCheck plausibility of stocks\n"))

  source("./src/functions/get_env.R")

  df <- data_frame %>%
    mutate(
      plausible_fscc = TRUE,
      implausible_fscc_reason = NA)

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

  df_layer <- get_env(paste0(code_survey, "_layers"))

  # Retrieve data per profile

  profile_stocks <- get_env(paste0(code_survey, "_profile_c_stocks"))

  # Define plausible limits ----

  if (exists("plot_c_stocks_summ")) {

    plaus_range_stock <- get_env("plot_c_stocks_summ") %>%
      pull(stock) %>%
      quantile(c(0.005, 0.995), na.rm = TRUE) %>%
      round(1)

    plaus_range_stock_change <- get_env("plot_c_stocks_summ") %>%
      pull(stock_change) %>%
      quantile(c(0.05, 0.95), na.rm = TRUE) %>%
      round(1) # approximately +-5 t C ha-1 year-1

    plaus_range_stock_change_rel <- get_env("plot_c_stocks_summ") %>%
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
      quantile(c(0.995), na.rm = TRUE) %>%
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

    plaus_range_c_density <- bind_rows(get_env("so_layers"),
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
      do(tidy(lm(toc_avg ~ survey_year, data = .))) %>%
      filter(term == "survey_year") %>%
      select(plot_id, survey_form, estimate) %>%
      rename(slope_toc_ff = estimate) %>%
      pull(slope_toc_ff) %>%
      quantile(c(0.025, 0.975), na.rm = TRUE) %>%
      round(1)

  }

  plaus_range_olw <- 143
  plaus_range_toc_ff <- c(42, 567)
  plaus_range_toc_bg <- 3
  plaus_range_c_density <- 8
  plaus_range_toc_ff_change <- c(-25, 25)

  # Define rules ----

  # First set of rules: plausibility of a values
  # within a certain plot x survey_year.
  # Stock flagged as implausible if not met.

  rule_1 <- "Stock should be between 21 and 894 t C ha-1 (99 % quantile)."

  rule_2 <- "Organic layer weight should be below 143 kg m-2 (99 % quantile)."
  rule_3 <- paste0("Total organic carbon of forest floor should be ",
                   "between 42 and 567 g kg-1 (99 % quantile).")
  rule_4 <- paste0("Max. below-ground total organic carbon should be ",
                   "above 3 g kg-1 (99 % quantile).")
  rule_5 <- "Carbon density should be below 8 t C ha-1 cm-1 (~95 % quantile)."
  rule_6 <- "Plots without Mull as humus form should have a forest floor."


  # Second set of rules: plausibility of changes between survey years
  # from a certain plot.
  # Stock of earliest survey year flagged as implausible if not met.

  rule_7 <- paste0("Annual sequestration rate should be ",
                   "between -5 and 5 t C ha-1 year-1 (~90 % quantile).")
  rule_8 <- paste0("Rel. annual sequestration rate should be ",
                   "between -10 and 10 % year-1 (99 % quantile).")

  rule_9 <- paste0("Change rate of mean forest floor total organic carbon ",
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

    # Individual plot surveys ----

    for (j in vec_i) {

      implausible_reasons_j <- NULL

      ## rule_1 ----
      # "Stock should be between 21 and 894 t C ha-1 (99 % quantile)"

      if (df$stock[j] < plaus_range_stock[1] ||
          df$stock[j] > plaus_range_stock[2]) {

        df$plausible_fscc[j] <- FALSE
        implausible_reasons_j <- paste(rule_1,
                                       implausible_reasons_j)
      }

      ## rule_2 ----
      # "Organic layer weight should be below 143 kg m-2 (99 % quantile)."

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

      if (any(olw_j > plaus_range_olw)) {

        df$plausible_fscc[j] <- FALSE
        implausible_reasons_j <- paste(rule_2,
                                       implausible_reasons_j)
      }


      ## rule_3 ----
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

        df$plausible_fscc[j] <- FALSE
        implausible_reasons_j <- paste(rule_3,
                                       implausible_reasons_j)
      }


      ## rule_4 ----
      # "Max. below-ground total organic carbon should be
      # above 3 g kg-1 (99 % quantile)."

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

        df$plausible_fscc[j] <- FALSE
        implausible_reasons_j <- paste(rule_4,
                                       implausible_reasons_j)
      }


      ## rule_5 ----
      # "Carbon density should be below 8 t C ha-1 cm-1 (~95 % quantile)."

      plaus_range_c_density

      c_dens_j <- df_layer %>%
        filter(plot_id == plot_ids[i]) %>%
        filter(survey_form == df$survey_form[j]) %>%
        filter(survey_year == df$survey_year[j]) %>%
        filter(!is.na(density)) %>%
        pull(density)

      if (any(c_dens_j > plaus_range_c_density)) {

        df$plausible_fscc[j] <- FALSE
        implausible_reasons_j <- paste(rule_5,
                                       implausible_reasons_j)
      }


      ## rule_6 ----
      # "Plots without Mull as humus form should have any non-OL forest floor."

      if ((is.na(df$unknown_forest_floor[j]) ||
          df$unknown_forest_floor[j] == FALSE) &&
          (is.na(df$stock_forest_floor[j]) ||
           df$stock_forest_floor[j] == 0) &&
          grepl("mull", df$humus_form[j], ignore.case = TRUE)) {

        df$plausible_fscc[j] <- FALSE
        implausible_reasons_j <- paste(rule_6,
                                       implausible_reasons_j)

      }

      if (!identical(implausible_reasons_j, NULL)) {

        df$implausible_fscc_reason[j] <- implausible_reasons_j

      }

    } # End of j in vec_i



    # Change between surveys for individual plot ----

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


        ## rule_7 ----
        # "Annual sequestration rate should be
        # between -5 and 5 t C ha-1 year-1 (~90 % quantile)."

        seq_rate_j <- (df$stock[j_next] - df$stock[j]) /
          (df$survey_year[j_next] - df$survey_year[j])

        if (seq_rate_j < plaus_range_stock_change[1] ||
            seq_rate_j > plaus_range_stock_change[2]) {

          df$plausible_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_7,
                                         implausible_reasons_j)
        }


        ## rule_8 ----
        # "Rel. annual sequestration rate should be
        # between -10 and 10 % year-1 (99 % quantile)."

        seq_rate_rel_j <- ((df$stock[j_next] - df$stock[j]) /
          (df$survey_year[j_next] - df$survey_year[j])) / df$stock[j] * 100

        if (seq_rate_rel_j < plaus_range_stock_change_rel[1] ||
            seq_rate_rel_j > plaus_range_stock_change_rel[2]) {

          df$plausible_fscc[j] <- FALSE
          implausible_reasons_j <- paste(rule_8,
                                         implausible_reasons_j)
        }


        ## rule_9 ----
        # "Change rate of mean forest floor total organic carbon
        # should be between -25 and 25 g kg-1 year-1 (~95 % quantile)."

        toc_ff_rate_j <- df_layer %>%
          filter(plot_id == plot_ids[i]) %>%
          filter(survey_form == df$survey_form[j]) %>%
          filter(survey_year %in% df$survey_year[c(j, j_next)]) %>%
          filter(layer_type == "forest_floor") %>%
          group_by(plot_id, profile_id_form,
                   survey_form, survey_year) %>%
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
            do(tidy(lm(toc_avg ~ survey_year, data = .))) %>%
            filter(term == "survey_year") %>%
            pull(estimate)

          if (toc_ff_rate_j < plaus_range_toc_ff_change[1] ||
              toc_ff_rate_j > plaus_range_toc_ff_change[2]) {

            df$plausible_fscc[j] <- FALSE
            implausible_reasons_j <- paste(rule_9,
                                           implausible_reasons_j)
          }
        }


        if (!identical(implausible_reasons_j, NULL)) {

          if (!is.na(df$implausible_fscc_reason[j])) {

            df$implausible_fscc_reason[j] <-
              paste(df$implausible_fscc_reason[j],
                    implausible_reasons_j)
          } else {

            df$implausible_fscc_reason[j] <- implausible_reasons_j

          }
        }

      } # End of j (with j_next) in vec_i

    } # End of som versus pfh

    # Update progress bar
    setTxtProgressBar(progress_bar, i)

  } # End of evaluation plot_ids


  close(progress_bar)



return(df)


}
