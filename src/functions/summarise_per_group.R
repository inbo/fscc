


summarise_per_group <- function(df,
                                variables_to_summarise,
                                mode = "mean",
                                digits = NULL) {

  # The dataframe should be grouped already
  assertthat::assert_that(is_grouped_df(df))

  # Retrieve the grouping variables
  grouping_vars <- group_vars(df)

  # Create the "key" column by pasting the values of grouping variables
  # together
  df <- df %>%
    ungroup() %>%
    rowwise() %>%
    mutate(key = paste(c_across(all_of(grouping_vars)),
                       collapse = "_")) %>%
    group_by(across(all_of(grouping_vars)), key)

  assertthat::assert_that(is.character(variables_to_summarise))


  for (var in variables_to_summarise) {

    assertthat::assert_that(
      var %in% names(df))

    var_min <- paste0(var, "_min")
    var_max <- paste0(var, "_max")
    var_stdev <- paste0(var, "_stdev")

    # Option 1: within-repetition variation is indicated in the dataframe ----
    # (using columns with names ending with "_min" and "_max")

    if (var_min %in% names(df) &
        var_max %in% names(df)) {

      ## Mode: mean ----

      if (mode == "mean") {

        df_summ_var <- df %>%
          reframe(
            # !!var_stdev := {
            #   var_values <- .data[[var]]
            #   if (sum(!is.na(var_values)) > 1) {
            #     sd(var_values, na.rm = TRUE)
            #   } else {
            #     NA_real_
            #   }
            # },
            !!var_min := ifelse(
              any(!is.na(.data[[var_min]])),
              min(.data[[var_min]], na.rm = TRUE),
              NA_real_
            ),
            !!var_max := ifelse(
              any(!is.na(.data[[var_max]])),
              max(.data[[var_max]], na.rm = TRUE),
              NA_real_
            ),
            !!var := ifelse(
              any(!is.na(.data[[var]])),
              mean(.data[[var]], na.rm = TRUE),
              NA_real_
            )) %>%
          # We are not proceeding with this standard deviation, which only
          # reflects the between-replicate variation (of the estimates),
          # not the within-replicate variation.
          # The uncertainty ranges provided for each estimate already
          # encapsulate the variation. They already gives us an understanding
          # of the range within which the true parameter value is likely
          # to lie.
          # select(-{{var_stdev}}) %>%
          relocate({{var}}, .before = {{var_min}})

      }


      ## Mode: sum ----

      if (mode == "sum") {

        df_summ_var <- df %>%
          reframe(
            !!var_min := ifelse(
              any(!is.na(.data[[var_min]])),
              sum(.data[[var_min]], na.rm = TRUE),
              NA_real_
            ),
            !!var_max := ifelse(
              any(!is.na(.data[[var_max]])),
              sum(.data[[var_max]], na.rm = TRUE),
              NA_real_
            ),
            !!var := ifelse(
              any(!is.na(.data[[var]])),
              sum(.data[[var]], na.rm = TRUE),
              NA_real_
            )) %>%
          relocate({{var}}, .before = {{var_min}})

      }


    } else {

      # Option 2: within-repetition variation is not indicated ----
      # in the dataframe (i.e. there are no columns with names ending with
      # "_min" and "_max")

      ## Mode: mean ----

      if (mode == "mean") {

        df_summ_var <- df %>%
          reframe(
            !!var_stdev := {
              var_values <- .data[[var]]
              if (sum(!is.na(var_values)) > 1) {
                sd(var_values, na.rm = TRUE)
              } else {
                NA_real_
              }
            },
            !!var := ifelse(
              any(!is.na(.data[[var]])),
              mean(.data[[var]], na.rm = TRUE),
              NA_real_
            )) %>%
          mutate(
            !!var_min := ifelse(
              !is.na(.data[[var_stdev]]) & !is.na(.data[[var]]),
              .data[[var]] - .data[[var_stdev]],
              ifelse(
                is.na(.data[[var_stdev]]) & !is.na(.data[[var]]),
                .data[[var]],
                NA_real_)),
            !!var_max := ifelse(
              !is.na(.data[[var_stdev]]) & !is.na(.data[[var]]),
              .data[[var]] + .data[[var_stdev]],
              ifelse(
                is.na(.data[[var_stdev]]) & !is.na(.data[[var]]),
                .data[[var]],
                NA_real_))) %>%
          # We can proceed with minimum and maximum values of the uncertainty
          # ranges, to be consistent across parameters
          select(-{{var_stdev}}) %>%
          relocate({{var}}, .before = {{var_min}})

      }


      ## Mode: sum ----

      if (mode == "sum") {

        df_summ_var <- df %>%
          reframe(
            !!var := ifelse(
              any(!is.na(.data[[var]])),
              sum(.data[[var]], na.rm = TRUE),
              NA_real_
            ))

      }

    }

    if (!is.null(digits)) {

      df_summ_var <- df_summ_var %>%
        mutate(
          !!var := round(.data[[var]], digits = digits))

      if (var_min %in% names(df_summ_var) &&
          var_max %in% names(df_summ_var)) {

        df_summ_var <- df_summ_var %>%
          mutate(
            !!var_min := round(.data[[var_min]], digits = digits),
            !!var_max := round(.data[[var_max]], digits = digits))

      }
    }


    if (which(var == variables_to_summarise) == 1) {

      # For the first variable
      df_summ <- df_summ_var

    } else {

      # For the next variables
      df_summ <- df_summ %>%
        left_join(df_summ_var %>%
                    select(-all_of(grouping_vars)),
                  by = "key")
    }

  } # End of "for loop" over variables

  df_summ <- df_summ %>%
    ungroup() %>%
    select(-key)

  return(df_summ)
}

