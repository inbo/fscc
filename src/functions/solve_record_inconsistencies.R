
#' Solve inconsistencies with duplicate or redundant records
#'
#' This function solves some inconsistencies with records in the "so_som"
#' survey form which were discovered by checking profiles that contain
#' any code_layer "H". It usually concerns data from old surveys.
#'
#' @param survey_form Character string - Name of the survey form (lower case and
#' separated by '_') to be evaluated
#' @param data_frame Dataframe with the data to be evaluated. Default is NULL.
#' @param solve Logical - Indicates whether records need to be solved (TRUE) or
#' indicated using an extra column 'to_remove' (FALSE)
#' @param save_to_env Logical which indicates whether the output dataframe
#' can be saved to the global environment and override any existing objects
#' with the same name (TRUE) or should be returned using "return" (FALSE).
#' Default is FALSE.
#'
#' @details
#' Currently only relevant for "so_som"


solve_record_inconsistencies <- function(survey_form,
                                         data_frame = NULL,
                                         solve = FALSE,
                                         save_to_env = FALSE) {

source("./src/functions/get_env.R")
source("./src/functions/assign_env.R")
source("./src/functions/as_character_summary.R")

  cat(paste0(" \nSolve issues with duplicate records in '",
             survey_form, "'\n"))


# Retrieve the survey_form data

if (is.null(data_frame)) {
  df <- get_env(survey_form)
} else {
  df <- data_frame
}



# Merge duplicate records so_som ----

if (survey_form == "so_som") {

  source("./src/functions/merge_duplicate_records.R")
  df <- merge_duplicate_records(survey_form = "so_som",
                                data_frame = df,
                                merge = TRUE,
                                save_to_env = FALSE)

}



# Add a column to indicate records to be removed

  df$to_remove <- NA


# Get columns with numeric content ----
# but from which the column name does not end with "_loq" or "_rt"

  numeric_columns <- sapply(names(df), function(col_name) {
    is.numeric(df[[col_name]]) && !grepl("_loq$|_rt$", col_name)
  })

  numeric_column_names <- names(df)[numeric_columns]
  numeric_column_names <-
    numeric_column_names[which(!numeric_column_names %in% c(
      "survey_year", "code_country", "partner_code", "code_plot",
      "repetition", "layer_number", "subsamples", "q_flag", "line_nr",
      "code_plot_orig", "layer_number_bg", "layer_number_ff",
      "code_soil_horizon_sample_c", "layer_limit_superior",
      "layer_limit_inferior", "layer_limit_superior_orig",
      "layer_limit_inferior_orig"))]



# If so_som ----

if (survey_form == "so_som") {

## DE_BW (1995 and 1996) ----
# Some profiles are repeated (double) in layer 0: one for 1995 and one for 1996
# Action: remove those of 1996

profiles_to_remove <-
  df %>%
  ungroup() %>%
  filter(.data$partner_short == "DE_BW") %>%
  filter(.data$survey_year < 1997) %>%
  mutate(unique_repetition = paste0(.data$plot_id, "_",
                                    .data$repetition)) %>%
  distinct(unique_survey_repetition, .keep_all = TRUE) %>%
  group_by(unique_repetition) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(.data$count > 1) %>%
  # Recreate unique_survey_repetition. We will take the 1996 records out.
  mutate(unique_survey_repetition =
           str_replace(unique_repetition,
                       "^(.*?)_",
                       "\\1_1996_")) %>%
  pull(unique_survey_repetition)

# If there are any such profiles

if (!identical(profiles_to_remove, character(0))) {

  if (solve == TRUE) {

  nrow <- nrow(df[which(df$unique_survey_repetition %in% profiles_to_remove),
                  ])
  df <- filter(df,
               !.data$unique_survey_repetition %in% profiles_to_remove)

  cat(paste0("The following profiles have been submitted twice by DE_BW ",
             "(both in 1995 and 1996):\n",
             as_character_summary(profiles_to_remove), "\n\n",
             "Action: ", nrow, " records have been removed from '",
             survey_form, "'.\n\n"))

  }
}




## DE_BB (1995) ----
# Some profiles with both an O and H layer, without layer limits.
# For all parameters, either O or H contains data,
# except in exch_acidiy: both layers contain the same value
# Action: merge them into one forest_floor O layer.

profiles_to_combine_de_bb <-
  df %>%
  ungroup %>%
  filter(.data$partner_short == "DE_BB") %>%
  filter(.data$survey_year == 1995) %>%
  group_by(unique_survey_repetition) %>%
  summarise(both_o_and_h =
              any(.data$code_layer == "O") &
              any(.data$code_layer == "H")) %>%
  filter(.data$both_o_and_h == TRUE) %>%
  pull(unique_survey_repetition)




## DE_SN (1995) ----
# An O2, H and O layer per profile, without layer limits.
# Concerning the O2 and H layer: for all parameters, one of those contains data.
# Action: merge them into one forest_floor O2 layer.

profiles_to_combine_de_sn <-
  df %>%
  ungroup %>%
  filter(.data$partner_short == "DE_SN") %>%
  filter(.data$survey_year == 1995) %>%
  group_by(unique_survey_repetition) %>%
  summarise(both_o2_and_h =
              any(.data$code_layer == "O2") &
              any(.data$code_layer == "H")) %>%
  filter(.data$both_o2_and_h == TRUE) %>%
  pull(unique_survey_repetition)


profiles_to_combine <- c(profiles_to_combine_de_bb,
                         profiles_to_combine_de_sn)

if (!identical(profiles_to_combine, character(0))) {

for (i in seq_along(profiles_to_combine)) {

  if (profiles_to_combine[i] %in% profiles_to_combine_de_bb) {

    ind_target <-
      which(df$unique_survey_repetition %in% profiles_to_combine[i] &
              df$code_layer == "O")
    ind_to_merge <-
      which(df$unique_survey_repetition %in% profiles_to_combine[i] &
              df$code_layer == "H")
  }

  if (profiles_to_combine[i] %in% profiles_to_combine_de_sn) {

    ind_target <-
      which(df$unique_survey_repetition %in% profiles_to_combine[i] &
              df$code_layer == "O2")
    ind_to_merge <-
      which(df$unique_survey_repetition %in% profiles_to_combine[i] &
              df$code_layer == "H")
  }

  assertthat::assert_that(length(ind_target) == 1 &&
                            length(ind_to_merge) == 1)

  ind_dupl <- c(ind_target, ind_to_merge)

  df$to_remove[ind_to_merge] <- TRUE

  # Get the columns where values are the same for the duplicated rows
  same_columns <- sapply(df[ind_dupl, ], function(col) length(unique(col)) == 1)

  # Extract the column names
  same_column_names <- names(df)[same_columns]

  # Columns which need to be merged
  # (i.e. for which MAN != OPT, or MAN or OPT is NA):

  df_pars_to_merge <- names(df)[!names(df) %in% same_column_names]

  # Summarise each of these parameters

  if (!identical(df_pars_to_merge, character(0))) {
    for (j in seq_along(df_pars_to_merge)) {

      par_j <- as.vector(df_pars_to_merge[j])
      col_ind_j <- as.vector(which(names(df) == par_j))
      values_j <- df[ind_dupl, col_ind_j] %>% pull


        if (any(!is.na(values_j)) && any(is.na(values_j))) {
          summary_value <- values_j[which(!is.na(values_j))]
        } else

          if (all(!is.na(values_j)) && (length(unique(values_j)) == 2)) {

            assertthat::assert_that(
              par_j %in% c("code_layer",
                           "layer_number",
                           "date_labor_analyses",
                           "origin",
                           "layer_type",
                           "code_line",
                           "line_nr",
                           "qif_key",
                           "unique_survey_layer",
                           "unique_layer_repetition",
                           "unique_layer",
                           "code_layer_orig"),
              msg = paste0("Conflicting values for unique_survey_repetition '",
                           profiles_to_combine[i], "' for parameter '",
                           par_j, "'."))

            summary_value <- df[ind_target, col_ind_j]
          }

      # Paste the information in the MAN row

      df[ind_target, col_ind_j] <- summary_value

    } # End of for loop along parameters
  }

} # End of for loop along profiles with duplicates

nrow <- length(which(df$to_remove == TRUE))

cat(paste0("The following profiles by DE_BW (1995) ",
           "contain duplicate organic layers:\n",
           as_character_summary(profiles_to_combine_de_bb), "\n\n",
           "The following profiles by DE_SN (1995) ",
           "contain duplicate organic layers:\n",
           as_character_summary(profiles_to_combine_de_sn), "\n\n",
           "Action: duplicate records were merged.\n",
           nrow, " redundant records (after merging) to be removed from '",
           survey_form, "'.\n\n"))

}


## Poland (1999) ----

# Some Polish profiles contain "H" layers without any data.
# Remove these records (redundant).

profiles_h_to_remove_poland <-
  df %>%
  ungroup %>%
  filter(.data$partner_code == 53) %>%
  filter(.data$survey_year < 2000) %>%
  group_by(unique_survey_repetition) %>%
  summarise(contains_h =  any(.data$code_layer == "H")) %>%
  filter(.data$contains_h == TRUE) %>%
  pull(unique_survey_repetition)





if (!identical(profiles_h_to_remove_poland, character(0))) {

  for (i in seq_along(profiles_h_to_remove_poland)) {

    ind_to_check <- which(df$unique_survey_repetition ==
                            profiles_h_to_remove_poland[i] &
                          df$code_layer == "H")

    assertthat::assert_that(
      all(is.na(df[ind_to_check,
                   which(names(df) %in% numeric_column_names)])),
      msg = paste0("Issue for '", profiles_h_to_remove_poland[i], "'."))

    df$to_remove[ind_to_check] <- TRUE

  }

  cat(paste0(length(profiles_h_to_remove_poland),
             " Polish profiles (1999) contain an 'H' layer ",
             "without any data.\n",
             "Action: redundant 'H' records to be removed from '",
             survey_form, "'.\n\n"))

}

## Poland (2009) ----

# Some Polish plot layers have the same upper and lower layer limit and
# are further totally empty.
# Remove those layers since redundant.

layers_to_check <- df %>%
  filter(code_country == 53) %>%
  filter(layer_limit_superior == 0) %>%
  filter(layer_limit_inferior == 0) %>%
  filter(if_all(all_of(numeric_column_names), is.na)) %>%
  pull(code_line)

if (!identical(layers_to_check, character(0))) {

  df$to_remove[which(df$code_line %in% layers_to_check)] <- TRUE
}




## Norway ----

# Some Norwegian plots have a second repetition for which only an "O" layer
# is reported.
# Assert that these contain different data,
# otherwise (if redundant) they should be removed.

surveys_to_check <- df %>%
  ungroup %>%
  filter(code_country == 55) %>%
  group_by(unique_survey_repetition, unique_survey) %>%
  summarise(contains_o =
              any(.data$code_layer == "O"),
            count_layers = n(),
            .groups = "drop") %>%
  filter(.data$contains_o == TRUE &
           .data$count_layers == 1) %>%
  pull(unique_survey)


if (!identical(surveys_to_check, character(0))) {

  for (i in seq_along(surveys_to_check)) {

      df_sub <- df %>%
        filter(unique_survey == surveys_to_check[i]) %>%
        filter(code_layer == "O")

      if (nrow(df_sub) > 1) {
        result_columns <- sapply(df_sub[, numeric_column_names],
                                 function(col) length(unique(col)) > 1)

        columns_with_different_content <- names(result_columns[result_columns])

        assertthat::assert_that(!identical(columns_with_different_content,
                                           character(0)))
      }
    }
  }




# There are differences between the layers, so keep this data






## BE_Flanders ----

# Some Flemish plots have a second repetition for which only
# forest floor layers are reported.
# Assert that these contain different data,
# otherwise (if redundant) they should be removed.

surveys_to_check <- df %>%
  ungroup %>%
  filter(partner_code == 102) %>%
  group_by(unique_survey_repetition, unique_survey) %>%
  summarise(contains_only_ff =
              !(any(.data$layer_type %in% c("peat", "mineral"))),
            .groups = "drop") %>%
  filter(contains_only_ff == TRUE) %>%
  distinct(unique_survey) %>%
  pull(unique_survey)

if (!identical(surveys_to_check, character(0))) {

for (i in seq_along(surveys_to_check)) {

  ff_layers <- df %>%
    filter(unique_survey == surveys_to_check[i]) %>%
    filter(layer_type == "forest_floor") %>%
    distinct(code_layer) %>%
    pull

  for (j in seq_along(ff_layers)) {

    df_sub <- df %>%
      filter(unique_survey == surveys_to_check[i]) %>%
      filter(code_layer == ff_layers[1])

    if (nrow(df_sub) > 1) {
    result_columns <- sapply(df_sub[, numeric_column_names],
                             function(col) length(unique(col)) > 1)

    columns_with_different_content <- names(result_columns[result_columns])

    assertthat::assert_that(!identical(columns_with_different_content,
                                       character(0)))
    }
  }
}
}








# Change layer_type of "H" to "forest_floor" in so_som

if (solve == TRUE) {
df <- df %>%
  mutate(layer_type = as.character(.data$layer_type)) %>%
  mutate(layer_type = ifelse(!is.na(.data$code_layer) &
                               .data$code_layer == "H",
                             "forest_floor",
                             .data$layer_type))

}



# Spain ----

# It may (?) be necessary to update some survey_years or
# to remove (some) records for 1993, 1994, 1995

} # End of "if so_som"





# If so_pfh ----

if (survey_form == "so_pfh") {

  ## Germany ----

  # German records to be removed, as requested by German soil expert

  ind_to_remove <-
    which(df$code_country == 4 &
            df$code_plot %in% c(611, 613) &
            df$survey_year == 2008)

  df$to_remove[ind_to_remove] <- TRUE

  cat(paste0("Action: redundant German profiles to be removed from '",
             survey_form, "'.\n\n"))


  ## Norway ----

  # Norwegian records without any horizon_master, layer_limits,
  # horizon_c_organic_total, bulk_density, texture data, are redundant

  ind_to_remove <-
    which(df$code_country == 55 &
            df$code_plot == 4 &
            df$survey_year == 1988 &
            is.na(df$horizon_master) &
            is.na(df$horizon_clay) &
            is.na(df$horizon_c_organic_total))

  df$to_remove[ind_to_remove] <- TRUE

  cat(paste0("Action: redundant Norwegian layers to be removed from '",
             survey_form, "'.\n\n"))


  ## Hungary ----
  # Hungarian plots 15, 16, 17, 18, 20 contain data for both 2009 and
  # 2010, where the 2010 data are clearly an improved (more complete)
  # version of the 2009 data. Remove the latter.

  unique_surveys_to_remove <-
    df %>%
    filter(.data$partner_code == 51) %>%
    distinct(.data$unique_survey_profile, .keep_all = TRUE) %>%
    group_by(plot_id, code_country, code_plot, profile_pit_id) %>%
    mutate(survey_year_oldest_update =
             survey_year[which(change_date ==
                                 min(as.Date(.data$change_date)))]) %>%
    ungroup() %>%
    group_by(plot_id, code_country, code_plot, profile_pit_id,
             survey_year_oldest_update) %>%
    summarise(count = n(),
              .groups = "drop") %>%
    filter(count > 1) %>%
    mutate(unique_survey = paste0(.data$code_country, "_",
                                  .data$survey_year_oldest_update, "_",
                                  .data$code_plot))

  if (nrow(unique_surveys_to_remove) > 0) {

    for (i in seq_len(nrow(unique_surveys_to_remove))) {

      df_sub <- df %>%
        filter(plot_id == unique_surveys_to_remove$plot_id[i]) %>%
        arrange(horizon_master)

      if (nrow(df_sub) > 1) {

        result_columns <- sapply(df_sub[, numeric_column_names],
                                 function(col) length(unique(na.omit(col))) >
                                   ceiling(nrow(df_sub)/2))

        columns_with_different_content <- names(result_columns[result_columns])

        assertthat::assert_that(identical(columns_with_different_content,
                                          character(0)) ||
                                  # Sometimes single values may have been
                                  # updated upon resubmission
                                  length(columns_with_different_content) < 4)

        ind_to_remove <-
          which(df$unique_survey %in%
                  unique_surveys_to_remove$unique_survey[i])

        df$to_remove[ind_to_remove] <- TRUE

      }
    }

    cat(paste0("Action: redundant Hungarian profiles to be removed from '",
               survey_form, "'.\n\n"))
  }



}








# If s1_som ----

if (survey_form == "s1_som") {

  ## Slovakia ----
  # Slovakia has duplicate profiles in 2006 versus 2007/2008.
  # The data from 2006 are the most recent and were corrected
  # in accordance with the PIR issues.
  # Therefore, they are not totally the same like the ones from 2007/2008

  unique_surveys_to_remove <-
    df %>%
    filter(.data$partner_code == 54) %>%
    filter(.data$survey_year > 2000) %>%
    arrange(code_plot, repetition, layer_limit_inferior) %>%
    distinct(.data$unique_survey_repetition, .keep_all = TRUE) %>%
    group_by(plot_id, code_country, code_plot, repetition) %>%
    mutate(survey_year_oldest_update =
             survey_year[which(change_date ==
                                 min(as.Date(.data$change_date)))]) %>%
    ungroup() %>%
    group_by(plot_id, code_country, code_plot, repetition,
             survey_year_oldest_update) %>%
    summarise(count = n(),
              .groups = "drop") %>%
    filter(count > 1) %>%
    mutate(unique_survey = paste0(.data$code_country, "_",
                                  .data$survey_year_oldest_update, "_",
                                  .data$code_plot))


  if (nrow(unique_surveys_to_remove) > 0) {

    for (i in seq_len(nrow(unique_surveys_to_remove))) {

      df_sub <- df %>%
        filter(plot_id == unique_surveys_to_remove$plot_id[i]) %>%
        filter(survey_year > 2000 &
                 survey_year < 2010) %>%
        arrange(code_layer)

      if (nrow(df_sub) > 1) {

        result_columns <- sapply(df_sub[, numeric_column_names],
                                 function(col) length(unique(col)) >
                                   ceiling(nrow(df_sub)/2))

        columns_with_different_content <- names(result_columns[result_columns])

        assertthat::assert_that(identical(columns_with_different_content,
                                           character(0)) ||
                                # Sometimes single values may have been
                                # updated upon resubmission
                                length(columns_with_different_content) < 4)

        ind_to_remove <-
          which(df$unique_survey %in%
                  unique_surveys_to_remove$unique_survey[i])

        df$to_remove[ind_to_remove] <- TRUE

      }
    }

    cat(paste0("Action: redundant Slovak profiles to be removed from '",
               survey_form, "'.\n\n"))
  }



  ## BE_Flanders ----
  # BE_Flanders has duplicate profiles in 2004 versus 2007.
  # The data from 2004 are the most recent


  unique_surveys_to_remove <-
    df %>%
    filter(.data$partner_code == 102) %>%
    filter(.data$survey_year > 2000 &
             .data$survey_year < 2010) %>%
    arrange(code_plot, repetition, layer_limit_inferior) %>%
    distinct(.data$unique_survey_repetition, .keep_all = TRUE) %>%
    group_by(plot_id, code_country, code_plot, repetition) %>%
    mutate(survey_year_oldest_update =
             survey_year[which(change_date ==
                                 min(as.Date(.data$change_date)))]) %>%
    ungroup() %>%
    group_by(plot_id, code_country, code_plot, repetition,
             survey_year_oldest_update) %>%
    summarise(count = n(),
              .groups = "drop") %>%
    filter(count > 1) %>%
    mutate(unique_survey = paste0(.data$code_country, "_",
                                  .data$survey_year_oldest_update, "_",
                                  .data$code_plot))


  if (nrow(unique_surveys_to_remove) > 0) {

    for (i in seq_len(nrow(unique_surveys_to_remove))) {

      df_sub <- df %>%
        filter(plot_id == unique_surveys_to_remove$plot_id[i]) %>%
        filter(survey_year > 2000 &
                 survey_year < 2010) %>%
        arrange(code_layer)

      if (nrow(df_sub) > 1) {

        result_columns <- sapply(df_sub[, numeric_column_names],
                                 function(col) length(unique(col)) >
                                   ceiling(nrow(df_sub)/2))

        columns_with_different_content <- names(result_columns[result_columns])

        assertthat::assert_that(identical(columns_with_different_content,
                                          character(0)) ||
                                  # Sometimes single values may have been
                                  # updated upon resubmission
                                  length(columns_with_different_content) < 4)

        ind_to_remove <-
          which(df$unique_survey %in%
                  unique_surveys_to_remove$unique_survey[i])

        df$to_remove[ind_to_remove] <- TRUE

      }
    }

    cat(paste0("Action: redundant Flemish profiles to be removed from '",
               survey_form, "'.\n\n"))
  }



  ## UK ----

  # #
  # unique_profiles_to_check <- df %>%
  #   filter(layer_type != "mineral") %>%
  #   filter(is.na(layer_limit_superior) | layer_limit_superior < 0) %>%
  #   group_by(unique_survey_repetition) %>%
  #   reframe(contains_o = any(code_layer == "O"),
  #           contains_h = any(code_layer == "H")) %>%
  #   filter(contains_o == TRUE &
  #            contains_h == TRUE) %>%
  #   pull(unique_survey_repetition)
  #
  # unique_profiles_to_check <- df %>%
  #   filter(unique_survey_repetition %in% unique_profiles_to_check) %>%
  #   filter(layer_type != "mineral") %>%
  #   filter(is.na(layer_limit_superior) | layer_limit_superior < 0) %>%
  #   group_by(unique_survey_repetition) %>%
  #   reframe(max_data_count =
  #             max(colSums(!is.na(across(part_size_clay:p_ox))))) %>%
  #   filter(max_count_dat)


  # Some profiles with both an O and H layer, without layer limits.
  # For all parameters, either O or H contains data,
  # except in exch_acidiy: both layers contain the same value
  # Action: merge them into one forest_floor O layer.

  profiles_to_combine_uk <-
    df %>%
    filter(.data$code_country == 6) %>%
    group_by(unique_survey_repetition) %>%
    reframe(both_o_and_h =
                any(.data$code_layer == "O") &
                any(.data$code_layer == "H")) %>%
    filter(.data$both_o_and_h == TRUE) %>%
    pull(unique_survey_repetition)

  profiles_to_combine_uk <- df %>%
    filter(unique_survey_repetition %in% profiles_to_combine_uk) %>%
    filter(code_layer %in% c("O", "H")) %>%
    group_by(unique_survey_repetition) %>%
    reframe(max_count_data =
             max(colSums(!is.na(across(all_of(numeric_column_names)))))) %>%
    filter(max_count_data == 1) %>%
    pull(unique_survey_repetition)

  profiles_to_combine <- profiles_to_combine_uk

  if (!identical(profiles_to_combine, character(0))) {

    assertthat::assert_that(profiles_to_combine == "6_1994_636_1")

    for (i in seq_along(profiles_to_combine)) {

        ind_target <-
          which(df$unique_survey_repetition %in% profiles_to_combine[i] &
                  df$code_layer == "O")
        ind_to_merge <-
          which(df$unique_survey_repetition %in% profiles_to_combine[i] &
                  df$code_layer == "H")

      assertthat::assert_that(length(ind_target) == 1 &&
                                length(ind_to_merge) == 1)

      ind_dupl <- c(ind_target, ind_to_merge)

      df$to_remove[ind_to_merge] <- TRUE

      # Get the columns where values are the same for the duplicated rows
      same_columns <-
        sapply(df[ind_dupl, ], function(col) length(unique(col)) == 1)

      # Extract the column names
      same_column_names <- names(df)[same_columns]

      # Columns which need to be merged
      # (i.e. for which MAN != OPT, or MAN or OPT is NA):

      df_pars_to_merge <- names(df)[!names(df) %in% same_column_names]

      # Summarise each of these parameters

      if (!identical(df_pars_to_merge, character(0))) {
        for (j in seq_along(df_pars_to_merge)) {

          par_j <- as.vector(df_pars_to_merge[j])
          col_ind_j <- as.vector(which(names(df) == par_j))
          values_j <- df[ind_dupl, col_ind_j] %>% pull


          if (any(!is.na(values_j)) && any(is.na(values_j))) {
            summary_value <- values_j[which(!is.na(values_j))]
          } else

            if (all(!is.na(values_j)) && (length(unique(values_j)) == 2)) {

              assertthat::assert_that(
                par_j %in% c("code_layer",
                             "layer_number",
                             "date_labor_analyses",
                             "origin",
                             "layer_type",
                             "code_line",
                             "line_nr",
                             "qif_key",
                             "unique_survey_layer",
                             "unique_layer_repetition",
                             "unique_layer",
                             "code_layer_orig"),
                msg =
                  paste0("Conflicting values for unique_survey_repetition '",
                             profiles_to_combine[i], "' for parameter '",
                             par_j, "'."))

              summary_value <- df[ind_target, col_ind_j]
            }

          # Paste the information in the MAN row

          df[ind_target, col_ind_j] <- summary_value

        } # End of for loop along parameters
      }

    } # End of for loop along profiles with duplicates

    nrow <- length(which(df$to_remove == TRUE))

    cat(paste0("The following profiles by UK (1994) ",
               "contain duplicate organic layers:\n",
               as_character_summary(profiles_to_combine), "\n\n",
               "Action: duplicate records were merged.\n",
               nrow, " redundant records (after merging) to be removed from '",
               survey_form, "'.\n\n"))

  }



  ## Czech Republic ----

  # Due to former mistake in database: Czech plot 2188 actually
  # doesn't exist and refers to plot 188 (see read_raw)

  lines_to_check <- df %>%
    filter(plot_id == "58_188") %>%
    filter(survey_year == 1995)

  if (nrow(lines_to_check) > 0 &&
      length(which(lines_to_check$code_layer == "M01")) == 2 &&
      length(which(lines_to_check$code_layer == "M12")) == 2 &&
      length(unique(lines_to_check$repetition)) == 1 &&
      length(unique(lines_to_check$code_plot_orig)) == 2) {

    lines_to_check <- lines_to_check %>%
      filter(code_plot_orig != 188) %>%
      pull(code_line)

    df <- df %>%
      mutate(repetition = ifelse((!is.na(code_line)) &
                                   code_line %in% lines_to_check,
                                 2,
                                 repetition)) %>%
      mutate(unique_survey_repetition =
               ifelse((!is.na(code_line)) &
                        code_line %in% lines_to_check,
                      paste0(code_country, "_", survey_year, "_",
                             code_plot, "_", repetition),
                      unique_survey_repetition),
             unique_layer_repetition =
               ifelse((!is.na(code_line)) &
                        code_line %in% lines_to_check,
                      paste0(code_country, "_", survey_year, "_",
                             code_plot, "_", code_layer, "_", repetition),
                      unique_layer_repetition))

  }



  ## Germany ----

  # Some profiles (plots) appear partly in one year and
  # partly in another. It is more likely that the survey took place in
  # the first year than in the second.

  unique_surveys_to_check <- df %>%
    distinct(plot_id, survey_year, .keep_all = TRUE) %>%
    group_by(plot_id, code_plot, code_country) %>%
    reframe(
      count = n(),
      min_difference =
        if (count > 1) min(diff(sort(survey_year))) else NA_real_,
      years_min_diff = if (count > 1) {
        years <- sort(survey_year)
        min_diff <- min(diff(years))
        paste(years[c(which(diff(years) == min_diff),
                      which(diff(years) == min_diff) + 1)],
              collapse = "_")
      } else NA_character_) %>%
    filter(!is.na(min_difference) &
             min_difference == 1) %>%
    select(plot_id, code_country, code_plot, years_min_diff)


  if (nrow(unique_surveys_to_check) > 0) {

    assertthat::assert_that(all(unique_surveys_to_check$code_country == 4))

    for (i in seq_along(unique_surveys_to_check)) {

      years_i <- unlist(strsplit(unique_surveys_to_check$years_min_diff[i],
                                 "_"))

      unique_surveys_i <- c(
        paste0(unique_surveys_to_check$code_country[i], "_",
               years_i[1], "_",
               unique_surveys_to_check$code_plot[i]),
        paste0(unique_surveys_to_check$code_country[i], "_",
               years_i[2], "_",
               unique_surveys_to_check$code_plot[i]))

      df_sub <- df %>%
        filter(unique_survey %in% unique_surveys_i)

      assertthat::assert_that(
        # No overlap between the layers of the different survey years
        (length(unique(df_sub$code_layer[
          which(df_sub$survey_year == years_i[1])])) +
           length(unique(df_sub$code_layer[
             which(df_sub$survey_year == years_i[2])]))) ==
          length(unique(df_sub$code_layer)))

      assertthat::assert_that(years_i[1] < years_i[2])

      df$survey_year[which(df$unique_survey %in% unique_surveys_i)] <-
        as.numeric(years_i[1])
    }
  }




} # End of "if s1_som"




# If s1_pfh ----

if (survey_form == "s1_pfh") {


  ## Slovakia ----
  # Slovakia has duplicate profiles in 2006 versus 2007/2008.
  # The data from 2006 are the most recent and were corrected
  # in accordance with the PIR issues.
  # Therefore, they are not totally the same like the ones from 2007/2008

  unique_surveys_to_remove <-
    df %>%
    filter(.data$partner_code == 54) %>%
    filter(.data$survey_year > 2000) %>%
    arrange(code_plot, profile_pit_id, horizon_limit_low) %>%
    distinct(.data$unique_survey_profile, .keep_all = TRUE) %>%
    group_by(plot_id, code_country, code_plot, profile_pit_id) %>%
    mutate(survey_year_oldest_update =
             paste(survey_year[which(change_date ==
                                 min(as.Date(.data$change_date)))],
                   collapse = "_")) %>%
    ungroup()

  unique_surveys_to_remove2 <- unique_surveys_to_remove %>%
    group_by(plot_id, code_country, code_plot, profile_pit_id,
             survey_year_oldest_update) %>%
    summarise(count = n(),
              .groups = "drop") %>%
    filter(count > 1) # %>%


  if (nrow(unique_surveys_to_remove2) > 0) {

    for (i in seq_len(nrow(unique_surveys_to_remove2))) {

      df_sub <- df %>%
        filter(plot_id == unique_surveys_to_remove2$plot_id[i]) %>%
        filter(survey_year > 2000 &
                 survey_year < 2010) %>%
        arrange(horizon_master)

      if (nrow(df_sub) > 1) {

        result_columns <- sapply(df_sub[, numeric_column_names],
                                 function(col) length(unique(col)) >
                                   ceiling(nrow(df_sub)/2) + 1)

        columns_with_different_content <- names(result_columns[result_columns])

        # Some variables can be adjusted during resubmission
        columns_with_different_content <-
          columns_with_different_content[!columns_with_different_content %in%
                                           c("horizon_c_organic_total",
                                             "horizon_n_total",
                                             "horizon_bulk_dens_measure",
                                             "horizon_bulk_dens_est")]

        assertthat::assert_that(identical(columns_with_different_content,
                                          character(0)) ||
                                  # Sometimes single values may have been
                                  # updated upon resubmission
                                  length(columns_with_different_content) < 6)

        ind_to_remove <-
          which(df$plot_id %in%
                  unique_surveys_to_remove2$plot_id[i] &
                df$survey_year %in%
                  unlist(str_split(
                    unique_surveys_to_remove2$survey_year_oldest_update[i],
                    "_")))

        df$to_remove[ind_to_remove] <- TRUE

      }
    }

    cat(paste0("Action: redundant Slovak profiles to be removed from '",
               survey_form, "'.\n\n"))
  }




  ## Italy ----
  # Italy has duplicate profiles in 2008 versus 2007.
  # The data from 2007 are the most recent


  unique_surveys_to_remove <-
    df %>%
    filter(.data$partner_code == 5) %>%
    filter(.data$survey_year > 2000 &
             .data$survey_year < 2010) %>%
    arrange(code_plot, profile_pit_id, horizon_limit_low) %>%
    distinct(.data$unique_survey_profile, .keep_all = TRUE) %>%
    group_by(plot_id, code_country, code_plot, profile_pit_id) %>%
    mutate(survey_year_oldest_update =
             survey_year[which(change_date ==
                                 min(as.Date(.data$change_date)))]) %>%
    ungroup() %>%
    group_by(plot_id, code_country, code_plot, profile_pit_id,
             survey_year_oldest_update) %>%
    summarise(count = n(),
              .groups = "drop") %>%
    filter(count > 1) %>%
    mutate(unique_survey = paste0(.data$code_country, "_",
                                  .data$survey_year_oldest_update, "_",
                                  .data$code_plot))


  if (nrow(unique_surveys_to_remove) > 0) {

    for (i in seq_len(nrow(unique_surveys_to_remove))) {

      df_sub <- df %>%
        filter(plot_id == unique_surveys_to_remove$plot_id[i]) %>%
        filter(survey_year > 2000 &
                 survey_year < 2010) %>%
        arrange(horizon_limit_low)

      if (nrow(df_sub) > 1) {

        result_columns <- sapply(df_sub[, numeric_column_names],
                                 function(col) length(unique(col)) >
                                   ceiling(nrow(df_sub)/2))

        columns_with_different_content <- names(result_columns[result_columns])

        # Some variables can be adjusted during resubmission
        columns_with_different_content <-
          columns_with_different_content[!columns_with_different_content %in%
                                           c("horizon_c_organic_total",
                                             "horizon_n_total",
                                             "horizon_bulk_dens_measure",
                                             "horizon_bulk_dens_est")]

        assertthat::assert_that(identical(columns_with_different_content,
                                          character(0)) ||
                                  # Sometimes single values may have been
                                  # updated upon resubmission
                                  length(columns_with_different_content) < 4)

        ind_to_remove <-
          which(df$unique_survey %in%
                  unique_surveys_to_remove$unique_survey[i])

        df$to_remove[ind_to_remove] <- TRUE

      }
    }

    cat(paste0("Action: redundant Italian profiles to be removed from '",
               survey_form, "'.\n\n"))
  }


  ## Portugal ----

  # Profiles (plots) 10_277 and 10_278 appear partly in 2007 and
  # partly in 2008. Forest floor layers have a different survey year
  # than the below-ground. Based on BIOSOIL.LI.PFH, these so-called
  # survey years may be derived from the analysis date. It is more
  # likely that the survey took place in 2007 than in 2008 for both.

  unique_surveys_to_check <- df %>%
    filter(code_country == 10) %>%
    filter(survey_year %in% c(2007, 2008)) %>%
    distinct(unique_survey, .keep_all = TRUE) %>%
    group_by(plot_id) %>%
    summarise(count = n()) %>%
    filter(count > 1) %>%
    pull(plot_id)

  if (!identical(unique_surveys_to_check, character(0))) {

    for (i in seq_along(unique_surveys_to_check)) {

      assertthat::assert_that(unique_surveys_to_check[i] %in%
                                # Manually checked
                                c("10_277",
                                  "10_278"))

      df_sub <- df %>%
        filter(plot_id == unique_surveys_to_check[i])

      assertthat::assert_that(
        # No overlap between the layers of the different survey years
        (length(unique(df_sub$horizon_master[
          which(df_sub$survey_year == 2007)])) +
          length(unique(df_sub$horizon_master[
            which(df_sub$survey_year == 2008)]))) ==
        length(unique(df_sub$horizon_master)))

      df$survey_year[which(df$plot_id == unique_surveys_to_check[i])] <-
        2007
    }
  }




  ## Sweden and Poland ----

  # Some profiles (plots) appear partly in one year and
  # partly in another. It is more likely that the survey took place in
  # the first year than in the second.

  unique_surveys_to_check <- df %>%
    filter(is.na(to_remove)) %>%
    distinct(plot_id, survey_year, .keep_all = TRUE) %>%
    group_by(plot_id, code_plot, code_country) %>%
    reframe(
      count = n(),
      min_difference =
        if (count > 1) min(diff(sort(survey_year))) else NA_real_,
      years_min_diff = if (count > 1) {
        years <- sort(survey_year)
        min_diff <- min(diff(years))
        paste(years[c(which(diff(years) == min_diff),
                      which(diff(years) == min_diff) + 1)],
              collapse = "_")
      } else NA_character_) %>%
    filter(!is.na(min_difference) &
             min_difference == 1) %>%
    select(plot_id, code_country, code_plot, years_min_diff)


  if (nrow(unique_surveys_to_check) > 0) {

    assertthat::assert_that(
      all(unique_surveys_to_check$code_country %in% c(13, 53)))

    for (i in seq_along(unique_surveys_to_check)) {

      years_i <- unlist(strsplit(unique_surveys_to_check$years_min_diff[i],
                                 "_"))

      unique_surveys_i <- c(
        paste0(unique_surveys_to_check$code_country[i], "_",
               years_i[1], "_",
               unique_surveys_to_check$code_plot[i]),
        paste0(unique_surveys_to_check$code_country[i], "_",
               years_i[2], "_",
               unique_surveys_to_check$code_plot[i]))

      df_sub <- df %>%
        filter(unique_survey %in% unique_surveys_i)

      assertthat::assert_that(
        # No overlap between the layers of the different survey years
        (length(unique(df_sub$horizon_limit_up[
          which(df_sub$survey_year == years_i[1] &
                  !is.na(df_sub$horizon_limit_up))])) +
           length(unique(df_sub$horizon_limit_up[
             which(df_sub$survey_year == years_i[2] &
                     !is.na(df_sub$horizon_limit_up))]))) ==
          length(unique(df_sub$horizon_limit_up[
            which(!is.na(df_sub$horizon_limit_up))])))

      assertthat::assert_that(years_i[1] < years_i[2])

      df$survey_year[which(df$unique_survey %in% unique_surveys_i)] <-
        as.numeric(years_i[1])
    }
  }





  ## Sweden/Spain ----

  # Some profiles in Sweden and one profile in Spain consist of one horizon
  # without any data

  # This also includes two Swedish layers without horizon_master and layer
  # limits

  records_to_check <- df %>%
    group_by(unique_survey_profile, plot_id) %>%
    reframe(count = n(),
            no_toc = all(is.na(horizon_c_organic_total)),
            no_bd = all(is.na(horizon_bulk_dens_measure) |
                          is.na(horizon_bulk_dens_est)),
            no_coarse = all(is.na(horizon_coarse_weight)),
            no_exch = all(is.na(horizon_exch_ca) &
                            is.na(horizon_cec)),
            no_layer = all((is.na(horizon_master) |
                             horizon_master == "") &
                             (is.na(horizon_limit_up) &
                                is.na(horizon_limit_low)))) %>%
    filter(count == 1 &
             no_toc == TRUE &
             no_bd == TRUE &
             no_coarse == TRUE &
             (no_exch == TRUE |
                no_layer == TRUE))

  if (nrow(records_to_check) > 0) {

    df <- df %>%
      mutate(to_remove =
               ifelse(unique_survey_profile %in%
                        pull(records_to_check, unique_survey_profile),
                      TRUE,
                      .data$to_remove))
  }



} # End of "if s1_pfh"



# Remove records that should be removed ----

if (solve == TRUE) {
  nrow <- length(which(df$to_remove == TRUE))

  df <- df %>%
    filter(is.na(.data$to_remove) | .data$to_remove == FALSE) %>%
    select(-to_remove)

  cat(paste0("In total, ",
             nrow, " redundant records have been removed.\n\n"))
}



  # Update unique identifiers ----

  if (unlist(strsplit(survey_form, "_"))[2] == "som") {

    df <- df %>%
      mutate(plot_id = paste0(code_country, "_",
                              code_plot),
             unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot),
             unique_survey_repetition = paste0(code_country, "_",
                                               survey_year, "_",
                                               code_plot, "_",
                                               repetition),
             unique_survey_layer = paste0(code_country, "_",
                                          survey_year, "_",
                                          code_plot, "_",
                                          code_layer),
             unique_layer_repetition = paste0(code_country, "_",
                                              survey_year, "_",
                                              code_plot, "_",
                                              code_layer, "_",
                                              repetition))
  }

  if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

    df <- df %>%
      mutate(plot_id = paste0(code_country, "_",
                              code_plot),
             unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot),
             unique_survey_profile = paste0(code_country, "_",
                                            survey_year, "_",
                                            code_plot, "_",
                                            profile_pit_id),
             unique_survey_layer = paste0(code_country, "_",
                                          survey_year, "_",
                                          code_plot, "_",
                                          horizon_master),
             unique_layer_repetition = paste0(code_country, "_",
                                              survey_year, "_",
                                              code_plot, "_",
                                              horizon_master, "_",
                                              profile_pit_id))
  }









# Save the survey form ----

if (save_to_env == TRUE) {
  assign_env(survey_form, df)
} else {
  return(df)
}


}
