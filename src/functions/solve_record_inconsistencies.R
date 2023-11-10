
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
  
# Retrieve the survey_form data

if (is.null(data_frame)) {
  df <- get_env(survey_form)
} else {
  df <- data_frame
}
  
  df$to_remove <- NA
  
  
# DE_BW (1995 and 1996) ----
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




# DE_BB (1995) ----
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




# DE_SN (1995) ----
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
                           "code_layer_original"),
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


# Poland (1999) ----

profiles_h_to_remove_poland <-
  df %>%
  ungroup %>%
  filter(.data$partner_code == 53) %>%
  filter(.data$survey_year < 2000) %>%
  group_by(unique_survey_repetition) %>%
  summarise(contains_h =  any(.data$code_layer == "H")) %>%
  filter(.data$contains_h == TRUE) %>%
  pull(unique_survey_repetition)



# Get columns with numeric content but don't end with "_loq" or "_rt"
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





# Norway ----

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

df %>%
  filter(unique_survey %in% surveys_to_check) %>%
  filter(code_layer == "O")


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






# BE_Flanders ----

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








# Change layer_type to "forest_floor"

if (solve == TRUE) {
df <- df %>%
  mutate(layer_type = as.character(.data$layer_type)) %>%
  mutate(layer_type = ifelse(.data$code_layer == "H",
                             "forest_floor",
                             .data$layer_type))
  
}






if (solve == TRUE) {
  nrow <- length(which(df$to_remove == TRUE))
  
  df <- df %>%
    filter(is.na(.data$to_remove) | .data$to_remove == FALSE) %>%
    select(-to_remove)
    
  cat(paste0("In total, ",
             nrow, " redundant records have been removed.\n\n"))
}

# Save the survey form and list_range_inconsistencies for the given survey
# form to the global environment

if (save_to_env == TRUE) {
  assign_env(survey_form, df)
} else {
  return(df)
}


}
