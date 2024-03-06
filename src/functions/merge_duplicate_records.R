
#' Merge duplicate records in "so_som" survey form
#'
#' This function merges the non-conflicting data of double records in the
#' "so_som" form to one record.
#'
#' @param survey_form Character string - Name of the survey form (lower case and
#' separated by '_') to be evaluated
#' @param merge Logical - Indicates whether records need to be merged (TRUE) or
#' indicated using an extra column (FALSE)
#' @param save_to_env Logical which indicates whether the output dataframe
#' can be saved to the global environment and override any existing objects
#' with the same name. Default is FALSE.
#' @details
#' For "so_som"
#'
#' This merging process results in a more concise and
#' consistent "so_som" data form, in which one individual record corresponds
#' uniquely with one sample, i.e. one record per unique_layer_repetition
#' (partner_code x survey_year x code_plot x code_layer x repetition).
#'
#' The issue of double records in the "so_som" form arises from the historical
#' separation of mandatory (SOM) and optional (SOO) parameters during the data
#' reporting. Some countries reported both the mandatory and optional
#' parameters, while others reported only the primary key and optional
#' parameters. To address this issue, this function merges these duplicate
#' records, by taking the unique value over the two records per parameter.
#' In case of conflicts (i.e. different non-NA value in MAN versus OPT for the
#' given parameter), the value in MAN is retained, except for the "other_obs"
#' column, in which the different values are pasted into one string (separated
#' by a semicolon).
#'
#' @examples
#' merge_duplicate_records("so_som", merge = TRUE)

merge_duplicate_records <- function(survey_form,
                                    data_frame = NULL,
                                    merge = FALSE,
                                    save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(" \nMerge duplicate records 'so_som'\n")


  # Retrieve the survey_form data ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }


# Store the number of initial rows

nrow_initial <- nrow(df)

# Issue with Latvian records ----
# Four Latvian records (survey_year 2004; code_plot 5) do not have
# code_layer nor layer limit information, and therefore have the same
# unique_layer_repetition. However, based on "code_line", the records can
# be grouped in two MAN-OPT pairs. This line updates unique_layer_repetition
# based on this information:

if (survey_form == "so_som") {

ind_latvia <- which(df$unique_layer_repetition == "64_2004_5_NA_1")

if (length(which(duplicated(df$unique_layer_repetition[ind_latvia]))) == 3) {

  # Find indices containing "001"
  ind_latvia_001 <- which(grepl("001", df$code_line[ind_latvia]))

  # Find indices containing "002"
  ind_latvia_002 <- which(grepl("002", df$code_line[ind_latvia]))

  # Update unique_layer_repetition

  df$unique_layer_repetition[ind_latvia[ind_latvia_001]] <- "64_2004_5_001_1"
  df$unique_layer_repetition[ind_latvia[ind_latvia_002]] <- "64_2004_5_002_1"
  }
}


# Evaluate for each of the duplicated unique_layer_repetition ----


# List of unique_layer_repetition for which the combination of
# unique_layer_repetition and the layer limits are duplicated

  # The Slovakian plot with double OL and different layer limits
  # (54_2007_208_OL_1) is not part of this, because that case is an
  # issue with the primary key/code_layer name

vec_dupl <- unique(df$unique_layer_repetition[which(duplicated(
  paste0(df$unique_layer_repetition, "_",
         df$layer_limit_inferior, "_",
         df$layer_limit_superior)))])




# Set up a progress bar to track processing

if (!isTRUE(getOption("knitr.in.progress"))) {
progress_bar <- txtProgressBar(min = 0,
                               max = length(vec_dupl), style = 3)
}

# Create lists of parameters for which different merge rules apply

  # Rule:
  # if both values are the same: take the unique value,
  # if there is only one non-NA: take non-NA value,
  # if conflicting values: take value for origin "MAN"

list_man <- c("country", "partner_short", "partner", "survey_year",
              "code_country", "code_plot", "code_layer",
              "code_layer_original", "repetition",
              "layer_limit_superior", "layer_limit_inferior",
              "date_labor_analyses", "origin", "code_soil_horizon_sample_c",
              "partner_code", "q_flag", "change_date", "code_line", "line_nr",
              "qif_key", "download_date", "layer_type", "plot_id",
              "unique_survey", "unique_survey_repetition",
              "unique_survey_layer", "unique_layer_repetition", "unique_layer",
              "subsamples", "code_texture_class",
              "moisture_content", "part_size_clay", "part_size_silt",
              "part_size_sand", "coarse_fragment_vol", "organic_layer_weight",
              "ph_cacl2", "ph_h2o", "organic_carbon_total", "n_total",
              "carbonates", "exch_acidiy", "exch_al", "exch_ca", "exch_fe",
              "exch_k", "exch_mg", "exch_mn", "exch_na", "free_h", "extrac_al",
              "extrac_ca", "extrac_cd", "extrac_cr", "extrac_cu", "extrac_fe",
              "extrac_hg", "extrac_k", "extrac_mg", "extrac_mn", "extrac_na",
              "extrac_ni", "extrac_p", "extrac_pb", "extrac_s", "extrac_zn",
              "tot_al", "tot_ca", "tot_fe", "tot_k", "tot_mg",
              "tot_mn", "tot_na", "rea_al", "rea_fe", "exch_bce",
              "exch_ace", "exch_cec", "elec_cond", "ni", "base_saturation",
              "p_ox", "bulk_density", "partner_code_orig", "code_plot_orig",
              "bulk_density_source", "organic_layer_weight_source",
              "organic_carbon_total_source", "n_total_source",
              "code_layer_orig", "part_size_clay_orig", "part_size_silt_orig",
              "part_size_sand_orig", "bulk_density_orig",
              "coarse_fragment_vol_orig", "organic_layer_weight_orig",
              "organic_carbon_total_orig", "n_total_orig",
              "part_size_clay_source", "part_size_silt_source",
              "part_size_sand_source", "coarse_fragment_vol_source")

  # Rule:
  # if both values are the same: take the unique value,
  # if there is only one non-NA: take non-NA value,
  # if conflicting values: paste the value for the "MAN" record (first) +
  #                        then the value for the "OPT" record (second)

list_paste_alert <- c("other_obs")

# Assert that all columns are in one of these lists

source("./src/functions/as_character_summary.R")

df_parameters <- names(df)
df_parameters_extra <- df_parameters[which(!df_parameters %in%
                                           c(list_man, list_paste_alert))]

assertthat::assert_that(identical(df_parameters_extra, character(0)),
                        msg = paste0("The function does not know how to",
                                     " summarise the following columns: ",
                                     as_character_summary(df_parameters_extra)))

# Add a column for the OPT records which should be removed after merging

df$to_remove <- NA

# Add a column for the MAN records which will indicate later on whether a row
# has been merged in the history

df$origin_merged <- NA

# Iterate over each double record
for (i in seq_along(vec_dupl)) {

  # Row indices of duplicate rows i
  ind_dupl <- which(df$unique_layer_repetition == vec_dupl[i])

  # Row index of MAN row from i
  ind_man <- ifelse(all(!is.na(df$origin[ind_dupl])),
                    ind_dupl[which(df$origin[ind_dupl] == "MAN")],
                    ind_dupl[which(!is.na(df$organic_carbon_total[ind_dupl]))])
      # In some German records, the origin column is NA, even though
      # the double records clearly arise from the same MAN vs OPT issue:
      # the row which contains total organic carbon data is considered as "MAN"

  # Row index of OPT row from i
  ind_opt <- ind_dupl[!ind_dupl %in% ind_man]

  # Purpose: to complete the MAN row and remove the OPT row later on

  # Add a column for the MAN records which will indicate later on whether a row
  # has been merged in the history

  df$origin_merged[ind_man] <- TRUE

  # Indicate if row has to be removed

  df$to_remove[ind_opt] <- TRUE

  # Get the columns where values are the same for the duplicated rows
  same_columns <- sapply(df[ind_dupl, ], function(col) length(unique(col)) == 1)

  # Extract the column names
  same_column_names <- names(df)[same_columns]

  # Columns which need to be merged
  # (i.e. for which MAN != OPT, or MAN or OPT is NA):

  df_pars_to_merge <- df_parameters[!df_parameters %in% same_column_names]

  # Summarise each of these parameters

  if (!identical(df_pars_to_merge, character(0))) {
  for (j in seq_along(df_pars_to_merge)) {

    par_j <- df_pars_to_merge[j]
    col_ind_j <- which(names(df) == par_j)
    values_j <- df[ind_dupl, col_ind_j] %>% pull

    # Parameters for which data in "MAN" row should be taken

    if (par_j %in% list_man) {

      if (any(!is.na(values_j)) && any(is.na(values_j))) {
        summary_value <- values_j[which(!is.na(values_j))]
        } else

      if (all(!is.na(values_j)) && (length(unique(values_j)) == 2)) {
        summary_value <- df[ind_man, col_ind_j]
        }
      }

    # Parameters for which contrasting data should be pasted together

    if (par_j %in% list_paste_alert) {

      if (any(!is.na(values_j)) && any(is.na(values_j))) {
        summary_value <- values_j[which(!is.na(values_j))]
        } else

      if (all(!is.na(values_j)) && (length(unique(values_j)) == 2)) {
        summary_value <- paste0(df[ind_man, col_ind_j], "; ",
                                df[ind_opt, col_ind_j])
        }
      }

    # Paste the information in the MAN row

    df[ind_man, col_ind_j] <- summary_value

  }
  }

  # Update progress bar

  if (!isTRUE(getOption("knitr.in.progress"))) {
  setTxtProgressBar(progress_bar, i)
  }
} # End of "for" loop

if (!isTRUE(getOption("knitr.in.progress"))) {
close(progress_bar)
}

# Determine the amount of rows to be removed
# and the survey_years of these rows

years <- sort(unique(df$survey_year[which(!is.na(df$to_remove))]))
nrow_final <- nrow_initial - length(which(!is.na(df$to_remove)))
rel_reduction <- round(100 * (nrow_initial - nrow_final) / nrow_initial)

# If there are no double records

if (identical(which(!is.na(df$to_remove)), integer(0))) {

  # Print the following:
  cat("There are no double records.")
  } else {

# If there are double records

  # Add column with origin_merge_info
  df$origin_merge_info <-
    paste0("From ", nrow_initial, " to ",
           nrow_final, " rows (range survey years: ",
           min(years), " - ", max(years), ")")


  # If the duplicate rows have to be removed
  if (merge == TRUE) {

    # Remove the OPT rows from the dataframe
    df <- df[-which(!is.na(df$to_remove)),
             -which(names(df) == "to_remove")]

    if (save_to_env == TRUE) {
    # Save dataframe in global environment
    assign_env(survey_form, df)
    } else {
      return(df)
    }

    # Print the following:
    cat(paste0("Number of rows was reduced by ",
               rel_reduction,
               " %, i.e. from ", nrow_initial, " to ", nrow_final, " rows.\n",
               "Records have been removed from survey years between ",
               min(years), " and ", max(years), ".\n"))

    } else {

  # If the duplicate rows do not have to be removed

  if (save_to_env == TRUE) {
  # Save dataframe in global environment
  assign_env(survey_form, df)
  } else {
    return(df)
  }

  # Print the following:
  cat(paste0("Suggestion to remove the number of rows by ",
             rel_reduction,
             " %, i.e. from ", nrow_initial, " to ", nrow_final, " rows.\n",
             "Records to remove are indicated with TRUE in the column ",
             "'to_remove', and belong to the survey years between ", min(years),
             " and ", max(years), ".\n"))

    }
  }

}



extract_merge_info <- function(survey_form) {

  source("./src/functions/get_env.R")

  # Retrieve data form from the global environment
  df <- get_env(survey_form)

  # Extract origin_merge_info
  origin_merge_info <- unique(df$origin_merge_info)
  if (length(origin_merge_info) != 1) {
    stop("origin_merge_info contains multiple unique values.")
  }

  # Extract the numeric values from origin_merge_info using regular expressions
  nrow_initial <-
    as.character(as.integer(str_extract(origin_merge_info, "\\d+")))
  nrow_final <-
    as.character(as.integer(str_extract(origin_merge_info, "\\d+(?= rows)")))
  year_range <- str_extract(origin_merge_info, "(?<=survey years: )\\d+ - \\d+")
  min_year <- as.character(as.numeric(str_extract(year_range, "\\d+")))
  max_year <- as.character(as.numeric(str_extract(year_range, "(?<= - )\\d+")))

  return(c(nrow_initial = nrow_initial,
           nrow_final = nrow_final,
           min_year = min_year,
           max_year = max_year))
}
