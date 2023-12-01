
#' Convert unique_survey vector to plot_id vector
#'
#' This function takes a vector of unique_survey identifiers and converts them
#' to plot_ids by omitting the survey year.
#'
#' @param unique_survey_vec A character vector containing unique_survey
#' identifiers.
#'
#' @return A character vector with the corresponding plot_ids.


as_plot_id <- function(unique_survey_vec) {

    # Use gsub to replace the inner _XXXX_ with a single underscore

  converted_vector <- ifelse(grepl("_[0-9]{4}_", unique_survey_vec),
                             gsub("_[0-9]{4}_", "_", unique_survey_vec),
                             unique_survey_vec)

  return(converted_vector)
}
