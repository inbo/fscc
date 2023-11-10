
#' Expand unique survey vector by adjacent years
#'
#' This function expands a vector of unique survey strings by
#' adding adjacent years based on the given number of years.
#' 
#' This is useful to match unique surveys (plot_id x survey_year)
#' among survey forms, when some variation in the survey year is possible.
#'
#' @param unique_survey_vec A character vector of unique survey strings
#' in the format "XX_YYYY_Z", where XX is some identifier, YYYY is the year,
#' and Z is an additional identifier. Like "code_country" _ "survey_year" _
#' "code_plot" in the solid soil surveys of ICP Forests.
#' @param number_of_years An integer specifying the number of years to
#' expand the survey strings. For each year in the original vector,
#' the function will add surveys from (year - number_of_years) to
#' (year + number_of_years).
#'
#' @return A character vector containing the expanded unique survey
#' strings, including the original surveys and the surveys from adjacent years.
#'
#' @examples
#' unique_survey_vec <- c("14_1995_1", "14_2006_1", "14_1995_2", "14_2006_2")
#' expand_unique_survey_vec_adjacent_years(unique_survey_vec, 2)
#'

expand_unique_survey_vec_adjacent_years <- function(unique_survey_vec,
                                                    number_of_years) {
  
  # Extract the year
  years <- gsub(".*_(\\d{4}).*", "\\1", unique_survey_vec)
  
  # Replace each year one by one
  unique_surveys <- NULL
  
  for (i in seq_along(unique_survey_vec)) {
    
    unique_surveys <- c(
      unique_surveys,
      # Add unique_survey
      unique_survey_vec[i])
    
    for (j in seq_len(number_of_years)) {
      
      unique_surveys <- c(
        unique_surveys,
        # Add survey_year - 1
        gsub("_(\\d{4})_",
             paste0("_", as.character(as.numeric(years[i]) - j), "_"),
             unique_survey_vec[i]),
        # Add survey_year + 1
        gsub("_(\\d{4})_",
             paste0("_", as.character(as.numeric(years[i]) + j), "_"),
             unique_survey_vec[i]))
    }
  }
  
  return(unique_surveys)
  
}