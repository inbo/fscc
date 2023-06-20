
#' Perform a spatial join based on proximity (with distance threshold)
#' 
#' This function performs a spatial join between two sf (Simple Features) 
#' data frames based on distance with a distance threshold. It matches features
#' from `sf2` to the corresponding features in `sf1` based on their proximity
#' within the specified distance threshold.
#'
#' @param sf1 Character string - the first sf data frame to be joined.
#' @param sf2 Character string - the second sf data frame to be joined.
#' @param dist_threshold Numeric - the maximum distance threshold (in meters)
#'   for matching features between `sf1` and `sf2`. Default is 2000 (meter).
#' @param join_column The column name in `sf2` that you want to join to `sf1`.
#'   If not provided, a column with the name "colocated_(name sf2)" (if
#'   include_threshold is FALSE) or "colocated_(name sf2)_(dist_threshold)"
#'   (if include_threshold is TRUE) is added to sf1 which indicates if there
#'   is a match in sf2 (logical).
#' @param summary_stat Character string - the summary statistic to apply when
#'   there are multiple matches for a feature in `sf1`. The available options
#'    are "most_abundant", "mean", "median". Default is "most_abundant".
#' @param include_threshold Logical which indicates whether the distance
#'   threshold (in meters) should be added to the new column in sf1, provided
#'   that no "join_column" has been given as input argument. Default is FALSE.
#'
#' @return A new sf data frame containing the joined data from `sf2` appended
#'   to `sf1`. The join is based on the distance threshold and the specified
#'   join_column. The summary statistic is applied to handle multiple matches,
#'   if any.
#'
#' @details
#' 
#' Joins of ICP Forests data based on "plot_ID" (i.e. the combination of
#' partner_code and code_plot) are not always reliable when different surveys
#' (i.e. different monitoring subject, e.g. "so" versus "s1", or different
#' survey years of the same survey, e.g. 2007 versus 2020 of "so") have to be
#' merged. Therefore, it makes more sense to match records based on proximity.
#' 
#' For each of the records in sf1, this function searches for spatial matches
#' in sf2 (i.e. all records of sf2 which are within a certain distance threshold)
#' of the given record of sf1.
#' 
#' If the "join_column" input argument is not provided, a column is added to sf1
#' with the following name:
#' - if include_threshold is FALSE: "colocated_(name sf2)"
#' - if include_threshold is TRUE:  "colocated_(name_sf2)_(dist_threshold)
#' This column is set to TRUE if at least one match in sf2 has been found.
#' 
#' If the "join_column" input argument is provided, a column with this name is
#' added to sf1, and the column of sf2 with this name is summarised in this
#' new column in sf1:
#' if there is at least one match in sf2 for a given record of sf1:
#' summarise the data in this column of sf2 for the matched records, using the
#' summary statistic which is provided in the "summary_stat" input argument,
#' i.e. either the most abundant value, the mean or the median.
#' 
#' Assumptions:
#' \itemize{
#'   \item `sf1` and `sf2` should be valid sf data frames with geometries.
#'   \item The geometries in both `sf1` and `sf2` should be in the same
#'     coordinate reference system (CRS) for accurate distance calculations.
#'   \item The `join_column` in `sf2` should contain values that can be 
#'     summarized using the specified `summary_stat` (e.g., numeric values for
#'     "mean" or "median" calculations).
#' }
#'
#' @examples
#' # Example usage of distance_join function
#' distance_join(sf1, sf2, dist_threshold = 2000,
#'               join_column = "id", summary_stat = "most_abundant",
#'               include_threshold = FALSE)
#'
#' @references Provide any references or related resources here.
#'
#' @export

distance_join <- function(sf1, sf2, dist_threshold = NULL, join_column = NULL,
                          summary_stat = NULL, include_threshold = NULL) {
  
  # Install packages ----
  
  # Define packages to install
  packages_map_icpf <- c("sf", 
                         "tidyverse")
  
  # Install all packages that are not already installed
  install.packages(setdiff(packages_map_icpf, rownames(installed.packages())))
  
  # Load packages
  sapply(packages_map_icpf, library, character.only = TRUE)
  
  
  # Prepare sf1 and sf2 ----
  
  
  # Retrieve sf survey forms from global environment
  
  sf1_input <- sf1
  sf2_input <- sf2
  sf1 <- get(sf1_input, envir = .GlobalEnv)
  sf2 <- get(sf2_input, envir = .GlobalEnv)
  
  # Does sf1 exist and is it an sf dataframe?
  assertthat::assert_that(!missing(sf1) &&
                            exists("sf1", inherits = FALSE,
                                   envir = environment()) &&
                            identical(class(sf1), c("sf", "data.frame")),
                          msg = paste0("The object '",
                                       sf1,
                                       "' of class 'sf' (data frame) has not ",
                                       "been found in the global environment."))
  
  # Does sf2 exist and is it an sf dataframe?
  assertthat::assert_that(!missing(sf2) &&
                            exists("sf2", inherits = FALSE,
                                   envir = environment()) &&
                            identical(class(sf2), c("sf", "data.frame")),
                          msg = paste0("The object '",
                                       sf2,
                                       "' of class 'sf' (data frame) has not ",
                                       "been found in the global environment."))
  
  
  
  # Prepare dist_threshold ----
  
  # If no value is provided for the "dist_threshold" argument,
  # set it to 2000 meter (by default)
  # This default value of 2000 meter is based on:
  # - The following sentences:
  #   "We consider a threshold of 2 km, approximately the distance where
  #    coordinates were rounded to the minute level. [...] For [...] plots,
  #    distance was more than 2 km and therefore these plots are considered to
  #    have different locations." (page 94, section 3.1.2.2)
  #   in the following reference:
  #   De Vos, B. and Cools, N. 2011. Second European Forest Soil Condition Report.
  #   Volume I: Results of the BioSoil Soil Survey. INBO.R.2011.35. Research
  #   Institute for Nature and Forest, Brussel
  # - The experience that joins with a lower distance threshold leads to hardly
  #   any matches (e.g. distance join based on 50 meter leads to only 3 % of
  #   the amount of matches with a distance join based on 2000 meter in the
  #   surveys "so" versus "lf")
  
  if (is.null(dist_threshold)) {
    dist_threshold <- 2000} else { # meter
      assertthat::assert_that(is.numeric(dist_threshold) &&
                                (dist_threshold >= 0),
                              msg = paste0("The object 'dist_threshold' should ",
                                           "be a positive numeric."))
    }
  
  
  # Prepare include_threshold ----
  
  # If no value is provided for the "include_threshold" argument,
  # set it to FALSE
  if (is.null(include_threshold)) {
    include_threshold <- FALSE}
  
  
  # Prepare join_column ----
  
  # If no value is provided for the "join_column" argument
  if (is.null(join_column)) {
    
    # Determine name of the new column
    new_col_name <- ifelse(include_threshold == TRUE,
                           yes = paste0("colocated_", sf2_input, "_",
                                        dist_threshold, "m"),
                           no = paste0("colocated_", sf2_input))
    
    # Add a new column
    sf1$new_col <- NA
    # names(sf1)[which(names(sf1) == "new_col")] <- new_col_name
    
  } else {
    
    # If a value is provided for the "join_column" argument
    
    # Assert that the column exists
    assertthat::assert_that(join_column %in% names(sf2),
                            msg = paste0("No column with the name '",
                                         join_column,
                                         "' exists in '",
                                         sf2_input, "'."))
    
    # Add a new column
    sf1$new_col <- NA
    new_col_name <- join_column
  }
  
  
  # Prepare summary_stat ----
  
  # If no value is provided for the "summary_stat" argument
  if (is.null(summary_stat)) {
    # Set it to "most_abundant"
    summary_stat <- "most_abundant"
    
  } else {
    # If "summary_stat" was given as input argument
    # Assert that "summary_stat" is in the list of options
    assertthat::assert_that(summary_stat %in%
                              c("most_abundant", "mean", "median"),
                            msg = paste0("The provided 'summary_stat' argument ",
                                         "is not in the list of possible ",
                                         "options for this input argument. ",
                                         "Options are 'most_abundant', 'mean' ",
                                         "or 'median'."))
    
    # If "summary_stat" is "mean" or "median" and join_column is NULL
    if ((summary_stat %in% c("mean", "median")) &&
        (!is.null(join_column))) {
      
      # Assert that the column to join is numeric
      assertthat::assert_that(is.numeric(sf2[, which(
        names(sf2) == join_column)]),
        msg = paste0("The provided 'join_column' of '",
                     sf2_input,
                     "', i.e. '",
                     join_column,
                     "', should be numeric since 'summary_stat' is set to '",
                     summary_stat,
                     "' in the input arguments."))
    }}
  
  
  # Search for distance matches for each of the plots (records) in sf1 ----
  
  # Set progress bar
  progress_bar <- txtProgressBar(min = 0, max = nrow(sf1), style = 3)
  
  for (i in seq_len(nrow(sf1))) {
    
    # Look for row indices in sf2 which are within the threshold distance of
    # row i in sf1
    
    vec <- which(st_is_within_distance(sf1[i, ],
                                       sf2,
                                       dist = dist_threshold, # Units = meter
                                       sparse = FALSE) == TRUE); 
    
    # If there is at least one matching row
    if (!identical(vec, integer(0))) {
      
      # if join_column was not provided
      if (is.null(join_column)) {
        
        # Set the new column in sf1 to TRUE
        sf1$new_col[i] <- TRUE
        
      } else {
        
        # if join_column was provided
        
        # Derive the column index of the join_column in sf2
        col_ind <- which(names(sf2) == join_column)
        
        # Extract the data and remove NAs
        vec_data <- sf2[vec, col_ind]
        vec_data <- vec_data[!is.na(vec_data)]
        
        # If vec_data is not all NA
        if (!identical(vec_data, logical(0))) {
          
          # If the length of vec_data is 1
          if (length(vec_data) == 1) {
            
            # Set the value in the new column of sf1 to this value
            sf1$new_col[i] <- vec_data
            
          } else if (length(vec_data) > 1) {
          # If the length of vec_data is > 1
            
            # If summary_stat is "most_abundant"
            if (summary_stat == "most_abundant") {
              
            # Count occurrences of each unique value
            value_counts <- table(vec_data)
            
            # Find the most abundant value
            most_abundant_value <- names(value_counts)[which.max(value_counts)]
            
            # Convert the most abundant value back to the original type
            if (typeof(vec_data) == "numeric") {
              most_abundant_value <- as.numeric(most_abundant_value)
            } else if (typeof(vec_data) == "character") {
              most_abundant_value <- as.character(most_abundant_value)
            } else if (typeof(vec_data) == "factor") {
              most_abundant_value <- as.factor(most_abundant_value)
            } else if (typeof(vec_data) == "double") {
              most_abundant_value <- as.double(most_abundant_value)
            } else if (typeof(vec_data) == "integer") {
              most_abundant_value <- as.integer(most_abundant_value)
            }
            
            sf1$new_col[i] <- most_abundant_value};
            
            # If summary_stat is "mean"
            if (summary_stat == "mean") {
              
              sf1$new_col[i] <- mean(vec_data)};
            
            # If summary_stat is "median"
            if (summary_stat == "median") {
              
              sf1$new_col[i] <- median(vec_data)};
            }
        }
      }
    }
    
    # Update progress bar
    setTxtProgressBar(progress_bar, i)
  }
  
  close(progress_bar)
  
  # Update the name of the new column in sf1
  names(sf1)[which(names(sf1) == "new_col")] <- new_col_name
  
  # Save sf dataframe to global environment
  assign(sf1_input, sf1, envir = globalenv())
}

