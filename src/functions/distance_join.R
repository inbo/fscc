
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
#' @param save_to_global Logical which indicates whether the output sf1 can
#' be saved to the global environment and override the existing sf1 object.
#' Default is FALSE.
#'
#' @return A new sf data frame containing the joined data from `sf2` appended
#'   to `sf1`. The join is based on the distance threshold and the specified
#'   join_column. The summary statistic is applied to handle multiple matches,
#'   if any.
#'
#' @details
#'
#' Joins of ICP Forests data based on "plot_id" (i.e. the combination of
#' partner_code and code_plot) are not always reliable when different surveys
#' (i.e. different monitoring subject, e.g. "so" versus "s1", or different
#' survey years of the same survey, e.g. 2007 versus 2020 of "so") have to be
#' merged. Therefore, it may make sense to match records based on proximity.
#'
#' However, on the other hand, there are also plenty of opportunities to
#' introduce mistakes or be inaccurate during the reporting of the coordinates
#' (in spite of efforts to get the coordinates in the raw data correct).
#' Overall, plot_id's are actually probably still more reliable than
#' coordinates. If different coordinates are reported for a certain plot, then
#' it is still more likely that the coordinates are wrong, than that there is
#' an issue with the plot_id. So the function 'left_join(..., by = "plot_id")'
#' should be better.
#'
#' For each of the records in sf1, this function searches for spatial matches
#' in sf2 (i.e. all records of sf2 which are within a certain distance
#' threshold) of the given record of sf1.
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

distance_join <- function(sf1,
                          sf2,
                          dist_threshold = 2000, # meter
                          join_column = NULL,
                          summary_stat = c("most_abundant", "mean", "median"),
                          include_threshold = FALSE,
                          save_to_global = FALSE) {

  # Load packages ----

  stopifnot(require("assertthat"),
            require("sf"),
            require("tidyverse"))


  # Prepare sf1 and sf2 ----

  # Store names of input sf survey forms to use later on
  sf1_input <- sf1
  sf2_input <- sf2

  # Retrieve sf survey forms from global environment
  sf1 <- get(sf1_input, envir = .GlobalEnv)
  sf2 <- get(sf2_input, envir = .GlobalEnv)

  # Does sf1 exist and is it an sf dataframe?
  assertthat::assert_that(inherits(sf1, "sf"),
                          # Also yields an error when sf1 is missing
                          msg = paste0("The object '",
                                       sf1_input,
                                       "' of class 'sf' (data frame) has not ",
                                       "been found in the global environment."))

  # Does sf2 exist and is it an sf dataframe?
  assertthat::assert_that(inherits(sf2, "sf"),
                          # Also yields an error when sf1 is missing
                          msg = paste0("The object '",
                                       sf2_input,
                                       "' of class 'sf' (data frame) has not ",
                                       "been found in the global environment."))



  # Prepare dist_threshold ----

  # If no value is provided for the "dist_threshold" argument,
  # set it to 2000 meter (by default)
  # This default value of 2000 meter is based on:
  # - The following sentences:
  #     "We consider a threshold of 2 km, approximately the distance where
  #     coordinates were rounded to the minute level. [...] For [...] plots,
  #     distance was more than 2 km and therefore these plots are considered to
  #     have different locations." (page 94, section 3.1.2.2)
  #   in the following reference:
  #   De Vos, B. and Cools, N. 2011. Second European Forest Soil Condition
  #   Report. Volume I: Results of the BioSoil Soil Survey. INBO.R.2011.35.
  #   Research Institute for Nature and Forest, Brussel
  # - The experience that joins with a lower distance threshold leads to hardly
  #   any matches (e.g. distance join based on 50 meter leads to only 3 % of
  #   the amount of matches with a distance join based on 2000 meter in the
  #   surveys "so" versus "lf")

  assert_that(is.number(dist_threshold),
              dist_threshold >= 0,
              noNA(dist_threshold),
              msg = paste0("The object 'dist_threshold' should ",
                           "be a positive numeric."))


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

  summary_stat <- match.arg(summary_stat)

    # If "summary_stat" is "mean" or "median" and join_column is NULL
    if ((summary_stat %in% c("mean", "median")) &&
        (!is.null(join_column))) {

      # Assert that the column to join is numeric
      assertthat::assert_that(is.numeric(sf2[, join_column]),
        msg = paste0("The provided 'join_column' of '",
                     sf2_input,
                     "', i.e. '",
                     join_column,
                     "', should be numeric since 'summary_stat' is set to '",
                     summary_stat,
                     "' in the input arguments."))
    }


  # Search for distance matches for each of the plots (records) in sf1 ----

  # Set progress bar
  progress_bar <- txtProgressBar(min = 0, max = nrow(sf1), style = 3)


  # Create most_abundant function
  most_abundant <- function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }


  # Create generate_summary function
  generate_summary <- function(sf1, sf2, dist_threshold, i, summary_stat) {

    stopifnot(require("sf"))
    sf::st_is_within_distance(sf1[i, ],
                          sf2,
                          dist = dist_threshold,
                          sparse = FALSE) |>
      which() -> vec

    # use early returns
    if (identical(vec, integer(0))) {
      return(sf1)
    }

    # if join_column was not provided
    if (is.null(join_column)) {
      # Set the new column in sf1 to TRUE
      sf1$new_col[i] <- TRUE
      return(sf1)
    }

    # if join_column was provided
    # Extract the data and remove NAs
    vec_data <- sf2[vec, join_column]
    sf1$new_col[i] <- switch(
      summary_stat,
      mean = mean(vec_data, na.rm = TRUE),
      median = median(vec_data, na.rm = TRUE),
      "most abundant" = most_abundant(vec_data)
    )
    return(sf1)
  }


  # Apply the generate_summary function for all sf1 records
  for (i in seq_len(nrow(sf1))) {

    sf1 <- generate_summary(
      sf1 = sf1,
      sf2 = sf2,
      dist_threshold = dist_threshold,
      i = i,
      summary_stat = summary_stat
    )

    # Update progress bar
    setTxtProgressBar(progress_bar, i)
  }

  close(progress_bar)


  # Update the name of the new column in sf1
  names(sf1)[which(names(sf1) == "new_col")] <- new_col_name

  # Save sf dataframe to global environment
  # Check if the user wants to save to the global environment
  if (save_to_global) {
    # Prompt the user for confirmation
    confirmation <-
      readline(prompt = paste0("Do you want to save the modified data frames",
                               " to the global environment? (Y/N): "))

    # Check the user's response
    if (tolower(confirmation) == "y") {
      # Save the modified data frames to the global environment
      assign(sf1_input, sf1, envir = globalenv())
    }
  }


}
