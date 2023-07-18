
#' Bind Inconsistency Lists
#'
#' Bind inconsistency lists from a given inconsistency type from the
#' environment into a single data frame, and removes the original inconsistency
#' lists from the environment.
#'
#' @param inconsistency_type Character string specifying the type of
#' inconsistency lists to bind.
#' @param save_to_env Logical indicating whether to save the combined data
#' frame to the environment. Default is \code{FALSE}.
#'
#' @details This function takes an inconsistency type and looks for objects
#' in the environment with names that match the specified inconsistency type.
#' It then combines these objects into a single data frame using \code{rbind}.
#' The resulting data frame can be returned or saved to the environment,
#' depending on the value of \code{save_to_env}.
#'
#' @importFrom assertthat assert_that
#'
#' @importFrom knitr knit_global
#' @importFrom base do.call
#' @importFrom base identical
#' @importFrom base rm
#' @importFrom base paste0
#' @importFrom base ls
#' @importFrom base character
#' @importFrom base logical
#'
#' @seealso \code{\link{get_env}}, \code{\link{assign_env}}
#'
#' @examples
#' # Bind the list of primary inconsistencies and save to environment
#' bind_inconsistency_list("list_primary_inconsistencies", save_to_env = TRUE)
#'
#' # Bind the list of coordinate inconsistencies and return the combined data
#' # frame
#' bind_inconsistency_lists("list_primary_inconsistencies", save_to_env = TRUE)
#'
#' @export

bind_inconsistency_lists <- function(inconsistency_type, save_to_env = FALSE) {

  source("./src/functions/assign_env.R")
  source("./src/functions/get_date_local.R")

  download_date <- get_date_local("./data/raw_data/")

  assertthat::assert_that(inconsistency_type %in%
                            c("list_primary_inconsistencies",
                              "list_coordinate_inconsistencies",
                              "list_layer_inconsistencies",
                              "list_range_inconsistencies",
                              "list_derived_inconsistencies"),
                          msg = paste0("Unknown inconsistency type '",
                                       inconsistency_type,
                                       "' in input argument."))

  if (isTRUE(getOption("knitr.in.progress"))) {
    envir <- knitr::knit_global()
  } else {
    envir <- globalenv()
  }

  matching_objects <- ls(pattern = paste0("^", inconsistency_type, "_"),
                         envir = envir)

  if (!identical(matching_objects, character(0))) {

    # Get the data from the matching objects using mget()
    data_list <- mget(matching_objects, envir = envir)
    
    # Combine the data using do.call() and rbind()
    combined_data <- do.call(rbind, data_list)
    
    rownames(combined_data) <- NULL
  }

  if (save_to_env == TRUE) {
    rm(list = matching_objects, envir = envir)
    assign_env(inconsistency_type, combined_data)
  }

  cat(paste0("'", inconsistency_type, "' contains ",
             length(which(
               combined_data$non_duplicated_error_type_per_record == TRUE)),
             " unique inconsistencies.\n"))

}