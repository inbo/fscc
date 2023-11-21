
#' Bind Inconsistency Lists
#'
#' Bind inconsistency lists from a given inconsistency type from the
#' environment into a single data frame, and removes the original inconsistency
#' lists from the environment.
#'
#' @param object_name_start Character string specifying the type of
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
#' bind_inconsistency_lists(object_name_start = "pir_applied",
#'                          object_type = "pir_applied",
#'                          save_to_env = TRUE)
#'
#' @export

bind_objects_starting_with <- function(object_name_start,
                                       object_type = "new_inconsistencies",
                                       save_to_env = FALSE) {

  source("./src/functions/assign_env.R")
  source("./src/functions/get_date_local.R")

  if (object_type == "new_inconsistencies") {
    
    download_date <- get_date_local("./data/raw_data/")
    
  assertthat::assert_that(object_name_start %in%
                            c("list_primary_inconsistencies",
                              "list_coordinate_inconsistencies",
                              "list_layer_inconsistencies",
                              "list_range_inconsistencies",
                              "list_derived_inconsistencies"),
                          msg = paste0("Unknown inconsistency type '",
                                       object_name_start,
                                       "' in input argument."))
  }

  if (isTRUE(getOption("knitr.in.progress"))) {
    envir <- knitr::knit_global()
  } else {
    envir <- globalenv()
  }

  list_surveys <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                       s1 = c("som", "prf", "pls", "pfh", "lqa"),
                       si = c("eve", "plt", "sta", "tco"),
                       y1 = c("pl1", "st1", "ev1"),
                       sw = c("swa", "swc"))

  list_survey_forms <- unlist(
    lapply(names(list_surveys), function(name) {
      paste0(name, "_", list_surveys[[name]])
    })
  )
  
  matching_objects <- ls(pattern = paste0("^", object_name_start, "_"),
                         envir = envir)
  
  if (!identical(matching_objects, character(0))) {
    
  matching_objects <-
    matching_objects[which((gsub(paste0(object_name_start, "_"),
                                "",
                                matching_objects) %in% list_survey_forms) |
                           (gsub(paste0(object_name_start, "_"),
                                 "",
                                 matching_objects) %in% names(list_surveys)))]
  }

  if (!identical(matching_objects, character(0))) {

    # Get the data from the matching objects using mget()
    data_list <- mget(matching_objects, envir = envir)
    
    # Combine the data using do.call() and rbind()
    combined_data <- do.call(rbind, data_list)
    
    rownames(combined_data) <- NULL
  }

  if (save_to_env == TRUE) {
    rm(list = matching_objects, envir = envir)
    assign_env(object_name_start, combined_data)
  }

  if (object_type == "new_inconsistencies") {
    
  cat(paste0("'", object_name_start, "' contains ",
             length(which(
               combined_data$non_duplicated_error_type_per_record == TRUE)),
             " unique inconsistencies.\n"))
  }

}