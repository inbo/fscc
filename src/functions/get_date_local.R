
#' Get change date or download date from local data
#'
#' Retrieve the date from the local subfolder metadata file,
#' i.e. the change date (if this is reported) or download date
#  from a metadata.txt file which should be present in each local data folder
#'
#' @param path The path to the local subfolder.
#' @param save_to_env Logical which indicates whether the output date
#' can be saved to the environment and override any existing objects
#' with the same name. Default is FALSE.
#' @param collapsed Logical which indicates whether the date has to be returned
#' in a collapsed form, e.g. "2023-09-14" becomes "20230914". Default is FALSE.
#' @return The selected date from the metadata file.
#' @examples
#' date <- get_date_local("./data/raw_data/")
#' @export

get_date_local <- function(path, save_to_env = FALSE, collapsed = FALSE) {
  
  local_subfolders <- list.files(path)
  
  # If this subfolder is currently empty:
  # return NA
  
  if (identical(local_subfolders, character(0))) {
    return(NA)
    
    # If this subfolder is not empty:
  } else {
    
    # Assert that there is a file called "metadata.txt" in this subfolder
    
    assertthat::assert_that(file.exists(paste0(path, "metadata.txt")),
                            msg = paste0("No metadata.txt file with date ",
                                         "has been found in the subfolder '",
                                         path,
                                         "'."))
    
    # Read dates in metadata.txt file
    local_dates <- readLines(paste0(path, "metadata.txt"))
    
    # Initialize variables
    change_date <- NULL
    download_date <- NULL
    
    # Iterate through the local_dates
    for (date in local_dates) {
      
      # Check if the date matches the "Change date: " format
      if (grepl("^Change date:", date)) {
        change_date <- date %>%
          sub("^Change date: ", "", .) %>%
          parsedate::parse_iso_8601() %>%
          as.Date()
        # If a date matches "Change date: " format, exit the loop
        break
      }
      # Check if the date matches the "Download date: " format
      else if (grepl("^Download date:", date)) {
        download_date <- date %>%
          sub("^Download date: ", "", .) %>%
          parsedate::parse_iso_8601() %>%
          as.Date()
      }
    }
    
    # Select the appropriate date based on the conditions
    if (!is.null(change_date)) {
      selected_date <- change_date
    } else {
      selected_date <- download_date
    }

    if (collapsed == TRUE) {
      selected_date <- gsub("-", "", selected_date)
    }

    return(selected_date)

    if (save_to_env == TRUE) {
      source("./src/functions/assign_env.R")
      assign_env("download_date", selected_date)
    }
    
  }
}
