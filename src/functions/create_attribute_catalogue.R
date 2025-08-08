
create_attribute_catalogue <- function(data_frame,
                                       path_to_save = NULL) {

  source("./src/functions/get_env.R")

  # Import
  dict <- readLines("./data/additional_data/attribute_catalogue_dictionary.txt")

  # Initialise objects
  parameters <- character(0)
  descriptions <- character(0)
  current_parameter <- NA
  current_description <- ""

  for (line in dict) {

    # Line starting with hyphen and space: new parameter
    if (grepl("^-\\s", line)) {

      if (!is.na(current_parameter)) {
        parameters <- c(parameters, current_parameter)
        descriptions <- c(descriptions, current_description)
      }
      # Name of parameter
      current_parameter <- trimws(gsub("^-\\s+", "", line))
      current_parameter <- gsub(":$", "", current_parameter)

      # Reset the description
      current_description <- ""

    } else {

      # Add the line to the current description
      current_description <- paste(current_description, line, sep = "\n")
    }
  }

  parameters <- c(parameters, current_parameter)
  descriptions <- c(descriptions, current_description)

  dict <- data.frame(parameter = parameters,
                     field_desc = descriptions,
                     stringsAsFactors = FALSE) %>%
    arrange(parameter) %>%
    distinct(parameter, .keep_all = TRUE)


  # Required parameters

  if (is.character(data_frame)) {

    required_par <- NULL

    for (i in seq_along(data_frame)) {

      required_par <- c(required_par,
                        names(get_env(data_frame[i])))
    }

    required_par <- unique(required_par)

    source("./src/functions/as_character_summary.R")

    title <- paste0("\nAttribute catalogue for ",
                    as_character_summary(data_frame,
                                         quot_marks = TRUE),
                    "\n\n")

  } else {

    assertthat::assert_that("data.frame" %in% class(data_frame))

    required_par <- names(data_frame)

    title <- paste0("\nAttribute catalogue\n\n")
  }



  missing_par <- required_par[which(!required_par %in% dict$parameter)]

  if (!identical(missing_par, character(0))) {

    cat(paste0("\nPlease add the following parameters (with descriptions) to ",
               "'./data/additional_data/attribute_catalogue_dictionary.txt'.",
               "\nSubsequently rerun this function.\n\n"))
    cat(paste0("-   ", sort(missing_par), ""), sep = "\n")


  } else {

  # If all parameters have a description in the dictionary

  attr_cat <- dict %>%
    filter(parameter %in% required_par) %>%
    # Arrange following the exact order of the columns in the dataframe
    arrange(match(parameter, required_par))
    # arrange(parameter)

  if (is.null(path_to_save)) {

    # Define the path to save the .txt file
    path_to_save <- "./output/attribute_catalogue.txt"

  } else {

    path_to_save <- paste0(path_to_save, "attribute_catalogue.txt")
  }

  # Open a connection to write the formatted content to the .txt file
  write_con <- file(path_to_save, "w")

  writeLines(paste0(title,
                    "------------------------------------",
                    "-----------------------------------\n"),
             write_con)

  # Iterate through each row of the dataframe
  for (i in 1:nrow(attr_cat)) {
    # Get the parameter and description from the dataframe
    parameter <- attr_cat$parameter[i]
    description <- attr_cat$field_desc[i]

    # Format the content and write it to the .txt file
    formatted_content <- paste0("-   ", parameter, ":",
                                description)
    writeLines(formatted_content, write_con)
  }

  # Close the connection
  close(write_con)

  # Print a message to indicate that the file has been saved
  cat("\nAttribute table saved to", path_to_save, "\n")

  }


}
