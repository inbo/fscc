
#' Save solid soil data and output to Google Drive
#'
#' This function saves survey forms, other dataframes or ggplot objects to
#' a provided path in Google Drive.
#'
#' @param objects_to_save Character vector of objects to save, such as survey
#' forms (e.g. "so_som"), survey codes (e.g. "so"), other data frames
#' (e.g. for "./output/" folder), or ggplot objects. Default is NULL,
#' which saves all solid soil-related survey forms.
#' @param path_name Path where the data will be saved (should be in "./data/"
#' or "./output/" folder). Can be in the form of "0_01",
#' "0_01_intermediate_data", "./data/intermediate_data/0_01_intermediate_data".
#' @param change_date Character string representing the date on which the
#' data were changed, in "YYYYMMDD" format. If NULL, today's date is used
#' (default).
#' @param subfolder_non_survey_forms Character string with name of the subfolder
#' of the folder mentioned in "path_name" where dataframes which are not survey
#' forms should be stored in Google Drive. If NULL, a subfolder with indication
#' of the download_date and change_date is used. If FALSE, the objects are
#' stored in the folder mentioned in "path_name" (i.e. no subfolder is made).
#' @param ... Arguments to pass to ggplot2::ggsave() function. Can be "dpi",
#' "width", "height" or "export_name". If null, the arguments "width" and
#' "height" take the default values of ggsave, and the argument "dpi" becomes
#' 500. The argument "export_name" represents the names the ggplot figures
#' should get in Google Drive. This should be a character vector of the same
#' length as the number of the ggplot objects in "objects_to_save", and have
#' the same order as these ggplot objects.
#' @return None
#' @details
#'
#' @examples
#' Save all solid soil-related survey forms to the "0_01" breakpoint of the
#' intermediate data
#' save_to_google_drive(path_name = "0_01)
#'
#' Save specific survey forms, other dataframes and ggplot objects
#' to respectively a subfolder with date indication and a subfolder named
#' "example_folder" of the "./output/stocks/" folder on Google Drive. The
#' plot will be renamed.
#' save_to_google_drive(objects_to_save = c("so_som_plots_with_c", "so_som",
#'                                          "so_prf", "sw", "plot_c"),
#'                      path_name = "stocks",
#'                      subfolder_non_survey_forms = "example_folder",
#'                      export_name = "plot_c_example")

save_to_google_drive <- function(objects_to_save = NULL,
                                 path_name,
                                 change_date = NULL,
                                 subfolder_non_survey_forms = NULL,
                                 ...) {

# KEEP THE FOLLOWING CODE OF THIS R SCRIPT UP TO DATE: ----
  # - theoretical recursive subfolders of "output" and "data"
  #   and their relative paths
  #   Lines: 90
  # - Google Drive: recursive subfolders of "output" and "data"
  #   Lines: 165 and 173

# Define required packages ----

  stopifnot(require("sf"),
            require("tidyverse"),
            require("openxlsx"),
            require("parsedate"),
            require("googlesheets4"),
            require("googledrive"),
            require("assertthat"))

  # Derive the ID from the Google Drive root URL ----

  # Assert that an R script with the confidential Google Drive link was created
  assertthat::assert_that(
    file.exists("./data/sensitive_metadata/google_drive_link.R"),
    msg = str_wrap(
      paste0("The file './data/sensitive_metadata/google_drive_link.R' ",
             "with the confidential link of the root folder with data on ",
             "Google Drive does not exist. Kindly create this in accordance ",
             "with the 'Getting started: first time' section of README.md")))

  # Derive the id of the root drive with data on Google Drive
  source("./data/sensitive_metadata/google_drive_link.R")

  # Assert that the object "root_drive" has been created and is a character
  assertthat::assert_that(is.character(root_drive),
                          msg =
                            str_wrap(paste0("In the R script named ",
                          "'./data/sensitive_metadata/google_drive_link.R, '",
                          "no character object 'root_drive' with ",
                          "the private Google Drive link is created.")))

  # Save the root_drive object to the global environment
  assign("root_drive", as_id(root_drive), envir = globalenv())


# Translate the path name into the id of the target drive ----

# List the theoretical paths

  if (path_name %in% c("intermediate_data",
                       "./data/intermediate_data",
                       "./data/intermediate_data/")) {
  path_name <-
    readline(prompt =
               paste0("Kindly specify the breakpoint of the intermediate data",
                      " (e.g. 0_01, 0_02, 1_01):"))
  }

  paths <-   # TO KEEP UP-TO-DATE!
    data.frame(subfolder = c("data",
                             "raw_data",
                             "intermediate_data",
                             "0_01_intermediate_data",
                             "0_02_intermediate_data",
                             "layer1_data",
                             "layer2_data",
                             "additional_data",
                             "output",
                             "stocks",
                             "indices",
                             "physical_data",
                             "links_other_surveys",
                             "links_n_deposition",
                             "pirs",
                             "pathfinder"),
               subfolder_alternative = c("data",
                                         "raw",
                                         "intermediate",
                                         "0_01",
                                         "0_02",
                                         "layer1",
                                         "layer2",
                                         "additional",
                                         "output",
                                         "stocks",
                                         "indices",
                                         "physical_data",
                                         "links_other_surveys",
                                         "links_n_deposition",
                                         "pirs",
                                         "pathfinder"),
               path = c("./data/",
                        "./data/raw_data/",
                        "./data/intermediate_data/",
                        "./data/intermediate_data/0_01_intermediate_data/",
                        "./data/intermediate_data/0_02_intermediate_data/",
                        "./data/layer1_data/",
                        "./data/layer2_data/",
                        "./data/additional_data/",
                        "./output/",
                        "./output/stocks/",
                        "./output/indices/",
                        "./output/physical_data/",
                        "./output/links_other_surveys/",
                        "./output/links_n_deposition/",
                        "./output/pirs/",
                        "./output/pathfinder/"),
               path_alternative = c(".data",
                             "./data/raw_data",
                             "./data/intermediate_data",
                             "./data/intermediate_data/0_01_intermediate_data",
                             "./data/intermediate_data/0_02_intermediate_data",
                             "./data/layer1_data",
                             "./data/layer2_data",
                             "./data/additional_data",
                             "./output",
                             "./output/stocks",
                             "./output/indices",
                             "./output/physical_data",
                             "./output/links_other_surveys",
                             "./output/links_n_deposition",
                             "./output/pirs",
                             "./output/pathfinder"))

# List the existing Google Drive paths

  gd_subfolders <- drive_ls(root_drive_id, type = "folder")

  gd_data <-
    drive_ls(gd_subfolders$id[which(gd_subfolders$name == "data")],
             type = "folder")
  gd_data_recursive <-
    rbind(gd_data,
          drive_ls(gd_data$id[
            which(gd_data$name == "intermediate_data")]))
  # TO KEEP UP-TO-DATE!

  gd_output <-
    drive_ls(gd_subfolders$id[which(gd_subfolders$name == "output")])
  gd_output_recursive <- gd_output
  # TO KEEP UP-TO-DATE!

  # Combine "gd_data_recursive" and "gd_output_recursive"
  gd_recursive <- rbind(as.data.frame(gd_subfolders[, 1:2]),
                        as.data.frame(gd_data_recursive[, 1:2]),
                        as.data.frame(gd_output_recursive[, 1:2]))

# Combine Google Drive folders with theoretical paths

  paths <- left_join(paths, gd_recursive,
                     by = join_by(subfolder == name))

# Find the appropriate id, or create the appropriate Google Drive folder

  # Find the index of the row where path_name appears
  row_index <- which(apply(paths[, 1:4], 1, function(x) any(x == path_name)))

  # Assert that path_name exists in theory
  assertthat::assert_that(!identical(row_index, integer(0)),
                          msg = "The given path name is not known.")

  # Create target id

  id_target <- paths$id[row_index]

  # Create function to remove last part of path string

  remove_last_part_path <- function(path) {

    path <- gsub("/$", "", path)
    path <- gsub("/[^/]*$", "", path)
    return(path)
  }

  # Create function to create Google Drive folder and retrieve its id

  create_google_folder <- function(folder_name, id) {

    drive_mkdir(name = folder_name,
                path = id)

    gd_subfolders_new <- drive_ls(id,
                                  type = "folder")

    id <- gd_subfolders_new$id[which(gd_subfolders_new$name == folder_name)]

    return(id)
  }

  # Create a folder and retrieve its id if needed

  if (is.na(paths$id[row_index])) {

    path_shorter <- remove_last_part_path(paths$path[row_index])

    row_index_shorter <-
      which(apply(paths[, 1:4], 1, function(x) any(x == path_shorter)))

    id_target <- create_google_folder(folder_name = paths$subfolder[row_index],
                                      id = paths$id[row_index_shorter])

    paths$id[row_index] <- id_target

  }



  # Define vectors with survey forms and codes ----

  survey_forms_all <- c("so_som", "so_prf", "so_pls", "so_pfh", "so_lqa",
                        "s1_som", "s1_prf", "s1_pls", "s1_pfh", "s1_lqa",
                        "si_eve", "si_plt", "si_sta", "si_tco", "y1_pl1",
                        "y1_st1", "sw_swa", "sw_swc")

  list_data_tables <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                           s1 = c("som", "prf", "pls", "pfh", "lqa"),
                           si = c("eve", "plt", "sta", "tco"),
                           y1 = c("pl1", "st1"),
                           sw = c("swa", "swc"))


# Define and prepare survey forms + codes + other objects to save ----

  # If no "objects_to_save" argument is provided,
  # save all solid soil-related survey forms in the global environment
  # (by default)

  if (is.null(objects_to_save)) {

    # All survey forms which are in the global environment
    
    survey_forms <- survey_forms_all[
      which(survey_forms_all %in%
              ls(envir = parent.frame())[
                sapply(ls(envir = parent.frame()),
                       function(x) class(get(x, envir = parent.frame()))) == "data.frame"
              ])
    ]
    }

  # If "objects_to_save" argument is provided:

  if (!is.null(objects_to_save)) {

    # If it concerns survey forms:
    # replace any survey codes (e.g. "so")
    # by the actual survey forms (e.g. "so_som", "so_prf"...)

    survey_forms_extra <- NULL
    survey_forms <- objects_to_save

    # Check for each of the reported characters whether they represent a code
    # of a survey (length = 1) or a survey form (length = 2)

    for (i in seq_along(objects_to_save)) {

      if ((length(unlist(strsplit(objects_to_save[i], "_"))) == 1) &&
          (objects_to_save[i] %in% names(list_data_tables))) {


        # Replace the code by the actual survey forms
        survey_forms_extra <- c(survey_forms_extra,
                                paste0(objects_to_save[i], "_",
                                list_data_tables[[which(
                                  names(list_data_tables) ==
                                    objects_to_save[i])]]))

        survey_forms[i] <- NA
      }
      }

    if (!identical(survey_forms_extra, NULL)) {

      survey_forms <- c(survey_forms, survey_forms_extra)
      survey_forms <- survey_forms[which(!is.na(survey_forms))]
      }
    }


  # Assert that the survey forms exist +
  # Create a vector with the codes of the different surveys + a vector
  # with objects which are not survey forms to save

  survey_codes <- NULL
  objects <- NULL

  for (i in seq_along(survey_forms)) {

    assertthat::assert_that(exists(survey_forms[i], envir = .GlobalEnv))

    if (survey_forms[i] %in% survey_forms_all) {
    survey_codes <- c(survey_codes,
                      unlist(strsplit(survey_forms[i], "_"))[1])
    } else {
      objects <- c(objects,
                   survey_forms[i])
    }
    }

  survey_codes <- unique(survey_codes)


# Get download_date and change_date ----

  # Create function to retrieve download date

  get_download_date <- function(survey_forms,
                                collapsed_mode = TRUE) {

    download_date_list <- NULL
    for (i in seq_along(survey_forms)) {
      download_date_list <-
        c(download_date_list,
          suppressWarnings(get(survey_forms[i],
                               envir = .GlobalEnv)$download_date))
    }

    if (is.null(download_date_list)) {
      download_date_retrieved <-
        readline(prompt = paste0("What is the download date ",
                                 "of the data? (YYYYMMDD): "))
    } else {
    download_date_retrieved <-
      names(table(download_date_list))[which.max(table(download_date_list))]
    }

    if (collapsed_mode == TRUE) {
      download_date_retrieved <- gsub("-", "", download_date_retrieved)
    }
    return(download_date_retrieved)
  }

  # Get download date
  download_date_collapsed <- get_download_date(survey_forms = survey_forms)

  # Get change date

  if (!is.null(change_date)) {
    change_date <- as.character(as.Date(parsedate::parse_iso_8601(change_date)))
    assertthat::assert_that((!is.na(change_date)),
                            msg = paste0("The 'change_date' argument should ",
                                         "be in YYYYMMDD format."))
    change_date_collapsed <- gsub("-", "", change_date)
  }
  if (is.null(change_date)) {
    change_date <- as.character(Sys.Date())
    change_date_collapsed <- gsub("-", "", as.character(Sys.Date()))
  }

# Create a subfolder with indication of the dates inside id_target folder ----

  # Determine name of new subfolder for version label (dates)

  folder_name <- paste0(download_date_collapsed, "_",
                        change_date_collapsed, "_",
                        paths$subfolder[row_index])


  # Create a subfolder if there are any survey forms or if the input argument
  # 'subfolder_non_survey_forms' is NULL

  if (!is.null(survey_codes) ||
      (!is.null(objects) &&
       is.null(subfolder_non_survey_forms))) {

  # Check if this folder name already exists and ask permission to overwrite

  if (folder_name %in% (drive_ls(id_target, type = "folder")$name)) {

    delete_permission <- FALSE

    cat(paste0("Google Drive folder '",
               folder_name,
               "' already exists.\n"))

    confirmation <-
      readline(prompt = paste0("Do you grant ",
                               "permission to delete the existing folder ",
                               "on Google Drive? (Y/N): "))

    # Check the user's response
    if (tolower(confirmation) == "y") {
      delete_permission <- TRUE
    }

    # Assert that you have the permission
    assertthat::assert_that(delete_permission == TRUE,
                            msg = paste0("No permission to overwrite the ",
                                         "existing Google Drive folder with ",
                                         "same name (",
                                         folder_name,
                                         ")."))

    gd_subfolders <- drive_ls(id_target, type = "folder")
    id_to_delete <- gd_subfolders$id[which(gd_subfolders$name == folder_name)]
    drive_rm(as_id(id_to_delete))
  }


  # Create a new subfolder inside the target_id

  id_version <- create_google_folder(folder_name = folder_name,
                                     id = id_target)
  }



  # If there are other objects (not survey forms):
  # Check input argument 'subfolder_non_survey_forms' to see whether any
  # subfolder needs to be created

  if (!is.null(objects)) {

    # If a name was given to subfolder_non_survey_forms:
     if (is.character(subfolder_non_survey_forms)) {

    # If a subfolder with that name already exists: get its id
    if (subfolder_non_survey_forms %in%
         (drive_ls(id_target, type = "folder")$name)) {

      gd_subfolders <- drive_ls(id_target, type = "folder")

      id_object <-
        gd_subfolders$id[which(gd_subfolders$name ==
                                 subfolder_non_survey_forms)]

    # If a subfolder with that name does not exist: create it
    } else {
      id_object <- create_google_folder(subfolder_non_survey_forms,
                                        id = id_target)
    }

    # If subfolder_non_survey_forms is FALSE: no subfolder needed
    } else if (isFALSE(subfolder_non_survey_forms)) {
      id_object <- id_target

    # If subfolder_non_survey_forms is NULL: use the subfolder with dates
    } else if (is.null(subfolder_non_survey_forms)) {
      id_object <- id_version
    }
  }



  # Create function to upload csv files to Google Drive ----

  drive_upload_csv <- function(dataframe,
                               id,
                               file_name_drive) {

    # Save the data frame as a temporary CSV file

    temp_file <- tempfile(fileext = ".csv")
    write.csv2(dataframe,
               file = temp_file,
               row.names = FALSE,
               na = "")

    # If file_name_drive does not end with .csv, add ".csv"

    if (substr(file_name_drive,
               nchar(file_name_drive) - 3,
               nchar(file_name_drive)) != ".csv") {
      file_name_drive <- paste0(file_name_drive, ".csv")
    }

    # Upload the temporary CSV file to Google Drive
    drive_upload(temp_file,
                 path = id,
                 name = file_name_drive,
                 type = "text/csv")

    # Remove the temporary file
    file.remove(temp_file)

  }

  # Create function to upload png files to Google Drive ----

  drive_upload_png <- function(plot,
                               id,
                               file_name_drive,
                               dpi = 500,
                               width = NULL,
                               height = NULL) {

    # Save the plot as a temporary PNG file
    temp_file <- tempfile(fileext = ".png")

    if (is.null(width) && is.null(height)) {
      ggsave(temp_file,
             plot,
             dpi = dpi)
    } else {
      ggsave(temp_file,
             plot,
             dpi = dpi,
             width = width,
             height = height)
    }

    # If file_name_drive does not end with .png, add ".png"
    if (substr(file_name_drive,
               nchar(file_name_drive) - 3,
               nchar(file_name_drive)) != ".png") {
      file_name_drive <- paste0(file_name_drive, ".png")
    }

    # Upload the temporary PNG file to Google Drive
    drive_upload(temp_file,
                 path = id,
                 name = file_name_drive,
                 type = "image/png")

    # Remove the temporary file
    file.remove(temp_file)
  }




  # Save any normal survey forms ----

  # If there are any normal survey forms

  if (!is.null(survey_codes)) {

    # For each of the survey codes:

    for (i in seq_along(survey_codes)) {

      # List the survey forms which should be saved in that folder

      survey_forms_i <-
        survey_forms[which((survey_forms %in% survey_forms_all) &
                             (sapply(strsplit(survey_forms, "_"), "[", 1) ==
                                survey_codes[i]))]

      # Create a subfolder for the given survey code

      id_i <- create_google_folder(folder_name = survey_codes[i],
                                   id = id_version)

      # Save each of the survey forms in subfolder id_i

      for (j in seq_along(survey_forms_i)) {

        # Retrieve df from global environment
        df <- get(survey_forms_i[j], envir = .GlobalEnv)

        # Add a new change_date_google_drive
        df$change_date_google_drive <- change_date

        # Save the data frame as a .csv file in Google Drive
        drive_upload_csv(dataframe = df,
                         id = id_i,
                         file_name_drive = paste0(survey_forms_i[j], ".csv"))
      }

      # Save coordinate data form if this exists

      coordinate_df_name <- paste0("coordinates_", survey_codes[i])

      if (exists(coordinate_df_name) &&
          is.data.frame(get(coordinate_df_name))) {

        coordinate_df <- get(coordinate_df_name, envir = .GlobalEnv)
        drive_upload_csv(dataframe = coordinate_df,
                         id = id_i,
                         file_name_drive = paste0(coordinate_df_name, ".csv"))
      }

      # Save data availability form if this exists

      data_avail_name <- paste0("data_availability_", survey_codes[i])

      if (exists(data_avail_name) &&
          is.data.frame(get(data_avail_name))) {

        data_avail_df <- get(data_avail_name, envir = .GlobalEnv)
        drive_upload_csv(dataframe = data_avail_df,
                         id = id_i,
                         file_name_drive = paste0(data_avail_name, ".csv"))
      }
    }
  }

  # Save any other files ----

  # If there are any normal survey forms

  if (!is.null(objects)) {

    # For each of the objects:

    for (i in seq_along(objects)) {

      assertthat::assert_that(inherits(get(objects[i]), "ggplot") ||
                                inherits(get(objects[i]), "data.frame"),
                              msg = paste0("The function is only able to ",
                                           "save dataframes and ggplot ",
                                           "objects to Google Drive ",
                                           "in its current version."))

      # If the object is a data frame

      if (inherits(get(objects[i]), "data.frame")) {

        # Retrieve df from global environment
        df <- get(objects[i], envir = .GlobalEnv)

        # Save the data frame as a .csv file in Google Drive
        drive_upload_csv(dataframe = df,
                         id = id_object,
                         file_name_drive = paste0(objects[i], ".csv"))

      }

      # If the object is a ggplot

      if (inherits(get(objects[i]), "ggplot")) {

        # Retrieve plot from global environment
        plot <- get(objects[i], envir = .GlobalEnv)

        # Retrieve additional input arguments from save_to_google_drive function
        other_args <- list(...)

        # Extract specific arguments if needed
        dpi <- other_args$dpi
        # width <-
        #  if (is.null(other_args$width)) NULL else as.numeric(other_args$width)
        # height <-
        #  if (is.null(other_args$height)) NULL else as.numeric(other_args$height)
        width <- other_args$width
        height <- other_args$height
          # only for ggplot: should have the same sequence as the ggplot
          # objects in "objects_to_save"
        export_name <- other_args$export_name

        if (!is.null(export_name)) {

          # List the ggplot objects
          objects_ggplot <-
            objects[which((sapply(objects,
                                  function(obj) inherits(get(obj,
                                                            envir = .GlobalEnv),
                                                         "ggplot"))))]
          assertthat::assert_that(length(export_name) == length(objects_ggplot),
                                  msg = paste0("The 'export_name' vector ",
                                               "should ",
                                               "have the same length as the ",
                                               "number of ggplot objects ",
                                               "in 'objects_to_save' so ",
                                               "that the names can be matched ",
                                               "to the ggplot objects."))
          export_name <- export_name[match(objects[i], objects_ggplot)]
        }

        # Save the plot as a .png file in Google Drive

        file_name <- ifelse(is.null(export_name),
                            objects[i],
                            export_name)

        dpi <- ifelse(is.null(dpi),
                      500,
                      dpi)

        drive_upload_png(plot = plot,
                         id = id_object,
                         file_name_drive = paste0(file_name, ".png"),
                         dpi = dpi,
                         width = width,
                         height = height)

      }

    }
  }


}
