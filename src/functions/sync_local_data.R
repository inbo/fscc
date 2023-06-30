
#' Synchronise local data with Google Drive
#'
#' This function synchronises the local data folder with a Google Drive folder,
#' downloading missing files from Google Drive to the local folder and deleting
#' local files that are not present in Google Drive.
#'
#' @param list_subfolders_data Either a character vector with subfolders of
#' "data" to synchronise, NULL (synchronises all subfolders of
#' "data" by default), or FALSE (no subfolders of "data" are synchronised)
#' @param list_subfolders_output Either a character vector with subfolders of
#' "output" to synchronise, NULL (synchronises all subfolders of
#' "output" by default), or FALSE (no subfolders of "output" are synchronised)
#'
#' @details
#' To get this URL of the Google Drive root folder:
#' - Go to the root folder on Google Drive (with subfolders "data" and "output")
#' - Right-click on the root folder and select "Copy link".
#' - Paste the link somewhere. It should look like
#'   "https://drive.google.com/drive/folders/1Txxxxxxxe7?usp=drive_link"
#'   Copy the part you need, i.e. everything starting from two characters
#'   before the first x until two characters after the last x (i.e. before any
#'   question mark). The two characters before and after the x string are a
#'   random example.
#' - Create a new R script in the folder "./data/sensitive_metadata/" and give
#'   it the following name: "google_drive_link.R".
#' - Paste the code below in this script:
#'   ---
#'   # This is the URL of the Google Drive root folder which contains data
#'   # related to the fscc project of ICP Forests. The data in this folder
#'   # are sensitive and are only available for collaborators with access
#'   # to the Google Drive folder.
#'
#'   root_drive <- "1Txxxxxxxe7"
#'   ---
#' - Replace the character string after "<-" with the actual code from the link
#'   as explained above.
#'
#' For authentication, you will need to grant access to the documents on your
#' Google Drive account for the "Tidyverse API Packages".
#'
#' Data on Google Drive should always be stored in subfolders of a certain
#' folder (e.g. subfolder of "0_01_intermediate_data") which are named with
#' an indication of their version/modification status in the following format:
#' - raw_data: "download_(download_date)", e.g. "download_20230605"
#' - other subfolders: "(download_date)_(change_date)_(name_folder)", e.g.
#'   "20230605_20230628_0_01_intermediate_data"
#'
#' Raw data can be uploaded on Google Drive inside such subfolder as zipped
#' or unzipped folders.
#'
#' If a local folder is absent, any data (from the most recent change date)
#' will automatically be downloaded. If a local folder contains data which are
#' older than the most recent data on Google Drive, the function will ask your
#' permission to remove the existing local data in order to download the new
#' files from Google Drive. Local folders always contain only one version of
#' certain data or files and are named in a clean way (no indication of
#' the version status). However, a metadata.txt file with the download date and
#' change date is created/updated alongside during the synchronisation process.
#'
#' @import googledrive
#'
#' @examples
#' local_folder <- "./data/"
#' drive_folder_id <- "your_folder_id"
#' sync_data(local_folder, drive_folder_id)
#'
#' @export

sync_local_data <- function(list_subfolders_data = NULL,
                            list_subfolders_output = NULL) {

# KEEP THE FOLLOWING CODE OF THIS R SCRIPT UP TO DATE: ----
# - Google Drive: recursive subfolders of "output" and "data"
#   Lines: 103 and 109
# - local data folder: theoretical recursive subfolders of "output" and "data"
#   and their relative paths
#   Lines: 128 and 206

# Define required packages ----

  stopifnot(require("sf"),
            require("tidyverse"),
            require("openxlsx"),
            require("parsedate"),
            require("googlesheets4"),
            require("googledrive"),
            require("assertthat"))



# Derive the ID from the Google Drive URL ----

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
                          msg = str_wrap(paste0("In the R script named ",
                           "'./data/sensitive_metadata/google_drive_link.R, '",
                           "no character object 'root_drive' with ",
                           "the private Google Drive link is created.")))

  # Save the root_drive object to the global environment
  assign("root_drive", root_drive, envir = globalenv())


# List subfolders of Google Drive under the "data" and "output" subfolders ----

  root_drive_id <- as_id(root_drive)
  gd_subfolders <- drive_ls(root_drive_id)

  gd_data <-
    drive_ls(gd_subfolders$id[which(gd_subfolders$name == "data")])
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
  gd_recursive <- rbind(as.data.frame(gd_data_recursive[, 1:2]),
                        as.data.frame(gd_output_recursive[, 1:2]))


# Define "data" subfolders to synchronise: list_subfolders_data ----

  # The Google Drive root folder contains all the possible versions of a
  # certain data subfolder, e.g. all possible versions of "raw_data", or of
  # intermediate data at breakpoint "0_01". This function always looks for the
  # most recent version within a certain intermediate_data breakpoint, or layer.

  # Create a list with the possible data subfolders and their paths

  data_paths <-   # TO KEEP UP-TO-DATE!
    data.frame(subfolder = c("raw_data",
                             "intermediate_data",
                             "0_01_intermediate_data",
                             "0_02_intermediate_data",
                             "layer1_data",
                             "layer2_data"),
               # Let's ignore "additional_data" for now
               subfolder_recursive_key = c("raw_data",
                                           NA,
                                           "intermediate_data",
                                           "intermediate_data",
                                           "layer1_data",
                                           "layer2_data"),
               path = c("./data/raw_data/",
                        "./data/intermediate_data/",
                        "./data/intermediate_data/0_01_intermediate_data/",
                        "./data/intermediate_data/0_02_intermediate_data/",
                        "./data/layer1_data/",
                        "./data/layer1_data/"))


  # If no "list_subfolders_data" argument is provided,
  # synchronise all subfolders present in Google Drive (by default)

  list_subfolders_data_orig <- list_subfolders_data

  if (is.null(list_subfolders_data_orig)) {

    list_subfolders_data <-
      gd_data$name[gd_data$name %in% c(data_paths$subfolder,
                                       "additional_data")]

  # If a "list_subfolders_data" argument is provided,
  # (i.e. if "list_subfolders_data" is not FALSE)
  # assert that these folders exist

  } else if (!isFALSE(list_subfolders_data_orig)) {

    # Create a vector which indicates which of the input "list_subfolders_data"
    # does not exist on Google Drive

    input_not_exists <-
      list_subfolders_data[!list_subfolders_data %in% gd_data_recursive$name]

    # Assert that the subfolders in the input argument exist on Google Drive
    source("./src/functions/as_character_summary.R")

    assertthat::assert_that(identical(input_not_exists, character(0)),
                            msg = paste0("The subfolder(s) named ",
                                         as_character_summary(
                                           input_subfolder_absent),
                                         " do(es) not exist"))

  }

  # Update data_paths so that this dataframe only contains subfolders
  # which should be updated.
  # Exclude subfolders for which only their recursive subfolders have to be
  # checked, e.g. exclude "intermediate_data" but include its subfolders
  # "0_01_intermediate_data" and "0_02_intermediate_data"

  if (!isFALSE(list_subfolders_data_orig)) {

    data_paths <-
      left_join(as.data.frame(list_subfolders_data),
                data_paths,
                by = join_by(list_subfolders_data == subfolder_recursive_key),
                multiple = "all") %>%
      rename(folder = list_subfolders_data) %>%
      inner_join(gd_recursive,
                 by = join_by(subfolder == name))
  }



# Define "output" subfolders to synchronise: list_subfolders_output ----

  # Create a list with the possible output subfolders and their paths

  output_paths <-   # TO KEEP UP-TO-DATE!
    data.frame(subfolder = c("stocks",
                             "indices",
                             "physical_data",
                             "links_other_surveys",
                             "links_n_deposition",
                             "pirs",
                             "pathfinder"),
               subfolder_recursive_key = c("stocks",
                                           "indices",
                                           "physical_data",
                                           "links_other_surveys",
                                           "links_n_deposition",
                                           "pirs",
                                           "pathfinder"),
               path = c("./output/stocks/",
                        "./output/indices/",
                        "./output/physical_data/",
                        "./output/links_other_surveys/",
                        "./output/links_n_deposition/",
                        "./output/pirs/",
                        "./output/pathfinder/"))


  # If no "list_subfolders_output" argument is provided,
  # synchronise all subfolders present in Google Drive (by default)

  list_subfolders_output_orig <- list_subfolders_output
  
  if (is.null(list_subfolders_output_orig)) {

    list_subfolders_output <-
      gd_output$name[gd_output$name %in% output_paths$subfolder]

    # If a "list_subfolders_output" argument is provided,
    # (i.e. if "list_subfolders_output" is not FALSE)
    # assert that these folders exist

  } else if (!isFALSE(list_subfolders_output_orig)) {

    # Create a vector which indicates which of the input
    # "list_subfolders_output" does not exist on Google Drive

    input_not_exists <-
      list_subfolders_output[!list_subfolders_output %in%
                               gd_output_recursive$name]

    # Assert that the subfolders in the input argument exist on Google Drive
    source("./src/functions/as_character_summary.R")

    assertthat::assert_that(identical(input_not_exists, character(0)),
                            msg = paste0("The subfolder(s) named ",
                                         as_character_summary(
                                           input_subfolder_absent),
                                         " do(es) not exist"))

  }

  # Update output_paths so that this dataframe only contains subfolders
  # which should be updated
  # Exclude subfolders for which only their recursive subfolders have to be
  # checked, e.g. exclude "intermediate_data" but include its subfolders
  # "0_01_intermediate_data" and "0_02_intermediate_data"

  if (!isFALSE(list_subfolders_output_orig)) {

    output_paths <-
      left_join(as.data.frame(list_subfolders_output),
                output_paths,
                by = join_by(list_subfolders_output == subfolder_recursive_key),
                multiple = "all") %>%
      rename(folder = list_subfolders_output) %>%
      inner_join(gd_recursive,
                 by = join_by(subfolder == name))
  }


# Combine "data_paths" and "output_paths" in one df: subfolder_paths ----

  if (exists("data_paths", inherits = FALSE) &&
      exists("output_paths", inherits = FALSE)) {
    subfolder_paths <- rbind(data_paths,
                             output_paths)
  } else
  if (exists("data_paths", inherits = FALSE) &&
      !exists("output_paths", inherits = FALSE)) {
    subfolder_paths <- data_paths
  } else
  if (exists("output_paths", inherits = FALSE) &&
      !exists("data_paths", inherits = FALSE)) {
    subfolder_paths <- output_paths
  }







# Synchronise local "data" and "output" subfolders ----


  # Create function which checks whether a folder contains data ----

  subfolders_unknown_file_count <-
    subfolder_paths$folder[!subfolder_paths$folder %in% c("intermediate_data",
                                                          "layer1_data",
                                                          "layer2_data")]

  contains_data_gd <- function(id, folder_name) {

    id_subfolders_data <- drive_ls(id, recursive = TRUE)

    # If a folder id (with a certain version) contains the complete dataset,
    # It means that it should recursively contain 31 files:

    # "[date]_so"
    #   - "so_som.csv"
    #   - "so_pfh.csv"
    #   - "so_prf.csv"
    #   - "so_pls.csv"
    #   - "so_lqa.csv"
    #   - "coordinates_so.csv"
    #   - "data_availability_so.csv"
    # "20230605_s1"
    #   - "s1_som.csv"
    #   - "s1_pfh.csv"
    #   - "s1_prf.csv"
    #   - "s1_pls.csv"
    #   - "s1_lqa.csv"
    #   - "coordinates_s1.csv"
    #   - "data_availability_s1.csv"
    # "20230605_si"
    #   - "si_sta.csv"
    #   - "si_plt.csv"
    #   - "si_eve.csv"
    #   - "si_tco.csv"
    #   - "coordinates_si.csv"
    # "20230605_y1"
    #   - "y1_st1.csv"
    #   - "y1_pl1.csv"
    #   - "coordinates_y1.csv"
    # "20230605_sw"
    #   - "sw_swc.csv"
    #   - "sw_swa.csv"
    #   - "coordinates_sw.csv"
    #   - "data_availability_sw.csv"

    if (((length(id_subfolders_data$name) >= 31) &&
        (folder_name %in% c("intermediate_data",
                            "layer1_data",
                            "layer2_data"))) ||
        ((length(id_subfolders_data$name) >= 1) &&
         (folder_name %in% subfolders_unknown_file_count))) {

    return(TRUE)
    } else {
      return(FALSE)
    }
  }


  # Create function which gets the most recent change date in Google Drive ----
  # of the different data folder versions

  get_recent_change_date_gd <- function(id, folder_name) {

    require("parsedate")

    # List subfolders of given id

    id_subfolders <- drive_ls(id)

    # If nothing is inside:
    # Return c(NA, NA)

    if (nrow(id_subfolders) == 0) {
      return(c(NA, NA))
    } else {

    # Exclude rows which do not contain data

    id_subfolders$contains_data <- NA

    for (i in seq_along(id_subfolders$name)) {

      id_subfolders$contains_data[i] <-
        contains_data_gd(id_subfolders$id[i], folder_name)
    }

    id_subfolders <- filter(id_subfolders,
                            .data$contains_data == TRUE)

    # If no rows are left anymore:
    # Return c(NA, NA)

    if (nrow(id_subfolders) == 0) {
      return(c(NA, NA))
    } else {

    # Select the change date and id of the data folder on Google Drive
    # which was changed/updated the most recently

    # For intermediate_data, layer1_data and layer2_data
    # And the "output" subfolders

    if (!folder_name %in% c("raw_data")) {

    # In the first column (subfolders of id), the first date refers to the
    # date on which the raw data were downloaded from the PCC database,
    # while the second date refers to "change date" of the data (i.e. the
    # date on which the data inside the folder were saved on Google Drive)

    # Extract the change date from the name column and place it in a new column
    # "change_date"

    id_subfolders <- id_subfolders %>%
      mutate(change_date =
               str_extract(.data$name, "(?<=_)[^_]+(?=_\\w+$)")) %>%
      mutate(change_date =
               as.Date(parsedate::parse_iso_8601(.data$change_date))) %>%
      mutate(download_date = str_extract(name, "\\d{8}")) %>%
      mutate(download_date =
               as.Date(parsedate::parse_iso_8601(download_date)))

    # Assert that a change_date and download_date were reported
    # in all subfolder names

    id_subfolders_error <-
      id_subfolders$name[which(is.na(id_subfolders$change_date) |
                                 is.na(id_subfolders$download_date))]

    source("./src/functions/as_character_summary.R")

    assertthat::assert_that(identical(id_subfolders_error, character(0)),
                            msg =
                              paste0("Subfolder names are not in the ",
                                     "correct [download_date]_",
                                     "[change_date]_[name] format for ",
                                     "the following subfolder(s): ",
                                     as_character_summary(id_subfolders_error),
                                     "."))

    } else

    # For raw data

    if (folder_name %in% c("raw_data")) {

    # In the first column (subfolders of id), the data refers to the date on
    # which the raw data were downloaded from the PCC database. This is the
    # date we want in order to check whether the local data are up to date.

    id_subfolders <- id_subfolders %>%
      mutate(change_date = str_extract(.data$name, "\\d{8}$")) %>%
      mutate(change_date =
               as.Date(parsedate::parse_iso_8601(.data$change_date))) %>%
      mutate(download_date = change_date)

    # Assert that a download_date was reported in all subfolder names

    id_subfolders_error <-
      id_subfolders$name[which(is.na(id_subfolders$change_date))]

    source("./src/functions/as_character_summary.R")

    assertthat::assert_that(identical(id_subfolders_error, character(0)),
                            msg =
                              paste0("Subfolder names with raw data ",
                                     "are not in the ",
                                     "correct 'download_[download_date]' ",
                                     "format for ",
                                     "the following subfolder(s): ",
                                     as_character_summary(id_subfolders_error),
                                     "."))

    }

    # Identify the row with the most recent change date

    most_recent_row <-
      id_subfolders[which.max(id_subfolders$change_date), ] %>%
      select("change_date",
             "download_date",
             "id") %>%
      as.data.frame


    # Return
    return(c(as.character(most_recent_row$change_date[1]),
             as.character(most_recent_row$download_date[1]),
             as.character(most_recent_row$id[1])))

    }
    }
  }


  # Source "get_date_local()" function to get date of local data folder ----
  # i.e. the change date (if this is reported) or download date
  # from a metadata.txt file which should be present in each local data folder

  source("./src/functions/get_date_local.R")



  # Check whether the local subfolder is up to date ----

  subfolder_paths$change_date_recent <- as.Date(NA)
  subfolder_paths$download_date_recent <- as.Date(NA)
  subfolder_paths$id_recent <- NA
  subfolder_paths$change_date_local <- as.Date(NA)
  subfolder_paths$up_to_date <- NA

  for (i in seq_along(subfolder_paths$subfolder)) {

    # Add most recent change_date and its id in Google Drive

    gd_i <- get_recent_change_date_gd(id = subfolder_paths$id[i],
                                      folder_name = subfolder_paths$folder[i])

    subfolder_paths$change_date_recent[i] <- gd_i[1]
    subfolder_paths$download_date_recent[i] <- gd_i[2]
    subfolder_paths$id_recent[i] <- gd_i[3]

    # Add the change date or download date of the local folder

    subfolder_paths$change_date_local[i] <-
      get_date_local(subfolder_paths$path[i])

    # Check whether the local folder is up to date

    subfolder_paths$up_to_date[i] <-
      ifelse((subfolder_paths$change_date_local[i] <
               subfolder_paths$change_date_recent[i]) ||
             (is.na(subfolder_paths$change_date_local[i]) &&
              !is.na(subfolder_paths$change_date_recent[i])),
             FALSE,
             TRUE)

    if (is.na(subfolder_paths$up_to_date[i])) {
      subfolder_paths$up_to_date[i] <-
        ifelse((is.na(subfolder_paths$change_date_local[i]) &&
                     is.na(subfolder_paths$change_date_recent[i])),
               TRUE,
               NA)
    }

  }

  # Notify user about the folders which are already up to date ----
  # (notification on folders which are not up to date will follow)


  subfolder_paths_up_to_date <-
    subfolder_paths$path[which(subfolder_paths$up_to_date == TRUE &
                                !is.na(subfolder_paths$change_date_local))]

  if (!identical(subfolder_paths_up_to_date, character(0))) {

  source("./src/functions/as_character_summary.R")

  cat("Local files in the folder(s)",
      as_character_summary(subfolder_paths_up_to_date),
      "are already up to date.\n")
  }




  # Update list of subfolders which need to be updated ----

  subfolder_paths_to_update <- filter(subfolder_paths,
                                      .data$up_to_date == FALSE)

  # If no subfolders need to be updated

  if (nrow(subfolder_paths_to_update) == 0) {
    cat("All local folders are up to date. No synchronisation required.\n")
  } else

  # If any subfolders need to be updated

  if (nrow(subfolder_paths_to_update) > 0) {

  # Create function to notify about status raw data (if not up to date) ----

  notify_raw <- function(change_date_recent,
                         change_date_local,
                         folder_name) {

    # Create object which represents permission to delete local data which are
    # not up to date
    delete_permission <- FALSE

    # If the local folder is empty so nothing should be removed:
    if (is.na(change_date_local)) {

      cat(paste0("Local folder '",
                 folder_name,
                 "'is empty. Raw data on Google Drive from ",
                 "download date ",
                 as.character(change_date_recent),
                 " will be downloaded.\n"))

    # If the local folder is not empty:
    } else {

      cat(paste0("Data in local folder '",
                 folder_name,
                 "' are not up to date (download date: ",
                  as.character(change_date_local),
                  "). Raw data on Google Drive are ",
                  "more recent (download date: ",
                  as.character(change_date_recent),
                  ").\n"))

      # Prompt the user for confirmation to delete the local data
      cat(paste0("Existing raw data in the local folder '",
                 folder_name,
                 "' should be deleted to make room for ",
                 "downloading more recent data."))
      confirmation <-
        readline(prompt =
                   paste0("Do you grant ",
                          "permission to delete the current raw data ",
                          "in the local folder? (Y/N): "))

      # Check the user's response
      if (tolower(confirmation) == "y") {
        delete_permission <- TRUE
      }
    }

    return(delete_permission)
  }



  # Create function to notify about status data/output (if not up to date) ----

  notify <- function(change_date_recent,
                     change_date_local,
                     folder_name) {

    # Create object which represents permission to delete local data which are
    # not up to date
    delete_permission <- FALSE

    # If the local folder is empty so nothing should be removed:
    if (is.na(change_date_local)) {

      cat(paste0("Local folder '",
                  folder_name,
                  "' is empty. Files on Google Drive from change date ",
                  as.character(change_date_recent),
                  " will be downloaded.\n"))

      # If the local folder is not empty:
    } else {

      cat(paste0("Data in local folder '",
                 folder_name,
                 "' are not up to date (change date: ",
                  as.character(change_date_local),
                  "). Files on Google Drive are more recent (change date: ",
                  as.character(change_date_recent),
                  ").\n"))

      # Prompt the user for confirmation to delete the local data
      cat(paste0("Existing files in the local folder '",
                 folder_name,
                 "' should be deleted to make room for ",
                 "downloading more files."))
      confirmation <-
        readline(prompt = paste0("Do you grant ",
                                 "permission to delete the current files ",
                                 "in the local folder? (Y/N): "))

      # Check the user's response
      if (tolower(confirmation) == "y") {
        delete_permission <- TRUE
      }
    }

    return(delete_permission)
  }





  # Create function to delete files ----

  delete_local <- function(path) {

    # List files

    local_files <- paste0(path,
                          list.files(path, recursive = TRUE))

    # Remove them

    for (i in seq_along(local_files)) {

      file.remove(local_files[i])
      cat("Deleted", local_files[i], "locally.\n")


    # Remove subfolders
    subfolders <- list.dirs(path, recursive = TRUE, full.names = TRUE)
    subfolders <- rev(subfolders[-1])
    for (subfolder in subfolders) {
      unlink(subfolder, recursive = TRUE)
      cat("Deleted subfolder", subfolder, "locally.\n")
      }
    }
  }



  # Create function which adds a column with reduced names ----

  add_shorter_name <- function(list_folders) {

    assertthat::assert_that("name" %in% names(list_folders),
                            msg = paste0("No column 'name' is present in",
                                         " the given list of folders, which is",
                                         " required to add a shorter name."))

    list_folders <- list_folders %>%
      mutate(zip = ifelse(endsWith(name, ".zip"),
                          TRUE,
                          FALSE)) %>%
      mutate(name_short = sub("\\.zip$", "", name)) %>%
      mutate(name_short = str_split(name_short, "_")) %>%
      mutate(
        name_short =
          sapply(name_short,
                 function(x) paste(x[
                   grepl("^\\d{1,2}$|^[[:alpha:]]+$|^[[:alpha:]]\\d*$|\\.",
                         x)],
                   collapse = "_"))) %>%
      mutate(name_short = ifelse(zip == TRUE,
                           paste0(name_short, ".zip"),
                           name_short)) %>%
      select(-zip)

    return(list_folders)
  }



  # Create function to add relative paths to recursive Google Drive files ----




  add_relative_paths_gd <- function(id,
                                    include_sql = FALSE) {

    id <- as_id(id)

    # Initiate object to store final paths
    files_with_paths <- NULL

# 1st level: direct children

    # All direct children
    files <- drive_ls(id) %>%
      select("name", "id") %>%
      add_shorter_name() %>%
      mutate(path = paste0("",
                           name_short))

    # Only folders among direct children
    child_folders <- drive_ls(id, type = "folder") %>%
      select("name", "id") %>%
      add_shorter_name() %>%
      mutate(path = paste0("",
                           name_short))

    # Add column with file type (folder versus regular_file)
    files <- files %>%
      mutate(file_type =
               if_else(name %in% child_folders$name,
                       "folder",
                       if_else(substr(name,
                                      nchar(name) - 3, nchar(name)) == ".zip",
                               "zip_folder",
                               "regular_file")))

    # Add files of the 1st folder level to files_with_paths
    files_with_paths <- rbind(files_with_paths,
                              files)



# 2nd level: grandchildren

if (nrow(child_folders) > 0) {

child_folders_2nd_level <- NULL

# Evaluate the folders for their content (in the second level)

for (i in seq_along(child_folders$id)) {

  id <- as_id(child_folders$id[i])

  path_start_i <- paste0(child_folders$path[i], "/")

  # All direct children
  files <- drive_ls(id) %>%
    select("name", "id") %>%
    add_shorter_name() %>%
    mutate(path = paste0(path_start_i,
                         name_short))

  # Only folders among direct children
  child_folders_i <- drive_ls(id, type = "folder") %>%
    select("name", "id") %>%
    add_shorter_name() %>%
    mutate(path = paste0(path_start_i,
                         name_short))

  # Add column with file type (folder versus regular_file)
  files <- files %>%
    mutate(file_type =
             if_else(name %in% child_folders_i$name,
                     "folder",
                     if_else(substr(name,
                                    nchar(name) - 3, nchar(name)) == ".zip",
                             "zip_folder",
                             "regular_file")))

  # Add files of the 2nd folder level to files_with_paths
  files_with_paths <- rbind(files_with_paths,
                            files)

  # Add child_folders to child_folders_2nd_level
  child_folders_2nd_level <- rbind(child_folders_2nd_level,
                                   child_folders_i)
}
}



# 3rd level: great-grandchildren

if (nrow(child_folders_2nd_level) > 0) {

child_folders_3rd_level <- NULL

# Evaluate the folders for their content (in the second level)

for (i in seq_along(child_folders_2nd_level$id)) {

  id <- as_id(child_folders_2nd_level$id[i])

  path_start_i <- paste0(child_folders_2nd_level$path[i], "/")

  # All direct children
  files <- drive_ls(id) %>%
    select("name", "id") %>%
    add_shorter_name() %>%
    mutate(path = paste0(path_start_i,
                         name_short))

  # Only folders among direct children
  child_folders_i <- drive_ls(id, type = "folder") %>%
    select("name", "id") %>%
    add_shorter_name() %>%
    mutate(path = paste0(path_start_i,
                         name_short))

  # Add column with file type (folder versus regular_file)
  files <- files %>%
    mutate(file_type =
             if_else(name %in% child_folders_i$name,
                     "folder",
                     if_else(substr(name,
                                    nchar(name) - 3, nchar(name)) == ".zip",
                             "zip_folder",
                             "regular_file")))

  # Add files of the 2nd folder level to files_with_paths
  files_with_paths <- rbind(files_with_paths,
                            files)

  # Add child_folders to child_folders_2nd_level
  child_folders_3rd_level <- rbind(child_folders_3rd_level,
                                   child_folders_i)
}




# 4th level: great-great-grandchildren

if (nrow(child_folders_3rd_level) > 0) {

  child_folders_4th_level <- NULL

  # Evaluate the folders for their content (in the second level)

  for (i in seq_along(child_folders_3rd_level$id)) {

    id <- as_id(child_folders_3rd_level$id[i])

    path_start_i <- paste0(child_folders_3rd_level$path[i], "/")

    # All direct children
    files <- drive_ls(id) %>%
      select("name", "id") %>%
      add_shorter_name() %>%
      mutate(path = paste0(path_start_i,
                           name_short))

    # Only folders among direct children
    child_folders_i <- drive_ls(id, type = "folder") %>%
      select("name", "id") %>%
      add_shorter_name() %>%
      mutate(path = paste0(path_start_i,
                           name_short))

    # Add column with file type (folder versus regular_file)
    files <- files %>%
      mutate(file_type =
               if_else(name %in% child_folders_i$name,
                       "folder",
                       if_else(substr(name,
                                      nchar(name) - 3, nchar(name)) == ".zip",
                               "zip_folder",
                               "regular_file")))

    # Add files of the 2nd folder level to files_with_paths
    files_with_paths <- rbind(files_with_paths,
                              files)

    # Add child_folders to child_folders_2nd_level
    child_folders_4th_level <- rbind(child_folders_4th_level,
                                     child_folders_i)
  }



  # 5th level: great-great-great-grandchildren

  if (nrow(child_folders_4th_level) > 0) {

    child_folders_5th_level <- NULL

    # Evaluate the folders for their content (in the second level)

    for (i in seq_along(child_folders_4th_level$id)) {

      id <- as_id(child_folders_4th_level$id[i])

      path_start_i <- paste0(child_folders_4th_level$path[i], "/")

      # All direct children
      files <- drive_ls(id) %>%
        select("name", "id") %>%
        add_shorter_name() %>%
        mutate(path = paste0(path_start_i,
                             name_short))

      # Only folders among direct children
      child_folders_i <- drive_ls(id, type = "folder") %>%
        select("name", "id") %>%
        add_shorter_name() %>%
        mutate(path = paste0(path_start_i,
                             name_short))

      # Add column with file type (folder versus regular_file)
      files <- files %>%
        mutate(file_type =
                 if_else(name %in% child_folders_i$name,
                         "folder",
                         if_else(substr(name,
                                        nchar(name) - 3, nchar(name)) == ".zip",
                                 "zip_folder",
                                 "regular_file")))

      # Add files of the 2nd folder level to files_with_paths
      files_with_paths <- rbind(files_with_paths,
                                files)

      # Add child_folders to child_folders_2nd_level
      child_folders_5th_level <- rbind(child_folders_5th_level,
                                       child_folders_i)
    }
  }

  if (nrow(child_folders_4th_level) > 0) {
    assertthat::assert_that(nrow(child_folders_5th_level) == 0,
                            msg = paste0("Folder contains more than four levels",
                                         " of child folders.",
                                         " Update the code of the",
                                         " 'add_relative_paths_gd' function",
                                         " so that it allows for more than",
                                         " four levels of subfolders."))
  }
}
}

# Exclude .sql files by default

if (include_sql == FALSE) {
  files_with_paths <-
    files_with_paths[!grepl("\\.sql$", files_with_paths$name), ]
}


files_with_paths <- files_with_paths %>%
  arrange(path)

return(files_with_paths)
}





  # Create function to download files in subfolders from Google Drive ----

  download_from_google_drive <- function(path,
                                         id_recent,
                                         change_date_recent,
                                         download_date_recent) {

    id_recent <- as_id(id_recent)

    # Generate list of all files inside id_recent folder
    # and the relative local paths where they should go

    files_with_paths <- add_relative_paths_gd(id = id_recent)

    # Evaluate for each of the items in this file list how they should
    # be downloaded

    for (i in seq_along(files_with_paths$id)) {

      path_i <- paste0(path, files_with_paths$path[i])


      # For folders: create a new folder

      if (files_with_paths$file_type[i] == "folder") {
        dir.create(path_i, recursive = TRUE)
      }


      # For regular files: download the file

      if (files_with_paths$file_type[i] == "regular_file") {
        drive_download(as_id(files_with_paths$id[i]),
                       path = path_i)
      }


      # For zip folders: download the files in the zip folder

      if (files_with_paths$file_type[i] == "zip_folder") {

        # Create a new folder in local project where data need to be stored
        dir.create(str_remove(path_i,
                              "\\.zip$"))

        # Download zip file from Google Drive into a temporary directory
        drive_download(as_id(files_with_paths$id[i]),
                       file.path(tempdir(),
                                 files_with_paths$path[i]))

        # Unzip zip file into temporary directory
        unzip(file.path(tempdir(), files_with_paths$path[i]),
              exdir = file.path(tempdir(),
                                str_remove(files_with_paths$path[i],
                                           "\\.zip$")))

        # Copy files into new folder where they should be
        list.files(file.path(tempdir(),
                             str_remove(files_with_paths$path[i],
                                        "\\.zip$")),
                   full.names = TRUE) %>%
          file.copy(to = str_remove(path_i, "\\.zip$"),
                    recursive = TRUE)


        # Remove files from today from temporary directory

        files_temp <- list.files(tempdir(), full.names = TRUE)

        recent_files_temp <-
          files_temp[file.info(files_temp)$mtime >=
                       (Sys.time() - as.difftime(10, units = "mins"))]

        if (!identical(recent_files_temp, character(0))) {
          unlink(recent_files_temp, recursive = TRUE)
        }
      }
    }


    # Save a metadata.txt file with the date

    if (path == "./data/raw_data/") {
      metadata <-
        c(paste0("Download date: ",
                 as.Date(parsedate::parse_iso_8601(change_date_recent))))
      metadata_file <- paste0(path, "metadata.txt")
      writeLines(metadata, con = metadata_file)

    } else {
      metadata <-
        c(paste0("Change date: ",
                 as.Date(parsedate::parse_iso_8601(change_date_recent))),
          paste0("Download date: ",
                 as.Date(parsedate::parse_iso_8601(download_date_recent))))
      metadata_file <- paste0(path, "metadata.txt")
      writeLines(metadata, con = metadata_file)
    }


    # Notify user

    file_count <- length(which(files_with_paths$file_type == "regular_file"))
    cat("Downloaded", file_count,
        "files from Google Drive into the folder",
        path, "\n")

  }



# Synchronise additional_data ----

  if (is.null(list_subfolders_data_orig) ||
      (!isFALSE(list_subfolders_data_orig) &&
       "additional_data" %in% list_subfolders_data_orig))

# Check which files need to be added

additional_local <- list.files("./data/additional_data/",
                               recursive = TRUE)

additional_gd <-
  add_relative_paths_gd(id = as_id(gd_data$id[which(gd_data$name ==
                                                    "additional_data")]))

if (!identical(additional_local, character(0))) {
additional_gd <-
  additional_gd[which(!additional_gd$path %in% additional_local), ]
}

# Update additional data if any of the Google Drive files is not in the local
# documents

if (nrow(additional_gd[which(additional_gd$file_type != "folder"), ]) > 0) {

for (i in seq_along(additional_gd$id)) {
  
  path_i <- paste0(path, additional_gd$path[i])
  
  # For folders: create a new folder
  
  if (additional_gd$file_type[i] == "folder") {
    dir.create(path_i, recursive = TRUE)
  }

  # For regular files: download the file
  
  if (additional_gd$file_type[i] == "regular_file") {
    drive_download(as_id(additional_gd$id[i]),
                   path = path_i)
  }
}
cat(paste0("Data synchronisation for '",
               "additional_data",
               "' complete\n"))

}




  # Synchronise each of the folders ----

  for (i in seq_along(subfolder_paths_to_update$subfolder)) {

    # Notify the user that the local data are not up to date
    # and ask permission to delete any data which are currently saved locally
    # in the given folder.

    # If the folder contains raw data

    if (subfolder_paths_to_update$folder[i] %in% c("raw_data")) {

      delete_permission <-
        notify_raw(change_date_recent =
                     subfolder_paths_to_update$change_date_recent[i],
                   change_date_local =
                     subfolder_paths_to_update$change_date_local[i],
                   folder_name =
                     subfolder_paths_to_update$subfolder[i])
    } else


      # If the folder contains intermediate, layer1, layer2 or output data

    if (!subfolder_paths_to_update$folder[i] %in% c("raw_data")) {

      delete_permission <-
        notify(change_date_recent =
                 subfolder_paths_to_update$change_date_recent[i],
               change_date_local =
                 subfolder_paths_to_update$change_date_local[i],
               folder_name =
                 subfolder_paths_to_update$subfolder[i])
    }


    # If permission to delete local data is given: delete local data

      if (delete_permission == TRUE) {
        delete_local(path = subfolder_paths_to_update$path[i])
      }

    # If permission was given to delete files, or if no files had to be deleted:

    if (delete_permission == TRUE ||
               is.na(subfolder_paths_to_update$change_date_local[i])) {

      download_from_google_drive(
         path =
           subfolder_paths_to_update$path[i],
         id_recent =
           subfolder_paths_to_update$id_recent[i],
         change_date_recent =
           subfolder_paths_to_update$change_date_recent[i],
         download_date_recent =
           subfolder_paths_to_update$download_date_recent[i])

      cat(paste0("Data synchronisation for '",
                 subfolder_paths_to_update$subfolder[i],
                 "' complete\n"))

    }
  }

}


}
