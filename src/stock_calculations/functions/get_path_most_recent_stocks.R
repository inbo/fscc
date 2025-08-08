
get_path_most_recent_stocks <- function(shorter_var_name) {

  # Import most recent stocks

  dir <- paste0(list.dirs("./output/stocks", recursive = FALSE)[
    grepl(paste0("_", shorter_var_name, "_stocks"),
          list.dirs("./output/stocks", recursive = FALSE))], "/")

  if (length(dir) > 1) {

    # Check if there are csv files inside

    # Function to check if CSV files exist in the top-level folder

    check_csv_files <- function(folder) {

      csv_files <- list.files(folder,
                              pattern = "\\.csv$",
                              full.names = TRUE,
                              recursive = FALSE)

      # TRUE if there are any CSV files, FALSE otherwise
      return(length(csv_files) > 0)
    }

    # Apply the function to all folders and create a vector of TRUE/FALSE

    csv_check <- sapply(dir, check_csv_files)

    dir <- dir[which(csv_check)]

    assertthat::assert_that(length(dir) >= 1)

    # Get the dates from the folder names

    dates <- as.Date(sapply(strsplit(basename(dir), "_"), `[`, 1),
                     format = "%Y%m%d")

    dir <- dir[which.max(dates)]

  }

  return(dir)

}
