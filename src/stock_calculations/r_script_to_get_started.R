
# This script puts the data ready in order to get started

# Load packages which are used throughout the "fscc" R project/repo ----

stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"))

# Download the most recent versions of the data from Google Drive to local ----

source("./src/functions/sync_local_data.R")
sync_local_data()

# Import the local data in R ----

source("./src/functions/read_processed.R")
read_processed(save_to_env = TRUE)

# Add code to process the data below ----

