
# Transformation of "layer 1" ICP Forests solid soil data to obtain "layer 1+"
# ---------------------------------------------------------------------------

# Script initiation date: 22 March 2024

# Details: In this script, corrected and internally gap-filled "layer 1" data
# are processed to produce "layer 1+", which basically further gap-fills the
# "layer 1" based on rougher predictions (e.g. mainly pedotransfer functions).


# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O

# Attention: in order to be able to run the script, a number of
# additional data forms (which are not part of this GitHub repository
# or the raw "layer 0" data) are required.
# These can be requested from FSCC.

# Status:
# - Almost complete for Level II (so) (complete for variables required
#   for carbon stock calculations)
# - Almost complete for Level I (s1) (complete for variables required
#   for carbon stock calculations)



# 1. Prepare packages ----

# Define required packages
stopifnot(require("sf"),
          require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"),
          require("aqp"))


# 2. Import 'layer 1" data ----

source("./src/functions/read_processed.R")
read_processed(path_name = "./data/layer1_data/",
               save_to_env = TRUE)



# 3. Gap-fill bulk densities for mineral layers ----

















