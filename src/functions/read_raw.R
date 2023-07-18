
#' Read raw solid soil data for ICP Forests soil analysis
#'
#' This function imports the raw survey forms of the given survey and additional
#' dataframes with ICP Forests soil data (so-called "Layer 0") from local
#' folders.
#'
#' @param survey The name/code of the ICP Forests survey to import
#' @param download_date Default is NULL since unnecessary in local folder
#' structure of 'fscc' project, which only contains one version of the data.
#' Enter a character string of the date in the format "YYYYMMDD" if multiple
#' raw data versions exist (organised in 'raw_data/download_date_YYYYMMDD/'
#' subfolders).
#' @param save_to_env Logical which indicates whether the output dataframes
#' can be saved to the global environment and override any existing objects
#' with the same name. Default is FALSE.
#' @details
#' This function imports survey forms of the specified survey, along with some
#' additional dataframes containing ICP Forests soil data. Although the
#' function is customised for the soil surveys of ICP Forests, it can also
#' be used to import the other surveys. The function expects the survey forms
#' to be downloaded from the ICP Forests website and organised in a specific
#' folder structure.
#'
#' The function also creates a dataframe with the most common coordinates for
#' each plot in the survey, assuming these are the "correct" coordinates. It
#' is recommended to first import data for the system installment surveys
#' before importing data from other surveys, as the coordinates of non-system
#' installment surveys depend on the coordinates of system installment surveys
#' in the global environment.
#'
#' Requirements:
#'
#'   Data need to be downloaded from the ICP Forests website
#'   Link: https://www.icp-forests.org/data/fm_start.php
#'   To download data from for example the "s1" survey:
#'   Inspect data > Data table: e.g. "s1_som" (or any other "s1" survey form);
#'   Country/partner: "all"; Year: "all"; Mode: "SURVEY" > Show
#'   Repeat this for the other surveys.
#'
#'   Create a folder with the name "Download_YYYYMMDD" (replace YYYYMMDD by the
#'   download date) in the root folder, and place all downloaded surveys in this
#'   folder.
#'
#'   Organise your folders in the following way:
#'    .
#'    └── ICP Forests - soil data (R project)
#'    ├── src
#'    │   ├── functions
#'    │   ├── transformation_to_layer1
#'    │   └── (...)
#'    ├── output
#'    └── data
#'        ├── raw_data
#'        │   ├── s1
#'        │   ├── so
#'        │   ├── y1
#'        │   ├── si
#'        │   └── sw
#'        ├── intermediate_data
#'        │   └── (...)
#'        ├── additional_data
#'        └── (...)
#'
#' This function uses:
#'    - the function "dec_coordinate()"
#'      which converts a coordinate in +/-DDMMSS to decimal degrees
#'      and returns the decimal coordinate along with an "error" if any of the
#'      original MM or SS is not in the 0-59 range, or an NA if this is not the
#'      case
#'
#' Outputs - This function returns the following to the global environment:
#' - dataframes for each of the survey forms of the given survey
#' - "d_country"
#' - "d_partner"
#' - "data_availability_[CODESURVEY]"
#' - "coordinates_[CODESURVEY]"
#'
#' # How are the survey forms exactly updated in this function?
#' 1. Replace empty spaces ("") by NA
#' 2. Convert columns with time/date information to a date with the function
#'    "as.Date()" (in order to allow uniform reporting of the download_date
#'    and change_date in Partner Inconsistency Reports)
#' 3. Add a column with the date on which the data were downloaded from the
#'    central ICP Forests database (Layer 0)
#' 4. Add column "layer_type": factor with levels "forest_floor", "mineral"
#'    and "peat"
#' 5. Add columns "country", "partner_short" and "partner" (with the respective
#'    country, partner_short and partner names)
#' 6. "som" survey forms: column "repetition" becomes 1 for records where
#'    "repetition" is NA or -9999
#' 7. Convert coordinates in +/-DDMMSS to decimal coordinates (columns
#'    "longitude_dec" and "latitude_dec"). Columns "longitude_error" and
#'    "latitude_error", which indicate whether MM and/or SS are outside the
#'    possible 0-59 range, are added.
#' 8. Add columns with unique identifiers for plots, surveys,
#'    repetitions/profile_pit_id's, layers
#'        - "plot_id": "partner_code" + "code_plot"
#'        - "unique_survey": "partner_code" + "survey_year" + "code_plot"
#'        - "unique_survey_repetition": "partner_code" + "survey_year" +
#'          "code_plot" + "repetition" ("som" survey forms)
#'        - "unique_survey_profile": "partner_code" + "survey_year" +
#'          "code_plot" + "profile_pit_id" ("pfh" survey forms)
#'        - "unique_survey_layer": "partner_code" + "survey_year" +
#'          "code_plot" + "code_layer" ("som" survey forms);
#'           or:
#'           "partner_code" + "survey_year" +
#'          "code_plot" + "horizon_master" ("pfh" survey forms)
#'        - "unique_layer_repetition": "partner_code" + "survey_year" +
#'          "code_plot" + "code_layer" + "repetition" ("som" survey forms)
#'
#' WARNING - This function may not be optimally efficient and may ideally
#' require refactoring for better performance.
#'
#'
#' @examples
#' read_raw("s1", "20221116")


read_raw <- function(code_survey,
                     download_date = NULL,
                     save_to_env = FALSE) {



# Create a list with names of the different survey forms per survey

list_data_tables <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                         s1 = c("som", "prf", "pls", "pfh", "lqa"),
                         si = c("eve", "plt", "sta", "tco"),
                         y1 = c("pl1", "st1"),
                         sw = c("swa", "swc"),
                         ss = c("lqa", "pss", "ssm"),
                         bd = c("gpl", "dbh", "tht", "dwd", "can", "gvg"),
                         lf = c("lfp", "lfm", "lqa"),
                         c1 = c("tre", "trf"),
                         f1 = c("plf", "fot", "fom", "lqa"),
                         aq = c("pac", "pps", "aqa", "aqp",
                                "col", "aqb"),
                         cc = c("trc", "trd"),
                         dp = c("pld", "dem", "lqa"),
                         fo = c("plf", "fot", "fom", "lqa"),
                         gr = c("pli", "ipm", "inv", "ira",
                                "irh", "irm", "irp", "iev"),
                         gv = c("plv", "vem"),
                         gb = c("pgb", "gbm", "gbh", "lqa"),
                         la = c("pla", "lam", "lap", "llf"),
                         li = c("lip", "lit", "lis", "lia"),
                         mm = c("plm", "mem", "meh"),
                         oz = c("pll", "ltf", "lss", "ots",
                                "ozp"),
                         ph = c("phe", "plp", "phi", "phd",
                                "phc"))

# Assert that code_survey exists

assertthat::assert_that(code_survey %in% names(list_data_tables),
                        msg = paste0("The given survey code does not exist",
                                     " in ICP Forests"))

# Identify the level of the survey (Level I versus Level II)

if (code_survey %in% c("s1", "y1", "bd", "c1", "f1")) {
  survey_level <- "LI"
  }
if (code_survey %in% c("so", "si", "sw", "ss", "lf",
                       "aq", "cc", "dp", "fo", "gr",
                       "gv", "gb", "la", "li", "mm",
                       "oz", "ph")) {
  survey_level <- "LII"
  }

# If an input download_date was given ----

if (!is.null(download_date)) {

# Assert that download_date is in the right format

assertthat::assert_that(class(download_date) == "character" &&
                          !is.na(parsedate::parse_iso_8601(download_date)),
                        msg = paste0("The given download_date should be a ",
                                     "character string with format 'YYYYMMDD'"))

# Identify the name of the directory with data

dir <- paste0("./data/raw_data/download_", download_date)

# Assert that data are downloaded for the given download date

assertthat::assert_that(dir %in% list.dirs(path = "./data/raw_data",
                                           recursive = FALSE),
                        msg = paste0("There is no folder for the given ",
                                     "download date. Downloaded data must be ",
                                     "located in the path ",
                                     "./data/raw_data/",
                                     "download_[download_date]"))

# Identify the name of the subdirectory with data

subdir <- list.dirs(path = dir,
                    full.names = TRUE, recursive = TRUE) %>%
  str_subset(., pattern = code_survey) %>%
  str_subset(., pattern = "\\d$") # selects the string that ends in a number
                                 # (i.e. not the subfolders)

}

# If no input download_date was given ----

if (is.null(download_date)) {

  dir <- "./data/raw_data/"
  subdir <- paste0(dir, code_survey, "/")

}


# Import d_country and d_partner

d_country <- read.csv(paste0(subdir,
                             "adds/dictionaries/d_country.csv"), sep = ";")
d_partner <- read.csv(paste0(subdir,
                             "adds/dictionaries/d_partner.csv"), sep = ";")


# Import data_availability of the given survey, add unique identifiers and
# country/partner names, and save to the global environment

if (file.exists(paste0(subdir,
                       "/adds/data_availability_",
                       code_survey, ".csv"))) {

  data_availability <-
    read.csv(paste0(subdir,
                    "/adds/data_availability_",
                    code_survey, ".csv"), sep = ";")

 data_availability <- data_availability %>%
   mutate(plot_id = paste0(partner_code, "_",
                           code_plot)) %>%
   mutate(unique_survey = paste0(partner_code, "_",
                                 survey_year, "_",
                                 code_plot)) %>%
   left_join(d_country[, c("code", "lib_country")],
             by = join_by(code_country == code)) %>%
   rename(country = lib_country) %>%
   left_join(d_partner[, c("code", "desc_short", "description")],
             by = join_by(partner_code == code)) %>%
   rename(partner_short = desc_short) %>%
   rename(partner = description) %>%
   mutate(country = as.factor(country)) %>%
   mutate(partner_short = as.factor(partner_short)) %>%
   mutate(partner = as.factor(partner))

 }

# Import individual survey forms of the given survey ----

# For each of the data tables in the survey:

survey_forms <- paste0(code_survey, "_",
                       list_data_tables[[which(names(list_data_tables) ==
                                                 code_survey)]])

survey_forms_extended <- c(survey_forms,
                           paste0("data_availability_", code_survey),
                           paste0("coordinates_", code_survey))

if (isTRUE(getOption("knitr.in.progress"))) {
  envir <- knitr::knit_global()
} else {
  envir <- globalenv()
}

if (save_to_env == TRUE) {

for (i in seq_along(survey_forms_extended)) {

  if (exists(survey_forms_extended[i], envir = envir)) {
    rm(list = survey_forms_extended[i], envir = envir)
  }
  }
}

for (i in seq_along(survey_forms)) {
  
  # Read the data table ----
  df <- read.csv(paste0(subdir, "/", code_survey, "_",
                       list_data_tables[[which(names(list_data_tables) ==
                                                 code_survey)]][i], ".csv"),
                sep = ";")

  # Replace empty spaces by NA ----
  df[df == ""] <- NA

  # Convert columns that should be factors to factors ----
  vec_as_factor <- which(names(df) %in%
                         c("code_layer", "code_texture_class", "origin",
                           "horizon_master", "horizon_subordinate",
                           "colour_moist", "colour_dry",
                           "code_horizon_texture_class", "profile_pit_ID",
                           "profile_pit_id", "code_wrb_soil_group",
                           "code_wrb_qualifier_1",
                           "code_wrb_spezifier_1", "code_wrb_qualifier_2",
                           "code_wrb_spezifier_2", "code_wrb_qualifier_3",
                           "code_wrb_spezifier_3", "code_wrb_qualifier_4",
                           "code_wrb_spezifier_4", "code_wrb_qualifier_5",
                           "code_wrb_spezifier_5", "code_wrb_qualifier_6",
                           "code_wrb_spezifier_6", "classification_full_name",
                           "diagnostic_1", "diagnostic_2", "diagnostic_3",
                           "diagnostic_4", "diagnostic_5", "diagnostic_6",
                           "diagnostic_7", "diagnostic_8", "diagnostic_9",
                           "diagnostic_10", "code_wrb_publication",
                           "code_parameter", "code_pretreatment",
                           "code_sample_prep", "code_determination",
                           "laboratory_id", "sw_id",
                           "code_depth_layer", "pfh_id", "code_event_type",
                           "product_material"))

  for (j in vec_as_factor) {
    df[, j] <- as.factor(df[, j])
  }


  # Convert the columns with dates to dates ----
  # This removes information about the time of the day in "change_date",
  # but this is usually less important, and ensures that "change_date"
  # and "download_date" are harmonised in PIRs

  vec_as_date <- grep("(d|D)ate", names(df))
  if (!identical(vec_as_date, integer(0))) {
    for (j in vec_as_date) {
      df[, j] <- as.character(as.Date(df[, j]))
    }
    }

  # Add a column with download_date ----

  # If no download_date was given
  
  if (is.null(download_date)) {
  source("./src/functions/get_date_local.R")
  download_date <- get_date_local(path = dir)
  }
  
  df$download_date <-
    as.character(as.Date(parsedate::parse_iso_8601(download_date)))

  # For s1_som and so_som: add column "layer type" ----
  if ((code_survey == "so" || code_survey == "s1") && 
     list_data_tables[[
       which(names(list_data_tables) == code_survey)]][i] == "som") {

    layer_type_levels <- list(peat = c("H", "H05", "H12", "H24", "H51", "Hxx",
                                        "H1", "H2", "HFH", "HH", "HL", "H01",
                                        "H48", "HFS", "HS", "HF"),
                          forest_floor = c("O", "O1", "O2", "O3", "OF", "OFH",
                                           "OH", "OL", "OLF"),
                          mineral = c("M01", "M02", "M03", "M05", "M12", "M13",
                                       "M23", "M24", "M25", "M26", "M34",
                                       "M35", "M36", "M38", "M41", "M45",
                                       "M46", "M47", "M48", "M51", "M52",
                                       "M57", "M58", "M68", "M69", "M78",
                                       "M92", "M94", "Mxx", "M27", "M39",
                                       "M42"))

    df$layer_type <- df$code_layer
    levels(df$layer_type) <- layer_type_levels
    }

  # For s1_pfh and so_pfh: add column "layer type" ----
  if ((code_survey == "so" || code_survey == "s1") && 
      list_data_tables[[
        which(names(list_data_tables) == code_survey)]][i] == "pfh") {

    df$layer_type <- NA
      for (j in seq_len(nrow(df))) {
        if (!is.na(df$horizon_master[j])) {
        if (str_detect(df$horizon_master[j], "^O")) {
          # if horizon_master starts with O
          df$layer_type[j] <- "forest_floor"
     } else

      if (str_detect(df$horizon_master[j], "^H")) {
        # if horizon_master starts with H
      df$layer_type[j] <- "peat"
      } else {
        df$layer_type[j] <- "mineral"
        }
        }
        }

     df$layer_type <- as.factor(df$layer_type)
     }


  # For sw_swc: add column "layer type" ----
  if ((code_survey == "sw") &&
      list_data_tables[[
        which(names(list_data_tables) == code_survey)]][i] == "swc") {

    df$layer_type <- NA
  for (j in seq_len(nrow(df))) {
    if (str_detect(df$code_depth_layer[j], "^O")) {
    # if horizon_master starts with O
    df$layer_type[j] <- "forest_floor"
  } else

    if (str_detect(df$code_depth_layer[j], "^H")) {
      # if horizon_master starts with H
      df$layer_type[j] <- "peat"
    } else {
      df$layer_type[j] <- "mineral"
    }
    }

  df$layer_type <- as.factor(df$layer_type)
  }


  # Add columns with country and partner names ----

  df <- df %>%
    left_join(d_country[, c("code", "lib_country")],
            by = join_by(code_country == code)) %>%
    rename(country = lib_country) %>%
    left_join(d_partner[, c("code", "desc_short", "description")],
              by = join_by(partner_code == code)) %>%
    rename(partner_short = desc_short) %>%
    rename(partner = description) %>%
    mutate(country = as.factor(country)) %>%
    mutate(partner_short = as.factor(partner_short)) %>%
    mutate(partner = as.factor(partner)) %>%
    relocate(partner) %>%
    relocate(partner_short) %>%
    relocate(country)



  # For any coordinates (+/-DDMMSS): convert to decimal coordinates ----
  # Add new columns named with "_dec" to store the decimal coordinates,
  # and new columns named with "_error" to store possible inconsistencies
  # in the original coordinate (if MM or SS is not in the possible 0-59 range)

  renamed_latitude <- NA
  renamed_longitude <- NA

  # Temporarily rename columns with coordinates when needed

  if (any(!is.na(which(names(df) == "latitude_plot")))) {
    renamed_latitude <- TRUE
    names(df)[which(names(df) == "latitude_plot")] <- "latitude"
    }

  if (any(!is.na(which(names(df) == "longitude_plot")))) {
    renamed_longitude <- TRUE
    names(df)[which(names(df) == "longitude_plot")] <- "longitude"
    }

  # Convert coordinates to decimals

  if (any(!is.na(which(names(df) == "latitude"))) &&
      any(!is.na(which(names(df) == "longitude")))) {

  df$latitude_dec <- NA
  df$longitude_dec <- NA
  df$latitude_error <- NA
  df$longitude_error <- NA

  source("./src/functions/dec_coordinate.R")
  for (j in seq_len(nrow(df))) {

    if (!is.na(df$latitude[j])) {
    coord_dec_output <- dec_coordinate(df$latitude[j])
    df$latitude_dec[j] <- coord_dec_output[1]
    df$latitude_error[j] <- coord_dec_output[2]
   }

   if (!is.na(df$longitude[j])) {
     coord_dec_output <- dec_coordinate(df$longitude[j])
     df$longitude_dec[j] <- coord_dec_output[1]
     df$longitude_error[j] <- coord_dec_output[2]
     }
    }
  }

  # Rename coordinate columns to the original column name when needed

  if (!is.na(renamed_latitude)) {
    names(df)[which(names(df) == "latitude")] <- "latitude_plot"
    }
  if (!is.na(renamed_longitude)) {
    names(df)[which(names(df) == "longitude")] <- "longitude_plot"
    }

  # if "repetition" is "NA": insert "1" ----

  if (!identical(which(names(df) == "repetition"),
                 integer(0))) { # if the column "repetition" exists in df
    df$repetition[which(is.na(df$repetition) | df$repetition == -9999)] <- 1
    }


  # Add columns with unique identifiers ----
  # for plots, surveys, repetitions/profile_pit_id's, layers

  vec <- c(partner_code = (which(names(df) == "partner_code")),
           survey_year = (which(names(df) == "survey_year")),
           code_plot = (which(names(df) == "code_plot")),
           repetition = (which(names(df) == "repetition")),
           code_layer = (which(names(df) == "code_layer")),
           profile_pit_id = (which(names(df) == "profile_pit_id")),
           horizon_master = (which(names(df) == "horizon_master")))

  # plot_id
  if (!identical(which(names(vec) == "partner_code"), integer(0)) &&
      !identical(which(names(vec) == "code_plot"), integer(0))) {
    df$plot_id <- paste0(df[, vec[which(names(vec) == "partner_code")]], "_",
                            df[, vec[which(names(vec) == "code_plot")]])
      }

  # unique_survey
  if (!identical(which(names(vec) == "partner_code"), integer(0)) &&
      !identical(which(names(vec) == "survey_year"), integer(0)) &&
      !identical(which(names(vec) == "code_plot"), integer(0))) {
    df$unique_survey <- paste0(df[, vec[which(names(vec) == "partner_code")]],
                              "_",
                              df[, vec[which(names(vec) == "survey_year")]],
                              "_",
                              df[, vec[which(names(vec) == "code_plot")]])
  }

  # unique_survey_repetition
  if (!identical(which(names(vec) == "partner_code"), integer(0)) &&
      !identical(which(names(vec) == "survey_year"), integer(0)) &&
      !identical(which(names(vec) == "code_plot"), integer(0)) &&
      !identical(which(names(vec) == "repetition"), integer(0))) {
    df$unique_survey_repetition <-
                       paste0(df[, vec[which(names(vec) == "partner_code")]],
                              "_",
                              df[, vec[which(names(vec) == "survey_year")]],
                              "_",
                              df[, vec[which(names(vec) == "code_plot")]],
                              "_",
                              df[, vec[which(names(vec) == "repetition")]])}

  # unique_survey_profile
  if (!identical(which(names(vec) == "partner_code"), integer(0)) &&
      !identical(which(names(vec) == "survey_year"), integer(0)) &&
      !identical(which(names(vec) == "code_plot"), integer(0)) &&
      !identical(which(names(vec) == "profile_pit_id"), integer(0))) {
    df$unique_survey_profile <-
    paste0(df[, vec[which(names(vec) == "partner_code")]], "_",
           df[, vec[which(names(vec) == "survey_year")]], "_",
           df[, vec[which(names(vec) == "code_plot")]], "_",
           df[, vec[which(names(vec) == "profile_pit_id")]])
  }

  # unique_survey_layer
  if (!identical(which(names(vec) == "partner_code"), integer(0)) &&
      !identical(which(names(vec) == "survey_year"), integer(0)) &&
      !identical(which(names(vec) == "code_plot"), integer(0)) &&
      !identical(which(names(vec) == "code_layer"), integer(0))) {
    df$unique_survey_layer <-
                       paste0(df[, vec[which(names(vec) == "partner_code")]],
                              "_",
                              df[, vec[which(names(vec) == "survey_year")]],
                              "_",
                              df[,vec[which(names(vec) == "code_plot")]],
                              "_",
                              df[, vec[which(names(vec) == "code_layer")]])
  }

  if (!identical(which(names(vec) == "partner_code"), integer(0)) &&
      !identical(which(names(vec) == "survey_year"), integer(0)) &&
      !identical(which(names(vec) == "code_plot"), integer(0)) &&
      !identical(which(names(vec) == "horizon_master"), integer(0))) {
    df$unique_survey_layer <-
    paste0(df[, vec[which(names(vec) == "partner_code")]], "_",
           df[, vec[which(names(vec) == "survey_year")]], "_",
           df[, vec[which(names(vec) == "code_plot")]], "_",
           df[, vec[which(names(vec) == "horizon_master")]])
  }

  # unique_layer_repetition
  if (!identical(which(names(vec) == "partner_code"), integer(0)) &&
      !identical(which(names(vec) == "survey_year"), integer(0)) &&
      !identical(which(names(vec) == "code_plot"), integer(0)) &&
      !identical(which(names(vec) == "code_layer"), integer(0)) &&
      !identical(which(names(vec) == "repetition"), integer(0))) {
    df$unique_layer_repetition <-
                       paste0(df[, vec[which(names(vec) == "partner_code")]],
                              "_",
                              df[, vec[which(names(vec) == "survey_year")]],
                              "_",
                              df[, vec[which(names(vec) == "code_plot")]],
                              "_",
                              df[, vec[which(names(vec) == "code_layer")]],
                              "_",
                              df[, vec[which(names(vec) == "repetition")]])
  }

  if (!identical(which(names(vec) == "partner_code"), integer(0)) &&
      !identical(which(names(vec) == "survey_year"), integer(0)) &&
      !identical(which(names(vec) == "code_plot"), integer(0)) &&
      !identical(which(names(vec) == "horizon_master"), integer(0)) &&
      !identical(which(names(vec) == "profile_pit_id"), integer(0))) {
    df$unique_layer_repetition <-
    paste0(df[, vec[which(names(vec) == "partner_code")]], "_",
           df[, vec[which(names(vec) == "survey_year")]], "_",
           df[, vec[which(names(vec) == "code_plot")]], "_",
           df[, vec[which(names(vec) == "horizon_master")]], "_",
           df[, vec[which(names(vec) == "profile_pit_id")]])
  }


  # Save the survey forms to the global environment if permitted ----

  if (save_to_env == TRUE) {

  source("./src/functions/assign_env.R")

  assign_env(paste0(code_survey, "_",
                list_data_tables[[which(names(list_data_tables) ==
                                          code_survey)]][i]),
             df)

  }
  }







  # Create a dataframe with the most common coordinates for each plot ----

  # List all the surveys where coordinates have to be looked for

surveys <- paste0(code_survey, "_",
                  list_data_tables[[which(names(list_data_tables) ==
                                            code_survey)]])

if (survey_level == "LI" &&
    code_survey != "y1") {
  surveys <- c(surveys,
               paste0("y1", "_",
               list_data_tables[[which(names(list_data_tables) == "y1")]]))
  }

if (survey_level == "LII" &&
    code_survey != "si") {
  surveys <- c(surveys,
               paste0("si", "_",
                      list_data_tables[[which(names(list_data_tables) ==
                                                "si")]]))
  }

  # Retrieve survey forms with coordinates (from global environment)

source("./src/functions/get_env.R")

survey_forms_with_coordinates <- NULL

for (i in seq_along(surveys)) {
  if (surveys[i] %in% ls(envir = .GlobalEnv)) {
    if (!identical(get_env(surveys[i])$latitude_dec, NULL) &&
        !identical(get_env(surveys[i])$longitude_dec, NULL)) {

      if (is.null(survey_forms_with_coordinates)) {
        survey_forms_with_coordinates <- surveys[i]} else
        {survey_forms_with_coordinates <- c(survey_forms_with_coordinates,
                                            surveys[i])
        }
      }
    }
  }


  # Create vector with unique plots in all surveys

coordinates_full <- get_env(survey_forms_with_coordinates[1]) %>%
  select(plot_id, longitude_dec, latitude_dec)

if (length(survey_forms_with_coordinates) > 1) {
  for (i in 2:length(survey_forms_with_coordinates)) {
    coordinates_full_i <- get_env(survey_forms_with_coordinates[i]) %>%
      select(plot_id, longitude_dec, latitude_dec)

    coordinates_full <- rbind(coordinates_full,
                              coordinates_full_i)
    }
}

coordinates_full <- coordinates_full %>%
  filter(!is.na(longitude_dec)) %>%
  filter(!is.na(latitude_dec))

coordinates <- distinct(coordinates_full, plot_id, .keep_all = TRUE)
coordinates$longitude_dec <- NA
coordinates$latitude_dec <- NA

for (j in seq_len(nrow(coordinates))) {

  # Determine the row indices of the given plot_id in coordinates_full

  vec <- which(coordinates$plot_id[j] == coordinates_full$plot_id)

  # Store all coordinates which are mentioned for the given plot_id (per Level)

  if (!identical(vec, integer(0))) {

    list_latitude <- coordinates_full$latitude_dec[vec]
    list_latitude <- list_latitude[!is.na(list_latitude)]

    list_longitude <- coordinates_full$longitude_dec[vec]
    list_longitude <- list_longitude[!is.na(list_longitude)]

    # Store the most abundant coordinate

    if (!identical(list_latitude, logical(0))) {
    coordinates$latitude_dec[j] <-
      as.numeric(list_latitude[which.max(table(list_latitude))])
    }

    if (!identical(list_longitude, logical(0))) {
    coordinates$longitude_dec[j] <-
      as.numeric(list_longitude[which.max(table(list_longitude))])
    }
  }
  }

coordinates <- coordinates %>%
  filter(!is.na(latitude_dec)) %>%
  filter(!is.na(longitude_dec))


# Save remaining dataframes to the global environment if permitted

if (save_to_env == TRUE) {

  source("./src/functions/assign_env.R")

  if (exists("data_availability", envir = environment())) {
    assign_env(paste0("data_availability_", code_survey), data_availability)
  }

assign_env(paste0("coordinates_", code_survey), coordinates)

assign_env("d_country", d_country)

assign_env("d_partner", d_partner)

}
}
