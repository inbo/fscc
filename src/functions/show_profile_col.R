

show_profile_col <- function(data_frame,
                             plot_id,
                             survey_year,
                             profile_pit_id) {

  # This function is helpful for the assessment of horizon_masters
  # in "pfh" survey form records where this information is missing

  # Define required packages
  stopifnot(require("sf"),
            require("tidyverse"),
            require("openxlsx"),
            require("parsedate"),
            require("googlesheets4"),
            require("googledrive"),
            require("assertthat"),
            require("aqp"))

  assertthat::assert_that("colour_moist_hex" %in% names(data_frame))

  df <- data_frame
  plot_id_input <- plot_id
  survey_year_input <- survey_year
  profile_pit_id_input <- profile_pit_id


  if ("horizon_limit_up" %in% names(df) &&
      "horizon_limit_low" %in% names(df)) {

    df <- df %>%
      rename(top = horizon_limit_up) %>%
      rename(bottom = horizon_limit_low)
  }

  if ("unique_survey_profile" %in% names(df)) {

    df <- df %>%
      rename(profile_id = unique_survey_profile)
  }


  if ("repetition" %in% names(df)) {

    df <- df %>%
      rename(profile_pit_id = repetition)
  }

  if ("horizon_master" %in% names(df)) {

    df <- df %>%
      rename(code_layer = horizon_master)
  }


  prof <- df %>%
    filter(plot_id == plot_id_input) %>%
    filter(survey_year == survey_year_input) %>%
    filter(profile_pit_id == profile_pit_id_input)


  # Upgrade to SoilProfileCollection (package "aqp"). Use:
  # - the name of the column containing the profile ID
  # - the name of the column containing horizon upper boundaries
  # - the name of the column containing horizon lower boundaries

  depths(prof) <-
    profile_id ~ top + bottom

  # Register the horizon designation column

  hzdesgnname(prof) <- "code_layer"

  plotSPC(prof,
          color = "colour_moist_hex",
          name.style = "left-center",
          depth.axis = list(line = -4, cex = 1))



}
