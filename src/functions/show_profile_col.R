

show_profile_col <- function(data_frame,
                             survey_form,
                             plot_id,
                             survey_year,
                             profile_pit_id,
                             save = FALSE) {

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

  assertthat::assert_that("layer_number" %in% names(df))

  prof <- df %>%
    filter(plot_id == plot_id_input) %>%
    filter(survey_year == survey_year_input) %>%
    filter(profile_pit_id == profile_pit_id_input) %>%
    filter(!is.na(layer_number)) %>%
    arrange(layer_number) %>%
    group_by(profile_id) %>%
    mutate(top_prof = min(top)) %>%
    ungroup %>%
    mutate(top = top - top_prof,
           bottom = bottom - top_prof) %>%
    mutate(colour_moist_hex = coalesce(colour_moist_hex,
                                       "#4B392DFF")) %>%
    as.data.frame


  # Upgrade to SoilProfileCollection (package "aqp"). Use:
  # - the name of the column containing the profile ID
  # - the name of the column containing horizon upper boundaries
  # - the name of the column containing horizon lower boundaries

  depths(prof) <-
    profile_id ~ top + bottom

  # Register the horizon designation column

  # hzdesgnname(prof) <- "code_layer"
  #
  # plotSPC(prof,
  #         color = "colour_moist_hex",
  #         name.style = "left-center",
  #         depth.axis = list(line = -4, cex = 1))

  hzdesgnname(prof) <- "horizon_master_orig"

  if (save == FALSE) {

    par(mar = c(0, 0, 0, 0))

    plotSPC(prof,
            color = "colour_moist_hex",
            name.style = "right-center",
            print.id = FALSE,
            divide.hz = FALSE, # remove lines between colours
            max.depth = 100 - unique(prof$top_prof), # don't go deeper than 100 cm
            y.offset = unique(prof$top_prof),
            cex.names = 1,
            width = 0.05,
            depth.axis = FALSE)
    # depth.axis = list(line = -6, cex = 0.7, interval = 10))

    addVolumeFraction(prof,
                      colname = "coarse_fragment_vol_converted",
                      col = "black")

    par(cex.axis = 1)  # Set the font size of axis labels to 0.7

    axis(side = 2,
         at = c(seq(-10 * ceiling(-0.1 * unique(prof$top_prof)),
                    0, by = 10),
                seq(10, 100, by = 10)),
         line = -12, las = 1)

  } else {

  name_path <- paste0("./output/other_graphs/profile_",
                      survey_form, "_",
                      unique(prof$profile_id), ".png")
  png(name_path,
      res = 100,
      bg = "white")

  par(mar = c(0, 0, 0, 0))

  plotSPC(prof,
          color = "colour_moist_hex",
          name.style = "right-center",
          print.id = FALSE,
          divide.hz = FALSE, # remove lines between colours
          max.depth = 100 - unique(prof$top_prof), # don't go deeper than 100 cm
          y.offset = unique(prof$top_prof),
          cex.names = 1,
          width = 0.05,
          depth.axis = FALSE)
          # depth.axis = list(line = -6, cex = 0.7, interval = 10))

  addVolumeFraction(prof,
                    colname = "coarse_fragment_vol_converted",
                    col = "black")

  par(cex.axis = 1)  # Set the font size of axis labels to 0.7

  axis(side = 2,
       at = c(seq(-10 * ceiling(-0.1 * unique(prof$top_prof)),
                  0, by = 10),
              seq(10, 100, by = 10)),
       line = -12, las = 1)

  dev.off()

  cat(paste0("\nSaved as '", name_path, "'"))

  }

}
