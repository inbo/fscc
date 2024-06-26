
#' Fit spline and return predicted data per cm
#'
#' Fit a cm-specific mass-preserving spline (using mpspline2 function)
#' and extrapolate from 0 cm until a maximum soil depth
#'
#' Initiated by Bruno
#'
#' @param id Name of profile
#' @param depth_top Vector with layer top (in cm) for each horizon
#' @param depth_bottom Vector with layer bottom (in cm) for each horizon
#' @param variab Vector with data of variable to spline
#' @param max_soil_depth Maximum soil depth until which spline should be
#' extrapolated
#' @param graph Logical indicating whether a graph of the spline fitting
#' should be made for the given depth profile
#'
#' @details
#' Splines can only be fit to profiles with known variable data for at least
#' two layers, for which layer limits are known and from which one is the
#' upper below-ground layer
#'
#' @return spline_output
#' @export
#'
#' @examples

soilspline <- function(id,
                       depth_top,
                       depth_bottom,
                       variab,
                       variab_min = NULL,
                       variab_max = NULL,
                       variab_source = NULL,
                       max_soil_depth,
                       survey_form = NULL,
                       graph = TRUE) {

  # Prepare packages

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"),
            require("ggtext"),
            require("mpspline2"))

  # Set directory where graphs should be saved

  path <- paste0("./output/stocks/",
                 as.character(format(Sys.Date(), "%Y%m%d")), "_",
                 as.character(format(as.Date(
                   get_date_local(path = "./data/raw_data/")),
                   format = "%Y%m%d")),
                 "_carbon_stocks/", survey_form,
                 "_splines_per_profile/")

  assertthat::assert_that(
    dir.exists(path))

  # Create directory if it doesn't exist

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Assert that input data are in the correct format ----

  assertthat::assert_that(all(!is.na(depth_top)) &&
                            all(!is.na(depth_bottom)) &&
                            all(!is.na(variab)) &&
                            !is.na(max_soil_depth))

  assertthat::assert_that(length(depth_top) == length(depth_bottom) &&
                            length(depth_top) == length(variab) &&
                            (length(depth_top) >= 2 ||
                               (length(depth_top) == 1 &&
                                  max(depth_bottom) >= 0.8 *
                                  max_soil_depth)))

  assertthat::assert_that(min(depth_top) == 0)



  # Fit a spline using mpspline function (mpspline2 package) ----
  # The max depth will be the bottom layer limit of the lowest layer)

  # Information about mpspline_one from GitHub:

  # This function implements the mass-preserving spline method of
  # Bishop et al (1999) for interpolating between measured soil attributes
  # down a single soil profile.
  # - site: data frame containing data for a single soil profile.
  #         * Column 1 must contain site identifiers.
  #         * Columns 2 and 3 must contain upper and lower sample depths,
  #           respectively, measured in centimeters.
  #         * Subsequent columns will contain measured values for those depths.
  # - var_name: length-1 character or length-1 integer denoting the column in
  #             "site" in which target data is stored. If not supplied, the
  #             fourth column of the input object is assumed to contain the
  #             target data.
  # - lam: smoothing parameter for spline. Defaults to 0.1.
  # - d: sequential integer vector; denotes the output depth ranges in cm.
  #      Defaults to c(0, 5, 15, 30, 60, 100, 200) after the GlobalSoilMap
  #      specification, giving output predictions over intervals 0-5 cm,
  #      5-15 cm, etc.
  # - vlow: numeric; constrains the minimum predicted value to a realistic
  #         number. Defaults to 0.
  # - vhigh: numeric; constrains the maximum predicted value to a realistic
  #          number. Defaults to 1000.

  # Returns a list with the following elements:
  # - Site ID,
  # - vector of predicted values over input intervals
  # - vector of predicted values for each cm down the profile to max(d)
  # - vector of predicted values over d (output) intervals
  # - root mean squared error (RMSE) +
  #   the interquartile range of the root mean squared error (RMSE_IQR)


  ## Mean ----

  # Prepare input for mpspline_one

  prof_input <-
    data.frame(id,
               depth_top,
               depth_bottom,
               variab)

  if (nrow(prof_input) == 1) {

    # Spline with one layer (e.g. in very shallow profile)

    suppressMessages({

      mpspline_output <- mpspline_one(site = prof_input,
                                      var_name = "variab",
                                      lam = 0.1,
                                      # constrains the maximum predicted value
                                      # to a realistic number. Defaults
                                      # to 1000:
                                      vhigh = 2600)
    })
  } else {

    mpspline_output <- mpspline_one(site = prof_input,
                                    var_name = "variab",
                                    lam = 0.1,
                                    # constrains the maximum predicted value
                                    # to a realistic number. Defaults
                                    # to 1000:
                                    vhigh = 2600)
  }



  # Extrapolate the mass-preserving spline output to depth max_soil_depth ----
  # (i.e. effective soil depth; max 100 cm)
  # using the spline function (stats package)

  x <- mpspline_output$est_1cm[seq_len(max(depth_bottom))]

  spline_output <-
    spline(x = x,
           # y should not be negative (e.g. not c(-1, -2, -3...))
           y = NULL,
           # Natural splines
           method = "natural",
           # "xout" is an optional set of values specifying where
           # interpolation is to take place
           # This enables extrapolation beyond the lower boundary of
           # the observations
           xout = seq_len(max_soil_depth))

  # Predicted and extrapolated variab
  spline_output <- spline_output$y

  # Output variable should not be below 0
  spline_output <- ifelse(spline_output < 0,
                          0,
                          spline_output)


  if (!is.null(variab_min) &&
      all(!is.na(variab_min)) &&
      !is.null(variab_max) &&
      all(!is.na(variab_max))) {

    ## Min ----

    if (nrow(prof_input) == 1) {

      suppressMessages({

        mpspline_output_min <- mpspline_one(site = data.frame(id,
                                                              depth_top,
                                                              depth_bottom,
                                                              variab_min),
                                            var_name = "variab_min",
                                            lam = 0.1,
                                            vhigh = 2600)
      })
    } else {

      mpspline_output_min <- mpspline_one(site = data.frame(id,
                                                            depth_top,
                                                            depth_bottom,
                                                            variab_min),
                                          var_name = "variab_min",
                                          lam = 0.1,
                                          vhigh = 2600)
    }


    spline_output_min <-
      spline(x = mpspline_output_min$est_1cm[seq_len(max(depth_bottom))],
             y = NULL,
             method = "natural",
             xout = seq_len(max_soil_depth))$y

    spline_output_min <- ifelse(spline_output_min < 0,
                                0,
                                spline_output_min)

    ## Max ----

    if (nrow(prof_input) == 1) {

      suppressMessages({

        mpspline_output_max <- mpspline_one(site = data.frame(id,
                                                              depth_top,
                                                              depth_bottom,
                                                              variab_max),
                                            var_name = "variab_max",
                                            lam = 0.1,
                                            vhigh = 2600)
      })
    } else {

      mpspline_output_max <- mpspline_one(site = data.frame(id,
                                                            depth_top,
                                                            depth_bottom,
                                                            variab_max),
                                          var_name = "variab_max",
                                          lam = 0.1,
                                          vhigh = 2600)
    }


    spline_output_max <-
      spline(x = mpspline_output_max$est_1cm[seq_len(max(depth_bottom))],
             y = NULL,
             method = "natural",
             xout = seq_len(max_soil_depth))$y

    spline_output_max <- ifelse(spline_output_max < 0,
                                0,
                                spline_output_max)

    # Create a list to store multiple results
    result <- list(
      spline_output = spline_output,
      spline_output_min = spline_output_min,
      spline_output_max = spline_output_max,
      rmse_mpspline = mpspline_output$est_err[1])


  } else {

    # No min and max

    # Create a list to store multiple results ----
    result <- list(
      spline_output = spline_output,
      rmse_mpspline = mpspline_output$est_err[1])
  }



  # Make a graph ----

  if (graph == TRUE) {

    if (!is.null(variab_min) &&
        all(!is.na(variab_min)) &&
        !is.null(variab_max) &&
        all(!is.na(variab_max))) {

      splines <- data.frame(depth = seq_len(max_soil_depth),
                            spline_avg = spline_output,
                            spline_min = spline_output_min,
                            spline_max = spline_output_max)
      } else {

        # No max and min
        splines <- data.frame(depth = seq_len(max_soil_depth),
                              spline_avg = spline_output)
      }


      if (!is.null(variab_source)) {

        prof_data <-
          data.frame(depth_top,
                     depth_bottom,
                     variab,
                     variab_source)

      } else {

        prof_data <-
          data.frame(depth_top,
                     depth_bottom,
                     variab) %>%
          mutate(variab_source = NA)
      }

      prof_data <- prof_data %>%
        mutate(variab_source = case_when(
          variab_source == "rule: fixed value below 80 cm" ~
            "Gap-filled\n(fixed value > 80 cm)",
          variab_source == "rule: constant below 40 cm" ~
            "Gap-filled\n(constant C > 40 cm)",
          TRUE ~ "Layer 1+")) %>%
        mutate(variab_source =
                 factor(variab_source,
                        levels = c("Layer 1+",
                                   "Gap-filled\n(constant C > 40 cm)",
                                   "Gap-filled\n(fixed value > 80 cm)")))

      # Create the plot
      p <- ggplot() +
        geom_rect(data = prof_data,
                  aes(ymin = 0,
                      ymax = variab,
                      xmin = (-1) * depth_bottom,
                      xmax = (-1) * depth_top,
                      fill = variab_source),
                  color = NA,
                  linewidth = 1)

      if (!is.null(variab_min) &&
          all(!is.na(variab_min)) &&
          !is.null(variab_max) &&
          all(!is.na(variab_max))) {

      p <- p +
        geom_ribbon(data = splines,
                    aes(ymin = spline_min,
                        ymax = spline_max,
                        x = (-1) * depth),
                    color = NA,
                    fill = "#843860",
                    alpha = 0.4)

      }

      p <- p +
        geom_line(data = splines,
                  aes(y = spline_avg,
                      x = (-1) * depth),
                  color = "#843860",
                  linewidth = 1) +
        scale_fill_manual(
          values = c("Layer 1+" = "#AAAAAA",
                     "Gap-filled\n(constant C > 40 cm)" = "#BEBEBE",
                     "Gap-filled\n(fixed value > 80 cm)" = "#D2D2D2")) +
        labs(y = "**C density**<br>(t ha<sup>-1</sup> cm<sup>-1</sup>)",
             x = "**Depth**<br>(cm)",
             fill = NULL,
             title = unique(prof$profile_id)) +
        coord_flip() +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(breaks = unique(c(seq(0, (-1) * max(splines$depth),
                                                 by = -20),
                                             (-1) * max_soil_depth)),
                           expand = c(0, 0)) +
        theme(text = element_text(color = "black",
                                  size = 10),
              plot.title = element_text(color = "black",
                                        size = 10,
                                        face = "bold",
                                        margin = margin(b = 10)),
              panel.background = element_blank(),
              axis.text = element_text(size = 10, colour = "black"),
              axis.text.y = element_text(hjust = 1,
                                         vjust = 0.5,
                                         margin = margin(0,
                                                         5,
                                                         0,
                                                         0)),
              axis.text.x = element_text(margin = margin(8,
                                                         0,
                                                         0,
                                                         0)),
              axis.title.x = element_markdown(hjust = 1.01,
                                              lineheight = 1.4,
                                              colour = "black",
                                              margin = margin(t = 8,
                                                              r = 5)),
              axis.title.y = element_markdown(angle = 0,
                                              hjust = 0,
                                              vjust = 1.02,
                                              lineheight = 1.4,
                                              colour = "black",
                                              margin = margin(r = 5)),
              legend.text = element_text(lineheight = 1.2),
              axis.ticks.length = unit(-0.2, "cm"),
              axis.ticks.x = element_line(linewidth = 0.7, colour = "black"),
              axis.ticks.y = element_line(linewidth = 0.7, colour = "black"),
              axis.line.y = element_line(linewidth = 0.7, colour = "black"),
              axis.line.x = element_line(linewidth = 0.7, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = margin(t = 0.5,  # Top margin
                                   r = 0.5,  # Right margin
                                   b = 0.5,  # Bottom margin
                                   l = 0.5,  # Left margin
                                   unit = "cm"),
              aspect.ratio = 1)

    ggsave(filename = paste0(id, ".png"),
           path = path,
           plot = p,
           dpi = 500,
           height = 4,
           width = 6.81)

  } # End of "if graph"

  # Return splined variable for each cm until the maximum soil depth
  return(result)
}


