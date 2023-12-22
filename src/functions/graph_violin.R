

graph_violin <- function(data_frame,
                         data_frame_2 = NULL,
                         violin_colour = "#949b38",
                         violin_2_colour = "#843860",
                         response,
                         group,
                         x_title = NULL,
                         title = NULL,
                         path_export = NULL) {

  # Initiate functions ----

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"),
            require("ggplot2"),
            require("boot"),
            require("rempsyc"),
            #require("INBOtheme"),
            require("ggtext"))

  assert_that(all(c(group, response) %in% names(data_frame)))

  source("./src/functions/as_character_summary.R")


  # This nice_violin code is directly based on the "rempsyc" package
  # https://github.com/rempsyc/rempsyc/tree/main

  nice_violin <- function(data,
                          data_2 = NULL,
                          scale = "count",
                          response,
                          group = NULL,
                          boot = FALSE,
                          bootstraps = 2000,
                          colours,
                          colours_2 = NULL,
                          xlabels = NULL,
                          ytitle = response,
                          xtitle = NULL,
                          has.ylabels = TRUE,
                          has.xlabels = TRUE,
                          comp1 = 1,
                          comp2 = 2,
                          signif_annotation = NULL,
                          signif_yposition = NULL,
                          signif_xmin = NULL,
                          signif_xmax = NULL,
                          ymin,
                          ymax,
                          yby = 1,
                          CIcap.width = 0.1,
                          obs = FALSE,
                          alpha = 1,
                          border.colour = "black",
                          border.colour_2 = NULL,
                          border.size = 2,
                          has.d = FALSE,
                          d.x = mean(c(comp1, comp2)) * 1.1,
                          d.y = mean(data[[response]]) * 1.3,
                          groups.order = "none",
                          xlabels.angle = 0) {

    check_col_names <- function(data, names) {
      missing.cols <- lapply(names, function(x) {
        x %in% names(data)
        # grep(x, names(data), invert = F)
      })
      if (length(missing.cols) > 0) {
        id <- which(!unlist(missing.cols))
        if (isTRUE(length(id) >= 1)) {
          missing.cols <- toString(names[id])
          stop(paste0("Variables not found: ", missing.cols,
                      ". Please double check spelling."))
        }
      }
    }

    theme_apa <- function(x, has.legend = FALSE) {
      x +
        ggplot2::theme_bw(base_size = 24) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          axis.text.x = ggplot2::element_text(colour = "black"),
          axis.text.y = ggplot2::element_text(colour = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(colour = "black"),
          axis.ticks = ggplot2::element_line(colour = "black")
        ) + {
          if (has.legend == FALSE) {
            ggplot2::theme(legend.position = "none")
          }
        }
    }

    # data ----

    check_col_names(data, c(group, response))
    rlang::check_installed(c("ggplot2"),
                           version = get_dep_version("ggplot2"),
                           reason = "for this function.")

    if (isTRUE(boot)) {
      rlang::check_installed(c("boot"), reason = "for this feature.")
    }

    if (is.null(group)) {
      group <- "All"
      data[[group]] <- group
    } else {
      data[[group]] <- as.factor(data[[group]])
    }

    data[[response]] <- as.numeric(data[[response]])

    # Duplicate groups with one observation

    groups_one_obs <- data %>%
      group_by(across(all_of(group))) %>%
      summarise(count = n()) %>%
      filter(count <= 1) %>%
      pull(!!group) %>%
      as.vector

    if (!identical(groups_one_obs, character(0))) {

    data_sub <-
      data[which(!data[[group]] %in% groups_one_obs), ]

    suppressWarnings({
    dataSummary <- rcompanion_groupwiseMean(
      group = group,
      var = response,
      data = data_sub,
      conf = 0.95,
      digits = 5,
      R = bootstraps,
      traditional = !boot,
      bca = boot,
      na.rm = TRUE
    )
    })

    } else {

      suppressWarnings({
        dataSummary <- rcompanion_groupwiseMean(
          group = group,
          var = response,
          data = data,
          conf = 0.95,
          digits = 5,
          R = bootstraps,
          traditional = !boot,
          bca = boot,
          na.rm = TRUE
        )
      })
    }

    if (!identical(groups_one_obs, character(0))) {
      for (i in seq_along(groups_one_obs)) {

        data_sub <-
          data[which(data[[group]] == groups_one_obs[i]), ]

        row_i <-
          data.frame(group = groups_one_obs[i],
                     n = nrow(data_sub),
                     Mean = pull(data_sub,
                                 !!response),
                     Conf.level = NA,
                     Bca.lower =
                       pull(data_sub,
                            !!response),
                     Bca.upper =
                       pull(data_sub,
                            !!response))

        names(row_i)[which(names(row_i) == "group")] <- group

        dataSummary <- rbind(
          dataSummary,
          row_i)
      }
    }

    if (groups.order == "increasing") {
      data[[group]] <- factor(
        data[[group]], levels = levels(data[[group]])[order(dataSummary$Mean)])
    } else if (groups.order == "decreasing") {
      data[[group]] <- factor(
        data[[group]], levels = levels(data[[group]])[order(dataSummary$Mean,
                                                            decreasing = TRUE)])
    } else if (groups.order == "string.length") {
      data[[group]] <- factor(
        data[[group]], levels = levels(data[[group]])[order(
          nchar(levels(data[[group]])))])
    }

    if (has.d == TRUE & any(
      !missing(comp1), !missing(comp2),
      !missing(signif_xmin)
    )) {
      if (missing(comp1) & missing(comp2) & !missing(signif_xmin)) {
        comp1.temp <- signif_xmin[1]
        comp2.temp <- signif_xmax[1]
      } else {
        comp1.temp <- comp1
        comp2.temp <- comp2
      }


      data.d <- data %>%
        dplyr::filter(UQ(dplyr::sym(group)) %in% levels(
          data[[group]]
        )[c(comp1.temp, comp2.temp)]) %>%
        droplevels()
      d <- round(effectsize::cohens_d(response,
                                      y = group,
                                      data = data.d
      )$Cohens_d, 2)
      d <- format_d(abs(d))
      d <- paste("=", d)
    }


    # data_2 ----

    if (!is.null(data_2)) {

    check_col_names(data_2, c(group, response))
    rlang::check_installed(c("ggplot2"),
                           version = get_dep_version("ggplot2"),
                           reason = "for this function.")

    if (isTRUE(boot)) {
      rlang::check_installed(c("boot"), reason = "for this feature.")
    }

    if (is.null(group)) {
      group <- "All"
      data_2[[group]] <- group
    } else {
      data_2[[group]] <- as.factor(data_2[[group]])
    }

    data_2[[response]] <- as.numeric(data_2[[response]])

    # Duplicate groups with one observation

    groups_one_obs <- data_2 %>%
      group_by(across(all_of(group))) %>%
      summarise(count = n()) %>%
      filter(count <= 1) %>%
      pull(!!group) %>%
      as.vector

    if (!identical(groups_one_obs, character(0))) {

      data_sub <-
        data_2[which(!data_2[[group]] %in% groups_one_obs), ]

      suppressWarnings({
        dataSummary_2 <- rcompanion_groupwiseMean(
          group = group,
          var = response,
          data = data_sub,
          conf = 0.95,
          digits = 5,
          R = bootstraps,
          traditional = !boot,
          bca = boot,
          na.rm = TRUE
        )
      })

    } else {

      suppressWarnings({
        dataSummary_2 <- rcompanion_groupwiseMean(
          group = group,
          var = response,
          data = data_2,
          conf = 0.95,
          digits = 5,
          R = bootstraps,
          traditional = !boot,
          bca = boot,
          na.rm = TRUE
        )
      })
    }

    if (!identical(groups_one_obs, character(0))) {
      for (i in seq_along(groups_one_obs)) {

        data_sub <-
          data[which(data_2[[group]] == groups_one_obs[i]), ]

        row_i <-
        data.frame(group = groups_one_obs[i],
                   n = nrow(data_sub),
                   Mean = pull(data_sub,
                               !!response),
                   Conf.level = NA,
                   Bca.lower =
                     pull(data_sub,
                          !!response),
                   Bca.upper =
                     pull(data_sub,
                          !!response))

        names(row_i)[which(names(row_i) == "group")] <- group

        dataSummary_2 <- rbind(
          dataSummary_2,
          row_i)

      }
    }

    if (!identical(dataSummary[[group]],
                   dataSummary_2[[group]])) {

      # Identify unique humus types in both data frames
      groups_union <-
        union(dataSummary[[group]], dataSummary_2[[group]])

      # Create data frames with the same humus types in the same sequence
      dataSummary <-
        data.frame(setNames(list(groups_union), group)) %>%
        left_join(dataSummary, by = group)

      dataSummary_2 <-
        data.frame(setNames(list(groups_union), group)) %>%
        left_join(dataSummary_2, by = group)
    }

    if (groups.order == "increasing") {
      data[[group]] <- factor(
        data[[group]], levels = levels(data[[group]])[order(dataSummary$Mean)])
    } else if (groups.order == "decreasing") {
      data[[group]] <- factor(
        data[[group]], levels = levels(data[[group]])[order(dataSummary$Mean,
                                                            decreasing = TRUE)])
    } else if (groups.order == "string.length") {
      data[[group]] <- factor(
        data[[group]], levels = levels(data[[group]])[order(
          nchar(levels(data[[group]])))])
    }


    if (groups.order == "increasing") {
      data_2[[group]] <- factor(
        data_2[[group]], levels = levels(data_2[[group]])[order(dataSummary_2$Mean)])
    } else if (groups.order == "decreasing") {
      data_2[[group]] <- factor(
        data_2[[group]], levels = levels(data_2[[group]])[order(dataSummary_2$Mean,
                                                            decreasing = TRUE)])
    } else if (groups.order == "string.length") {
      data_2[[group]] <- factor(
        data_2[[group]], levels = levels(data_2[[group]])[order(
          nchar(levels(data_2[[group]])))])
    }

    if (has.d == TRUE & any(
      !missing(comp1), !missing(comp2),
      !missing(signif_xmin)
    )) {
      if (missing(comp1) & missing(comp2) & !missing(signif_xmin)) {
        comp1.temp <- signif_xmin[1]
        comp2.temp <- signif_xmax[1]
      } else {
        comp1.temp <- comp1
        comp2.temp <- comp2
      }


      data_2.d <- data_2 %>%
        dplyr::filter(UQ(dplyr::sym(group)) %in% levels(
          data_2[[group]]
        )[c(comp1.temp, comp2.temp)]) %>%
        droplevels()
      d <- round(effectsize::cohens_d(response,
                                      y = group,
                                      data = data_2.d
      )$Cohens_d, 2)
      d <- format_d(abs(d))
      d <- paste("=", d)
    }

}


    # Plot ----

    suppressMessages({

    plot <- ggplot2::ggplot(data, ggplot2::aes(
      x = .data[[group]],
      y = .data[[response]],
      fill = .data[[group]]
    )) +
      # {
      #   if (!missing(colours)) {
      #     ggplot2::scale_fill_manual(values = colours)
      #   }
      # } +
      {
        if (!missing(xlabels)) {
          ggplot2::scale_x_discrete(labels = c(xlabels))
        }
      } +
      ggplot2::ylab(ytitle) +
      ggplot2::xlab(xtitle)
    })


    if (is.null(data_2)) {

      plot <- plot +
        ggplot2::geom_violin(color = border.colour,
                             alpha = alpha,
                             linewidth = border.size,
                             width = 1,
                             scale = scale,
                             fill = colours) +
        ggplot2::geom_point(ggplot2::aes(y = .data$Mean),
                            color = colorspace::darken(colours, 0.4),
                            size = 2,
                            data = dataSummary) +
        ggplot2::geom_errorbar(ggplot2::aes(
          y = .data$Mean,
          ymin = dataSummary[, 5],
          ymax = dataSummary[, 6]),
          color = colorspace::darken(colours, 0.4),
          linewidth = 1,
          width = CIcap.width,
          data = dataSummary)
    }

    if (!is.null(data_2)) {

      plot <- plot +
        ggplot2::geom_violin(position = position_nudge(-0.1),
                             color = border.colour,
                             alpha = alpha,
                             linewidth = border.size,
                             width = 1,
                             scale = scale,
                             fill = colours) +
        geom_violin(data = data_2,  # Specify the second dataset
                    aes(
                      x = .data[[group]],
                      y = .data[[response]],
                      fill = .data[[group]]
                    ),
                    position = position_nudge(0.1),
                    color = border.colour_2,
                    alpha = alpha,
                    linewidth = border.size,
                    width = 1,
                    scale = scale,
                    fill = colours_2) +
        ggplot2::geom_point(position = position_nudge(-0.1),
                            ggplot2::aes(y = .data$Mean),
                            color = colorspace::darken(colours, 0.4),
                            size = 2,
                            data = dataSummary) +
        ggplot2::geom_point(position = position_nudge(0.1),
                            ggplot2::aes(y = .data$Mean),
                            color = colorspace::darken(colours_2, 0.4),
                            size = 2,
                            data = dataSummary_2) +
        ggplot2::geom_errorbar(ggplot2::aes(
          y = .data$Mean,
          ymin = dataSummary[, 5],
          ymax = dataSummary[, 6]),
          position = position_nudge(-0.1),
          color = colorspace::darken(colours, 0.4),
          linewidth = 1,
          width = CIcap.width,
          data = dataSummary) +
        ggplot2::geom_errorbar(ggplot2::aes(
          y = .data$Mean,
          ymin = dataSummary_2[, 5],
          ymax = dataSummary_2[, 6]),
          position = position_nudge(0.1),
          color = colorspace::darken(colours_2, 0.4),
          linewidth = 1,
          width = CIcap.width,
          data = dataSummary)

    }



      # {
      #   if (!is.null(data_2)) {
      #     geom_violin(data = data_2,  # Specify the second dataset
      #                 aes(
      #                   x = .data[[group]],
      #                   y = .data[[response]],
      #                   fill = .data[[group]]
      #                 ),
      #                 color = border.colour_2,
      #                 alpha = alpha,
      #                 linewidth = border.size,
      #                 width = 1,
      #                 scale = scale,
      #                 fill = colours_2)
      #   }
      # } +
      # ggplot2::geom_point(ggplot2::aes(y = .data$Mean),
      #                     color = colorspace::darken(colours, 0.4),
      #                     size = 2,
      #                     data = dataSummary) +
      # ggplot2::geom_errorbar(ggplot2::aes(
      #   y = .data$Mean,
      #   ymin = dataSummary[, 5],
      #   ymax = dataSummary[, 6]),
      # color = colorspace::darken(colours, 0.4),
      # linewidth = 1,
      # width = CIcap.width,
      # data = dataSummary)


    plot <- theme_apa(plot) +
      {
        if (xlabels.angle != 0) {
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = xlabels.angle, size = 15, vjust = 1, hjust = 1))
        }
      }+
      {
        if (isTRUE(obs) || obs == "dotplot") {
          ggplot2::geom_dotplot(
            binaxis = "y",
            stackdir = "center",
            position = "dodge",
            color = NA,
            fill = "black",
            alpha = 0.3,
            dotsize = 0.5
          )
        } else if (obs == "jitter") {
          ggplot2::geom_jitter(
            alpha = 0.3,
            width = 0.25
          )
        }
      } +
      {
        if (has.ylabels == FALSE) {
          ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
          )
        }
      } +
      {
        if (has.xlabels == FALSE) {
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          )
        }
      } +
      {
        if (!missing(ymin)) {
          ggplot2::scale_y_continuous(
            limits = c(ymin, ymax), breaks = seq(ymin, ymax, by = yby)
          )
        }
      } +
      {
        if (!missing(comp1)) {
          rlang::check_installed("ggsignif", reason = "for this function.")
          ggsignif::geom_signif(
            comparisons = list(c(comp1, comp2)), test = "t.test",
            map_signif_level = TRUE, size = 1.3, textsize = 8
          )
        }
      } +
      {
        if (!missing(signif_annotation)) {
          rlang::check_installed("ggsignif", reason = "for this function.")
          ggsignif::geom_signif(
            annotation = signif_annotation, y_position = signif_yposition,
            xmin = signif_xmin, xmax = signif_xmax, size = 1.3, textsize = 8
          )
        }
      } +
      if (has.d == TRUE & any(
        !missing(comp1), !missing(comp2),
        !missing(signif_xmin)
      )) {
        ggplot2::annotate(
          geom = "text",
          x = d.x,
          y = d.y,
          label = sprintf("italic('d')~'%s'", d),
          parse = TRUE,
          hjust = 1,
          vjust = -1,
          size = 7
        )
      }
    plot
  }






  # Manipulate data as needed ----

  # Replace NAs in the grouping var by "Unknown"

  if (any(is.na(data_frame[[group]]))) {

    data_frame[[group]] <- ifelse(is.na(data_frame[[group]]),
                                  "Unknown",
                                  data_frame[[group]])
  }

  # Duplicate groups with one observation

  # groups_one_obs <- data_frame %>%
  #   group_by(across(all_of(group))) %>%
  #   summarise(count = n()) %>%
  #   filter(count <= 1) %>%
  #   pull(!!group) %>%
  #   as.vector

  # if (!identical(groups_one_obs, character(0))) {
  #
  #   cat(paste0(" \nAttention: the following groups have just ",
  #              "one observation:\n",
  #              as_character_summary(groups_one_obs), "\n"))
  #
  #   data_frame <- data_frame %>%
  #     filter(!get(group) %in% groups_one_obs)
  # }


  # Order the grouping column

  data_frame <- data_frame %>%
    mutate(group_unknown = ifelse(.data[[group]] != "Unknown",
                                   1,
                                   2)) %>%
    arrange(-group_unknown, desc(!!sym(group))) %>%
    select(-group_unknown) %>%
    mutate(group = fct_inorder(.data[[group]]))

  data_frame[[group]] <- fct_inorder(data_frame[[group]])




  if (!is.null(data_frame_2)) {

    # Replace NAs in the grouping var by "Unknown"

    if (any(is.na(data_frame_2[[group]]))) {

      data_frame_2[[group]] <- ifelse(is.na(data_frame_2[[group]]),
                                    "Unknown",
                                    data_frame_2[[group]])
    }

    # Order the grouping column

    data_frame_2 <- data_frame_2 %>%
      mutate(group_unknown = ifelse(.data[[group]] != "Unknown",
                                    1,
                                    2)) %>%
      arrange(-group_unknown, desc(!!sym(group))) %>%
      select(-group_unknown) %>%
      mutate(group = fct_inorder(.data[[group]]))

    data_frame_2[[group]] <- fct_inorder(data_frame_2[[group]])


    # Make sure the two datasets have the same levels

    # Create a union of unique levels
    all_levels <- union(levels(as.factor(data_frame[[group]])),
                        levels(as.factor(data_frame_2[[group]])))

    # Ensure both datasets have the same set of levels

    data_frame[[group]] <- factor(as.factor(data_frame[[group]]),
                                  levels = all_levels)
    data_frame_2[[group]] <- factor(as.factor(data_frame_2[[group]]),
                                  levels = all_levels)
  }


  # Specify titles ----

  if (is.null(x_title) && grepl("stock", response, ignore.case = TRUE)) {
    x_title <- paste0("**Forest soil carbon stock** <br>(t C ha<sup>-1</sup>)")
  }


  title_text <- glue::glue(
    paste0("**Carbon stocks** (topsoil) in ",
           "**<span style='color:{violin_colour}'>Level I</span>** and **",
           "<span style='color:{violin_2_colour}'>Level II</span>** <br>",
           "as a function of **",
           str_to_lower(str_replace_all(group, "_", " ")),
           "**")
  )

  if (is.null(data_frame_2)) {
    title_text <- glue::glue(
      paste0("**Carbon stocks** in **",
             "<span style='color:{violin_colour}'>Level II</span>** ",
             "as a function of **",
             str_to_lower(str_replace_all(group, "_", " ")),
             "**")
    )
  }

  # Calculate maximum x axis

  x_axis_max <- ifelse(max(data_frame[[response]]) > 750,
                       750,
                       max(data_frame[[response]]))

  # Make plot ----

  aspect_ratio <- length(unique(data_frame[[group]])) / 13 * 2


  suppressWarnings({

  nice_violin_plot <- nice_violin(
    data = data_frame,
    data_2 = data_frame_2,
    group = group,
    response = response,
    boot = TRUE,
    bootstraps = 4600,
    ytitle = x_title,
    colours = violin_colour,
    colours_2 = violin_2_colour,
    CIcap.width = 0,
    alpha = 0.5,
    border.colour = violin_colour,
    border.colour_2 = violin_2_colour,
    border.size = 1)


  # Convert the plot to horizontal
  nice_violin_plot +
    ggtitle(title_text) +
    scale_color_manual(values = c("Level I" = violin_colour,
                                  "Level II" = violin_2_colour)) +
    labs(y = x_title,
         x = "") +
    theme(plot.title = element_markdown(lineheight = 1.2,
                                        hjust = 0),
          legend.text = element_markdown(size = 10),
          plot.title.position = "plot",

          text = element_text(color = "black",
                              size = 10),
          axis.text = element_text(size = 10),
          axis.text.y = element_text(hjust = 0,
                                     vjust = 0.3,
                                     margin = margin(0,
                                                     10,
                                                     0,
                                                     0)),
          axis.text.x = element_text(margin = margin(10,
                                                     0,
                                                     0,
                                                     0)),
          # axis.title = element_text(hjust = 0,
          #                             face = "bold"),
          axis.title.x = element_markdown(hjust = 0,
                                          lineheight = 1.2),
          axis.title.y = element_blank(),
          axis.ticks.length = unit(-0.25, "cm"),
          axis.ticks.x = element_line(linewidth = 1),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(linewidth = 1),
          panel.grid.major.x = element_line(colour = "white"),
          plot.margin = margin(t = 0.5,  # Top margin
                               r = 0.5,  # Right margin
                               b = 0.5,  # Bottom margin
                               l = 0.5,  # Left margin
                               unit = "cm"),
          panel.background = element_rect(fill = "#D8E0E0"), #) +#,
          aspect.ratio = aspect_ratio) + #2  0.5) +
    coord_flip() +
    scale_y_continuous(limits = c(0, x_axis_max),
                       breaks = seq(0, x_axis_max,
                                    by = 100),
                       expand = expansion(0, 0)) +
    scale_x_discrete(expand = expansion(add = c(1, 1)))

  })


  # Export ----

  width <- (max(nchar(as.character(data_frame[[group]]))) +
              round(max(data_frame[[response]])/30))

  height <-
    10 * ((round(max(data_frame[[response]])/30)) * aspect_ratio) / width

  height <- ifelse(height > 9.7,
                   9.7,
                   height)


  ggsave(filename = paste0(as.character(format(Sys.Date(), "%Y%m%d")), "_",
                           response, "_",
                           group, ".png"),
         path = path_export,
         dpi = 500,
         height = height,
         width = 6.81)



  # ggsave(filename = paste0(as.character(format(Sys.Date(), "%Y%m%d")), "_",
  #                          response, "_",
  #                          group, "2.png"),
  #        path = path_export,
  #        dpi = 500,
  #        width = (max(nchar(as.character(data_frame[[group]]))) +
  #                   round(max(data_frame[[response]])/10)) / 10)
}
