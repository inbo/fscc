

graph_interval <- function(data,
                           response,
                           group,
                           aspect.ratio = NULL,
                           width = 6.81,
                           mode = "light",
                           version = "",
                           x_max = 100,
                           number_of_groups = NULL,
                           x_title = NULL,
                           title = NULL,
                           caption = NULL,
                           path_export = NULL,
                           return = FALSE) {

  if (mode == "dark") {
    col_panel <- "black"
    col_background <- "#2D2E2E"
    col_front <- "white"
  } else {
    col_panel <- "#D8E0E0"
    col_background <- "white"
    col_front <- "black"
  }


  if (is.null(path_export)) {
    path_export <- "./output/stocks/"
  }

  if (version != "") {
    version <- paste0("_", version)
  }

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"),
            require("ggplot2"),
            require("boot"),
            require("rempsyc"),
            require("ggtext"),
            require("ggdist"))

  assert_that(all(c(group, response) %in% names(data)))

  source("./src/functions/as_character_summary.R")

  data <- data %>%
    mutate(group = .data[[group]]) %>%
    mutate(response = .data[[response]]) %>%
    filter(!is.na(response)) %>%
    filter(!is.na(group) &
             group != "")

  if (!is.null(number_of_groups)) {

    assertthat::assert_that(is.numeric(number_of_groups) &
                              number_of_groups < n_distinct(data$group))

    groups_selected <- data %>%
      group_by(across(all_of(group))) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n = number_of_groups) %>%
      pull(.data[[group]])

    data <- data %>%
      filter(group %in% as.character(groups_selected))
  }


  data_count <- data %>%
    group_by(group) %>%
    reframe(count = n())

  data <- data %>%
    left_join(data_count, by = "group") %>%
    mutate(group = paste0("**", group, "** (", count, ")"))

  suppressWarnings({
    data_summary <- rcompanion_groupwiseMean(
      group = "group",
      var = "response",
      data = data,
      conf = 0.95,
      digits = 5,
      R = 2000,
      traditional = TRUE,
      bca = FALSE,
      na.rm = TRUE) %>%
      mutate(Trad.lower = ifelse(
        Trad.lower < 0, 0, Trad.lower))
  })

  data$response <- as.numeric(data$response)
  data$group <- as.factor(data$group)

  data$group <- factor(
    data$group, levels = levels(data$group)[order(data_summary$Mean,
                                                  decreasing = FALSE)])

  response_name <- case_when(
    response == "stock_forest_floor" ~ "(forest floor)",
    response == "stock" ~ "(forest floor + soil)",
    response == "stock_topsoil" ~ "(forest floor + topsoil)",
    .default = "")

  group_name <- case_when(
    group == "wrb_ref_soil_group" ~ "WRB Reference Soil Group",
    group == "eftc" ~ "European Forest Type Cat.",
    .default = str_to_lower(str_replace_all(group, "_", " ")))


  suppressWarnings({
    data_summary <- rcompanion_groupwiseMean(
      group = "group",
      var = "response",
      data = data,
      conf = 0.95,
      digits = 5,
      R = 2000,
      traditional = TRUE,
      bca = FALSE,
      na.rm = TRUE) %>%
      mutate(Trad.lower = ifelse(
        Trad.lower < 0, 0, Trad.lower))
  })



p <- ggplot() +
    stat_interval(data = data,
                  aes(x = response,
                      y = group),
                  .width = c(0.2, 0.5, 0.8, 0.9)) +
    geom_errorbar(data = data_summary %>%
                    mutate(type = "Mean + 95 % CI\n(bootstrapped)"),
                  aes(y = group,
                      xmin = Trad.lower,
                      xmax = Trad.upper,
                      linetype = type),
                  color = "white",
                  linewidth = 1,
                  width = 0) +
    geom_point(data = data_summary %>%
                 mutate(type = "Mean + 95 % CI\n(bootstrapped)"),
               aes(y = group,
                   x = Mean,
                   shape = type),
               color = "white", size = 1.2) +
    coord_cartesian(xlim = c(0, x_max),
                    expand = FALSE) +
    scale_color_viridis_d(option = "viridis",
                          direction = -1,
                          begin = 0.1, # darkest
                          end = 0.8,
                          labels = c("0.05 - 0.95", "0.10 - 0.90",
                                     "0.25 - 0.75", "0.40 - 0.60"),
                          name = "Quantile") +
    scale_linetype_manual(values = c("Mean + 95 % CI\n(bootstrapped)" =
                                       "solid"),
                          name = "") +
    scale_shape_manual(values = c("Mean + 95 % CI\n(bootstrapped)" = 16),
                       name = "") +
    labs(
      title = paste0("**Carbon stock** ",
                     response_name,
                     "<br>",
                     "as a function of **",
                     group_name, "**"),
      subtitle = paste0("(ICP Forests Level I and Level II)"),
      x = paste0("**Carbon stock** ",
                 "(mean over time)", " ", #"<br>",
                 "(t C ha<sup>-1</sup>)"),
      y = NULL,
      # "Mean + 95 % conf. int. mean (bootstrapped)"
      caption = caption) +
    theme(
      panel.background = element_rect(fill = col_panel),
      plot.background = element_rect(fill = col_background,
                                     colour = col_background),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = col_background, linewidth = 1),
      panel.grid.minor.x = element_line(color = col_background, linewidth = 1),
      plot.title = element_markdown(size = 10,
                                    colour = col_front,
                                    lineheight = 1.3,
                                    hjust = 0,
                                    margin = margin(b = 6)),
      plot.subtitle = element_markdown(size = 10,
                                    colour = col_front,
                                    lineheight = 1.3,
                                    hjust = 0,
                                    margin = margin(b = 10)),
      plot.caption = element_text(size = 10,
                                  face = "italic",
                                  colour = col_front),
      axis.title = element_markdown(size = 10,
                                    colour = col_front,
                                    hjust = 1,
                                    lineheight = 1.4),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_markdown(size = 10,
                                     lineheight = 1.1,
                                 colour = col_front,
                                 margin = margin(t = 8,
                                                 b = 5)),
      axis.text.y = element_markdown(size = 10,
                                 colour = col_front,
                                 margin = margin(r = 5)),
      aspect.ratio = aspect.ratio,
      legend.title = element_text(face = "bold",
                                  colour = col_front,
                                  size = 10),
      legend.key = element_rect(fill = NA),
      legend.background = element_rect(fill = col_background),
      legend.justification = c("left", "top"),
      legend.text = element_text(size = 10,
                                 colour = col_front,
                                 lineheight = 1.2),
      legend.spacing.y = unit(0.1, "cm"),
      legend.margin = margin(t = 0, b = 0, l = 14, r = 5),
      plot.margin = margin(t = 0.5,
                           r = 0.5,
                           b = 0.5,
                           l = 0.5,
                           unit = "cm"))


  ggsave(filename = paste0(response, "_",
                           group, version, ".png"),
         plot = p,
         path = path_export,
         dpi = 500,
         width = width)


if (return == TRUE) {
  return(p)
}



}
