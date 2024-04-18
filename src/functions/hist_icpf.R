
hist_icpf <- function(data,
                      plot_title = NA) {

  # Initiate functions ----

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("boot"),
            require("ggdist"),
            require("glue"),
            require("plotly"),
            require("ggtext"))


  if (is.vector(data)) {
    dataset <- as.data.frame(data)
    # Column is named "data"
  }

  if (any(is.na(dataset$data))) {
    dataset <- dataset %>%
      filter(!is.na(data))
  }

  mean_data <- mean(dataset$data, na.rm = TRUE)
  median_data <- median(dataset$data, na.rm = TRUE)
  n_data <- length(dataset$data)

  data <- dataset$data

  # Define the interval widths
  interval_widths <- c(0.5, 0.8, 0.95)
  interval_widths <- c(
    (1 - interval_widths) * 0.5,
    1 - ((1 - interval_widths) * 0.5))

  # Calculate the interval borders
  quantile_borders <- quantile(dataset$data,
                               probs = interval_widths)


  bg_color <- "grey97"


  density_data <- data.frame(
    x = ggdist::density_bounded(dataset$data)$x,
    y = ggdist::density_bounded(dataset$data)$y *
      0.9 / max(ggdist::density_bounded(dataset$data)$y))




  p <- ggplot() +
    stat_halfeye(data = dataset,
                 aes(x = data),
                 fill_type = "segments", alpha = 0.4,
                 fill = "#556400") +
    stat_interval(data = dataset,
                  aes(x = data)) +
    annotate(geom = "point", col = "white",
             x = median_data, y = 0) +
    geom_vline(xintercept = mean_data,
               col = "grey30",
               size = 1,
               lty = "dashed") +
    coord_cartesian(ylim = c(-0.2, 1)) +
    annotate("richtext", x = mean_data + 10, y = 0.9, label = "Mean",
             label.size = 0, size = 3, hjust = 0) +
    scale_color_viridis_d(option = "mako",
                          direction = -1,
                          begin = 0.3, # darkest
                          end = 0.9) +
    guides(col = "none") +
    labs(
      title = plot_title,
      subtitle = paste0("(n = ", n_data, ")"),
      caption = NA,
      x = plot_title,
      y = NULL) +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = NA, fill = bg_color),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = "grey75"),
      plot.title = element_markdown(size = 10),
      plot.title.position = "plot",
      plot.subtitle = element_textbox_simple(
        margin = margin(t = 4, b = 16), size = 10),
      plot.caption = element_textbox_simple(
        margin = margin(t = 12), size = 7),
      plot.caption.position = "plot",
      axis.title = element_markdown(size = 10,
                                    hjust = 1),
      axis.text.x = element_text(size = 10,
                                 colour = "black",
                                 margin = margin(t = 5,
                                                 b = 3)),
      axis.text.y = element_blank(),
      plot.margin = margin(4, 4, 4, 4)
    ) +
    annotate(
      "richtext",
      x = c(mean(c(quantile_borders[1], median_data)),
            mean(c(quantile_borders[5], quantile_borders[6])),
            mean(c(quantile_borders[4], quantile_borders[5],
                   quantile_borders[4])),
            median_data
            # mean(c(quantile_borders[2], median_data))
            ),
      y = c(-0.1, 0.2, -0.1, 0.2
            #0.3
            ),
      label = c("50 % of data<br>fall within this range",
                "95 % of<br>data",
                "80 % of<br>data",
                "Median"
                # "Distribution<br>of data"
                ),
      label.size = 0, size = 3, vjust = 1) +
    geom_curve(
      data = data.frame(
        x = c(mean(c(quantile_borders[1], median_data)),
              mean(c(quantile_borders[5], quantile_borders[6])),
              mean(c(quantile_borders[4], quantile_borders[5],
                     quantile_borders[4])),
              median_data
              # mean(c(quantile_borders[2], median_data)) - 30
              ),
        xend = c(mean(c(quantile_borders[1], median_data)),
                 mean(c(quantile_borders[5], quantile_borders[6])),
                 mean(c(quantile_borders[4], quantile_borders[5])),
                 median_data
                 # mean(c(quantile_borders[2], median_data)) - 100
                 ),
        y = c(-0.1, 0.09, -0.1, 0.13
              # 0.24
              ),
        yend = c(-0.03, 0.04, -0.03, 0.04
                 # 0.3
                 )),
      aes(x = x, xend = xend, y = y, yend = yend),
      stat = "unique", curvature = 0.2, size = 0.4, color = "grey12",
      arrow = arrow(angle = 20, length = unit(1, "mm"))
    ) +
    geomtextpath::geom_textline(data = density_data,
                                aes(x = x, y = y),
                                label = "Distribution of data",
                                size = 3,
                                vjust = -0.5,
                                hjust = 0.15,
                                linecolor = NA)

  p

}
