
#' Generate and save an ICP Forests map
#'
#' This function generates an ICP Forests map based on provided layers,
#' titles, legends, and other parameters, with biogeographical regions in
#' the background.
#'
#' @param layers A character vector specifying the names of the spatial layers
#' (sf data frames in the global environment) to be used in the map.
#' @param title A character string specifying the title of the overall map.
#' @param legend_title A character string specifying the title of the legend
#' (which refers to the layers in the 'layers' argument).
#' @param legend_classes A character vector specifying the names of the legend
#' classes (in the same order as the layers). If NULL, a map in which the
#' colours represent the continuous variable specified under
#' "variable_continuous" is made.
#' @param variable_continuous A character string specifying the name of the
#' column with the continuous variable (represented by a continuous colour
#' scale). Only activated if legend_classes is NULL. Default is NULL.
#' @param export_name A character string specifying the name of the exported
#' map file (exported as .png).
#' @param export_folder A character string specifying the name of the export
#' subfolder (i.e. subfolder of './output/plots/').
#' @param point_size A numeric value specifying the size of the points on the
#' map. Recommendation: 0.1 for Level I; 0.6 for Level II.
#' @param point_col A character vector specifying the colour names or hex codes
#' of the points of the layers (in the 'layers' argument) on the map, in the
#' same order as the layers. Recommendation: c("orange", "#e95198", "red")
#' @param biogeo_palette A character value specifying the colour palette to be
#' used for the biogeographical regions layer. The default is "biogeo_col",
#' a custom colour palette in mostly green-blue hues. Another option is
#' "viridis". NULL means that the background does not show biogeographical
#' regions.
#' @param inset_maps_offset_x A positive numeric value specifying the horizontal
#' offset of the small inset maps of the Canary Islands, Azores and Cyprus
#' (depending on the width of the legend, the x location of these inset maps
#' can change)
#'
#'
#' @return None
#'
#' @import sf
#' @import tidyverse
#' @import grid
#' @import cowplot
#' @import ggspatial
#'
#' @details
#' By default, this function plots a map of Europe with small inset maps for the
#' Azores, Canary Islands and Cyprus. The map shows biogeographical regions
#' as well as country borders. On top, it shows the
#' data layers in the 'layers' argument categorically as points.
#'
#' Note that this map is quite sensitive to changes in dimensions, since the
#' location of the small inset maps is relative to the whole map (including the
#' surrounding white space) rather than to the main map panel. As such, it is
#' recommended for the legend title and legend classes to occupy less
#' horizontal space than the title 'Biogeographical region'. If needed, the
#' legend title and classes can be split in multiple rows using "\n".
#'
#' References:
#' - European Environment Agency (2016). Biogeographical regions shapefile.
#'   Retrieved from
#'   https://www.eea.europa.eu/data-and-maps/data/biogeographical-regions-europe-3
#' - Hijmans, R. J., Barbosa, M., Ghosh, A., Mandel, A. (2023). geodata:
#'   Functions for downloading of geographic data for use in spatial analysis
#'   and mapping. R package version 0.4.11.
#'
#' @examples
#' map_icpf(layers = c("df_fao_but_no_wrb_spatial", "df_wrb_spatial"),
#'          title = "Level I",
#'          legend_title = "Soil classification\nsystem",
#'          legend_classes = c("FAO88", "WRB"),
#'          export_name = "map_LI_soil_classification",
#'          export_folder = "20230323_maps_ecocluster_classification",
#'          point_col = c("orange", "red"),
#'          point_size = 0.1)


map_icpf <- function(layers,
                     title,
                     legend_title,
                     legend_classes,
                     variable_continuous = NULL,
                     variable_cat = NULL,
                     export_name,
                     export_folder,
                     point_size = 0.6,
                     point_col = NULL,
                     biogeo_palette = "biogeo_col",
                     mode = "dark_light",
                     with_logo = FALSE,
                     count_plots_legend = FALSE,
                     return = FALSE,
                     inset_maps_offset_x = 0,
                     width = 6.81) {



  if (mode == "dark") {

    # Dark mode

    col_panel <- "black"
    col_sea <- "#677D83" #"#727878"
    col_background <- "#2D2E2E"
    col_background_plot <- col_background
    col_front <- "white"

  } else if (mode == "dark_light") {

    col_panel <- "white"
    col_sea <- "#D8E0E0"
    col_background <- "#2D2E2E"
    col_background_plot <- "white"
    col_front <- "black"

  } else {

    # Light mode

    col_panel <- "white"
    col_sea <- "#D8E0E0"
    col_background <- "white"
    col_background_plot <- "white"
    col_front <- "black"
  }


  # Load packages ----

  stopifnot(require("sf"),
            require("tidyverse"),
            require("grid"),
            require("magick"),      # To place a logo on top
            require("cowplot"),     # To put maps together
            require("ggspatial"),   # For scale bar and north arrow
            require("geodata"),     # To download country borders
            require("ggtext"))      # Markdown text


  str_wrap_br <- function(text, width = 50) {
    wrapped_text <- stringr::str_wrap(text, width = width, exdent = 10)
    stringr::str_replace_all(wrapped_text, "\n", "<br>")
  }

  # Import biogeographical regions layer ----
  # if this does not exist yet in the global environment, or if this exists
  # in the wrong class

  if (!is.null(biogeo_palette)) {

  if (!exists("biogeo_sf") ||
      (exists("biogeo_sf") &&
       any(!c("sf", "data.frame") %in% class(biogeo_sf)))) {

  # Import the shapefile with biogeographical regions
  biogeo_sf <-
    read_sf(paste0("./data/additional_data/shapefiles/",
                   "BiogeoRegions2016.shp"))

  biogeo_sf <- biogeo_sf %>%
    # Reduce the file size
    st_simplify(dTolerance = 1000) %>%
    # Remove category "outside"
    filter(.data$short_name != "outside") %>%
    # Rename category "Black Sea"
    mutate(code = if_else(.data$short_name == "blackSea",
                          "Black Sea", .data$code)) %>%
    filter(code != "Anatolian")

  # Check the file size
  object.size(biogeo_sf) %>% format(units = "MB")

  # Verify the coordinate reference system, which should be:
  # ETRS89-extended / LAEA Europe
  # EPSG: 3035
  st_crs(biogeo_sf) # already in ETRS89-extended/LAEA Europe

  # Save this sf layer to the global environment so that this step does not
  # always have to be repeated
  assign("biogeo_sf", biogeo_sf, envir = globalenv())
  }
}




  # Import country borders ----
  # if this does not exist yet in the global environment, or if this exists
  # in the wrong class

  if (!exists("world_spat") ||
      (exists("world_spat") &&
       any(class(world_spat) != c("sf", "data.frame")))) {

  # Import country borders with resolution 1 (highest resolution)
  world_spat <- geodata::world(resolution = 1,
                               level = 0,
                               path = tempdir()) %>%
    st_as_sf() %>%
    # Transform to the same coordinate reference system
    st_transform(crs = 3035)

  # Check the file size
  object.size(world_spat) %>% format(units = "MB")
  }





  # Define colours ----

  country_border_col <- "black"

  # Number of categories

  if (!is.null(variable_cat)) {

    n_cat <- get(layers[1], envir = .GlobalEnv) %>%
      pull(!!sym(variable_cat)) %>%
      n_distinct
  }

  if (is.null(point_col)) {

    if (!is.null(biogeo_palette)) {

       point_col <- c("#b34646",
                      "#F8C4B4",
                      "#780116",
                      "#FF0000")

       point_col <- point_col[seq_len(length(layers))]

       if (#grepl("stock", variable_cat) &&
         grepl("cat", variable_cat)) {

         point_col <- colorRampPalette(c("white", "black"))(n_cat + 2)
         point_col <- viridisLite::inferno(n_cat + 2, direction = -1)
         point_col <- point_col[seq(2, length(point_col) - 1)]

       }

    } else if (!is.null(variable_cat)) {

      if (grepl("wrb_by_stock", variable_cat)) {

        point_col <- viridisLite::magma(6, direction = 1)

      # } else if (grepl("stock", variable_cat) &&
      #            grepl("cat", variable_cat)) {
      #
      #   point_col <- colorRampPalette(c("white", "black"))(7)
      #   point_col <- point_col[seq(2, length(point_col) - 1)]

      } else if (grepl("wrb", variable_cat)) {

        # Based on worldwide WRB map:

        point_col <- c( "#D8DAA6", # Cambisols
                        "#B49FD7", # Podzols
                        "#EDBF2B", # Arenosols
                        "#9BCE95", # Regosols
                        "#DCDCDD", # Leptosols
                        "#FF995C", # Luvisols
                        "#513568", # Histosols
                        "#008F11", # Gleysols
                        "#C37776", # Umbrisols
                        "#84FED0", # Stagnosols
                        "#7D6F7D") # Other

        # Based on BioSoil map 2006:

        point_col <- c( "#FF6600", # Cambisols
                        "#00FF00", # Podzols
                        "#FFF500", # Arenosols
                        "#FFD37F", # Regosols
                        "#808080", # Leptosols
                        "#E69800", # Luvisols
                        "#993300", # Histosols
                        "#000099", # Gleysols
                        "#006900", # Umbrisols
                        "#73DFFF", # Stagnosols
                        "#B7B7B7") # Other
      } else {

        # Define colour palette categorical non-continuous (e.g. EFTC)
        point_col <- c("green4",
                        "dodgerblue2",
                        "#E31A1C",
                        "#6A3D9A",
                        "#FF7F00",
                        "#006e6a",
                        "#91DA00",
                        "gold1",
                        "#c99800",
                        "deeppink1",
                        "blue1",
                        "maroon",
                        "steelblue4",
                        "darkorange4",
                        "#B7B7B7")

        point_col <- viridisLite::magma(n_cat + 2, direction = -1)
        point_col <- point_col[seq(1, length(point_col) - 2)]

        if (#grepl("stock", variable_cat) &&
          grepl("cat", variable_cat)) {

          point_col <- colorRampPalette(c("white", "black"))(n_cat + 2)
          point_col <- case_when(
            grepl("dark", mode) ~ viridisLite::inferno(n_cat + 2),
            TRUE ~ viridisLite::inferno(n_cat + 2, direction = -1))
          point_col <- point_col[seq(2, length(point_col) - 1)]

        }
      }

      country_border_col <- "#3a494a"

      point_col <-
        point_col[seq_len(n_distinct(get_env(layers)[[variable_cat]]))]


    } else {

      point_col <- c("#77203B",
                     "#d44102",
                     "#fabe02",
                     "#ffff69")
                    # "#F0F921")

      country_border_col <- "#3a494a"

      point_col <- point_col[seq_len(length(layers))]


    }


  }


  # Legend ----
  # create a character vector with the colours and the corresponding
  # class as name

  if (!is.null(legend_classes)) {

  n_plots <- NULL

  if (identical(legend_classes, TRUE) &&
      !is.null(variable_cat)) {

    if (grepl("wrb_by_stock", variable_cat) ||
        (grepl("stock", variable_cat) && grepl("cat", variable_cat))) {

      suppressMessages({
        layers_part1 <- get_env(layers) %>%
          filter(!.data[[variable_cat]] %in% c("Other", "Unknown")) %>%
          distinct(.data[[variable_cat]]) %>%
          left_join(
            get_env(layers) %>%
              group_by(.data[[variable_cat]]) %>%
              reframe(count = n()) %>%
              ungroup)
      })


    } else {

      layers_part1 <- get_env(layers) %>%
        filter(!.data[[variable_cat]] %in% c("Other", "Unknown")) %>%
        group_by(.data[[variable_cat]]) %>%
        reframe(count = n()) # %>%
       # arrange(desc(count))

      layers_part1 <- get_env(layers) %>%
        distinct(.data[[variable_cat]]) %>%
        left_join(layers_part1)
    }

    legend_classes_col <- bind_rows(
      layers_part1,
      get_env(layers) %>%
        filter(.data[[variable_cat]] %in% c("Other", "Unknown")) %>%
        group_by(.data[[variable_cat]]) %>%
        reframe(count = n())) %>%
      bind_cols(col = point_col) %>%
      mutate(name = str_wrap_br(.data[[variable_cat]])) %>%
      rowwise %>%
      mutate(name = ifelse(
        grepl("eftc", variable_cat),
        paste0("<span style='color:black;'>",
               name, " · </span>",
               "<span style='color:", col, ";'>n = ",
               count, "</span>"),
        paste0("<span style='color:black;'>",
               name, "</span>",
               "<span style='color:", col, ";'><br>n = ",
               count, "</span>")))

    n_plots <- legend_classes_col %>%
      pull(count)

    legend_classes <- legend_classes_col %>%
      pull(.data[[variable_cat]])

    legend_classes <- str_wrap_br(legend_classes)

  }

  if (count_plots_legend == TRUE) {

    if (is.null(n_plots)) {

    # Get the number of plots for each layer
    n_plots <- c(nrow(get(layers[1], envir = .GlobalEnv)),
                 nrow(get(layers[2], envir = .GlobalEnv)),
                 nrow(get(layers[3], envir = .GlobalEnv)))

    if (length(layers) == 4) {

      n_plots <- c(nrow(get(layers[1], envir = .GlobalEnv)),
                   nrow(get(layers[2], envir = .GlobalEnv)),
                   nrow(get(layers[3], envir = .GlobalEnv)),
                   nrow(get(layers[4], envir = .GlobalEnv)))
    }
    }


    # Generate adjusted Markdown classes with counts

    if (!identical(variable_cat, NULL) &&
        grepl("eftc", variable_cat)) {

      legend_classes <- mapply(function(cls, col, count) {
        paste0("<span style='color:black;'>",
               cls, " · </span>",
               "<span style='color:", col, ";'>n = ", count, "</span>")
      },
      legend_classes, point_col, n_plots) %>% as.vector

    } else {

      legend_classes <- mapply(function(cls, col, count) {
        paste0("<span style='color:black;'>",
               cls, "</span>",
               "<span style='color:", col, ";'><br>n = ", count, "</span>")
      },
      legend_classes, point_col, n_plots) %>% as.vector
    }

  }




  legend_values <- point_col

  for (i in seq_along(legend_classes)) {
    names(legend_values)[i] <- legend_classes[i]
  }

  }

  # # Define the background fill colour for countries outside of Europe
  # col_background <- "#7D9191"
  #
  # # Define the panel background colours (i.e. colour of the sea)
  # sea_col <- col_sea # "#D8E0E0"

  # Define colour palette biogeographical regions
  biogeo_col <- c("#14293a",
                  "#6b9c6c",
                  "#005f0e",
                  "#004a30",
                  "#006e6a",
                  "#649600",
                  "#0097a5",
                  "#c99800",
                  "#00001f",
                  "#33002d")
                 # "#556400")

  # biogeo_col <- c("#F7EDCAFF",
  #                 "#8EB155FF",
  #                 "#497367FF",
  #                 "#F5DC9AFF",
  #                 "#2B3F00FF",
  #                 "#764000FF",
  #                 "#E19E57FF",
  #                 "#14293a",
  #                 "#0097a5",
  #                 "#6b9c6c")
                  # "#020570FF",
                  # "#3165B1FF",
                  # "#B8F7FEFF")

  biogeo_col <- colorspace::lighten(
    colorspace::desaturate(biogeo_col, amount = 0.3),
    amount = 0.8)


  # if (is.null(biogeo_palette)) {
  #   background_col <- "#222222"
  #   background_col <- "white" # "#222222"
  #     #"black" # "#293333" #"#203233" #"#14293a"
  #   sea_col <- col_sea #"#C2CECE"   #"#a1b3b3"
  # }



  # Continuous map: assert that the column is specified

  if (is.null(legend_classes)) {
    assertthat::assert_that(!is.null(variable_continuous),
                            msg = paste0("Please specify the column with ",
                                         "the continuous variable to be ",
                                         "shown with a continuous colour ",
                                         "scale."))
    assertthat::assert_that(
      variable_continuous %in% names(get(layers[1], envir = .GlobalEnv)),
      msg = paste0("Column '", variable_continuous, "' does not exist ",
                   "in object '", layers[1], "'."))
  }


  # X offset for inset maps
  # inset_maps_offset_x refers to the rectangles behind the maps
  # inset_maps_offset_x2 refers to the inset maps and is calculated based on
  # inset_maps_offset_x

  inset_maps_offset_x2 <- 0.057 * inset_maps_offset_x





  # Make a map for the first layer ----

  # Retrieve the first spatial layer of the 'layers' argument
  spat_layer_1 <- get(layers[1], envir = .GlobalEnv)

  if (!is.null(variable_cat)) {

    spat_layer_1 <- spat_layer_1 %>%
      left_join(legend_classes_col %>%
                  select(-count),
                by = variable_cat)

    if (count_plots_legend == FALSE) {

      spat_layer_1 <- spat_layer_1 %>%
        mutate(name = .data[[variable_cat]])
    }
  }


  ## Base map ----

  base_map <-
    ggplot() +
    # Map the fill colour of the countries (outside of Europe)
    geom_sf(data = world_spat, color = NA, fill = col_background) +
    # Give title to plot (input argument)
    ggtitle(title)


  if (!is.null(biogeo_palette)) {

    base_map <- base_map +
    # Map the biogeographical regions
    geom_sf(data = biogeo_sf, aes(fill = .data$code), color = NA)

    # Define the colour scale for the biogeographical regions layer
    if (biogeo_palette == "viridis") {
      base_map <- base_map + scale_fill_viridis_d()
    } else if (biogeo_palette == "biogeo_col") {
      base_map <- base_map + scale_fill_manual(values = biogeo_col)
    }

    # Create a legend for the biogeographical regions
  base_map <- base_map +
    guides(fill = guide_legend(title = "**Biogeographical region**", # Title
                               # Size of legend symbols (squares)
                               override.aes = list(size = 4),
                               # Order of this legend relative to other legends
                               # in the plot, i.e. second (from top)
                               order = 2))
  }


  base_map <- base_map +
    # Customise the theme of the plot
    theme(
      plot.background = element_rect(fill = col_background_plot,
                                     colour = col_background_plot),
      # # Set the size of the legend title to 10
      # legend.title = element_text(size = 10),
      # Set the font family to "sans" and colour to black for text elements
      text = element_text(family = "sans", color = col_front),
      # Black coordinates
      axis.text = element_text(color = col_front),
      # Set the size of the plot title to 10 and make it bold
      legend.title = element_markdown(lineheight = 1.4,
                                      colour = col_front,
                                      size = 10,
                                      margin = margin(b = 5)), #12
      plot.title = element_markdown(size = 10,
                                    lineheight = 1.4),
      # Position the legend at the top right corner of the plot
      legend.justification = c("right", "top"),
      # Set the background colour of the plot panel using the 'sea_col' variable
      panel.background = element_rect(fill = sea_col),
      # Set the color of major grid lines (i.e. coordinate lines)
      # in the plot to white
      panel.grid.major = element_line(color = col_panel),
      # Remove the grey background underneat the legend keys
      legend.key = element_blank(),
      legend.background = element_rect(fill = col_background_plot),
      legend.text = element_text(# size = 10,
                                 colour = col_front),
      plot.margin = margin(t = 0.5,  # Set top margin of the plot to 0.5 cm
                           r = 0.5,  # Set right margin of the plot to 0.5 cm
                           b = 0.5,  # Set bottom margin of the plot to 0.5 cm
                           l = 0.5,  # Set left margin of the plot to 0.5 cm
                           # Set the unit of measurement for margins
                           # to centimeters
                           unit = "cm")) +
    # Map the borders of the countries (without fill)
    geom_sf(data = world_spat, color = country_border_col,
            fill = NA, linewidth = 0.5) +
    # Map a square to cover Greenland (in order to plot the scale bar on top)
    # in 'sea_col' colour.
    geom_rect(aes(xmin = 2000000,
                  xmax = 4005151,
                  ymin = 5861986,
                  ymax = 5294329),
              fill = sea_col)

  if (!is.null(legend_classes) &&
      is.null(variable_cat)) {

    base_map <- base_map +
      theme(legend.text =
              element_markdown(vjust = -10,
                               lineheight = 1.2,
                               margin = margin(b = 5, t = 3, l = 5))) +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = legend_classes[1]),
              # Colour should refer to the legend class
              size = point_size) + # Input argument
      # Add a manual colour scale for each of the layers and legend classes
      # in the input arguments
      scale_color_manual(name = legend_title,
                         breaks = legend_classes,
                         values = legend_values) +
      guides(color = guide_legend(title = legend_title, # Input argument
                                  # Size of legend symbols (points)
                                  override.aes = list(size = 2),
                                  # Order of this legend relative to other
                                  # legends in the plot, i.e. first (from top)
                                  order = 1))

  } else if (!is.null(legend_classes) &&
             !is.null(variable_cat)) {

    base_map <- base_map +
      # theme(legend.text =
      #         element_markdown(vjust = 0.5,
      #                          lineheight = 1.1,
      #                          size = 8,
      #                          margin = margin(b = 0))) +
      theme(legend.text =
              element_markdown(vjust = 0.3,
                               lineheight = 1.2,
                               # size = 6,
                               margin = margin(b = 3, t = 3, l = 5))) +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = name),
              # Colour should refer to the legend class
              size = point_size) + # Input argument
      # Add a manual colour scale for each of the layers and legend classes
      # in the input arguments
      scale_color_manual(name = legend_title,
                         breaks = legend_classes,
                         values = legend_values) +
      guides(color = guide_legend(title = legend_title, # Input argument
                                  # Size of legend symbols (points)
                                  override.aes = list(size = 2),
                                  # Order of this legend relative to other
                                  # legends in the plot, i.e. first (from top)
                                  order = 1,
                                  keyheight = 1.1))

  } else {

    # Continuous variable

    base_map <- base_map +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = spat_layer_1[[variable_continuous]]),
              size = point_size) +
      scale_color_viridis_c(name = legend_title,
                            breaks =
                              seq(round(min(
                                spat_layer_1[[variable_continuous]])),
                                  100 * ceiling(max(0.01 *
                                      spat_layer_1[[variable_continuous]])),
                                         by = 100),
                            option = "plasma",
                            direction = 1,
                            guide = guide_colorbar(title = legend_title,
                                                   order = 1))

  }

  base_map <- base_map +
    # Add a scale bar to the plot
    ggspatial::annotation_scale(
      # Specify the width of the scale bar relative to the plot width
      width_hint = 0.5,
      # Set the location of the scale bar to the top left corner of the plot
      location = "tl",
      # Specify the width of the scale bar line
      line_width = 1,
      # Set the vertical distance between the scale bar and the edge of the
      # panel
      pad_y = unit(0.3, "cm")) +
    # Add a north arrow to the plot
    ggspatial::annotation_north_arrow(
      # Specify the style of the north arrow
      style = north_arrow_fancy_orienteering,
      # Set the location of the north arrow to the top left corner of the plot
      location = "tl",
      # Set the height of the north arrow
      height = unit(1, "cm"),
      # Set the width of the north arrow
      width = unit(1, "cm"),
      # Set the vertical distance between the north arrow and the edge of the
      # panel
      pad_y = unit(1, "cm"))

    # At this point, the limits of the x axis and y axis have not yet been
    # specified (using coord_sf()). This is necessary to restrict the visible
    # area, but can only be done after any other layers have been added.


  ## Azores inset map ----

  azores_map <-
    ggplot() +
    geom_sf(data = world_spat, color = NA, fill = col_background)

  if (!is.null(biogeo_palette)) {

    azores_map <- azores_map +
      geom_sf(data = biogeo_sf, aes(fill = .data$short_name), color = NA)

    if (biogeo_palette == "viridis") {
      azores_map <- azores_map + scale_fill_viridis_d()
    } else if (biogeo_palette == "biogeo_col") {
      azores_map <- azores_map + scale_fill_manual(values = biogeo_col)
    }
  }

  azores_map <- azores_map +
    theme(legend.title = element_text(size = 10),
          text = element_text(family = "sans", color = col_front),
          # Adjust the margins around the plot title
          plot.title = element_text(size = 10, face = "bold",
                                    margin = margin(0, 0, 0, 0)),
          legend.justification = c("right", "top"),
          # Remove the legend
          legend.position = "none",
          # Remove major grid lines (coordinate lines)
          panel.grid.major = element_blank(),
          # Remove axis ticks and text labels (coordinates)
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          # Remove the plot background
          plot.background = element_blank(),
          # Set the background color of the plot panel using the sea_col
          # variable
          panel.background = element_rect(fill = sea_col)) +
    geom_sf(data = world_spat, color = country_border_col,
            fill = NA, linewidth = 0.5)

  if (!is.null(legend_classes) &&
      is.null(variable_cat)) {

    azores_map <- azores_map +
      geom_sf(data = spat_layer_1,
              aes(color = legend_classes[1]),
              size = point_size) +
      scale_color_manual(name = legend_title,
                         breaks = legend_classes,
                         values = legend_values)

  } else if (!is.null(legend_classes) &&
             !is.null(variable_cat)) {

    azores_map <- azores_map +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = name),
              # Colour should refer to the legend class
              size = point_size) + # Input argument
      # Add a manual colour scale for each of the layers and legend classes
      # in the input arguments
      scale_color_manual(name = legend_title,
                         breaks = legend_classes,
                         values = legend_values)

  } else {

    # Continuous variable

    azores_map <- azores_map +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = spat_layer_1[[variable_continuous]]),
              size = point_size) +
      scale_color_viridis_c(name = legend_title,
                            breaks =
                              seq(round(min(
                                spat_layer_1[[variable_continuous]])),
                                100 * ceiling(max(0.01 *
                                        spat_layer_1[[variable_continuous]])),
                                by = 100),
                            option = "plasma",
                            direction = 1,
                            guide = guide_colorbar(title = legend_title,
                                                   order = 1))

  }


  azores_map <- azores_map +
    ggtitle("Azores (Portugal)") +
    # Update the size of the plot title and justify horizontally to the left
    theme(plot.title = element_text(hjust = 0.02, size = 7.5))





  ## Canary Islands inset map ----

  canary_map <-
    ggplot() +
    geom_sf(data = world_spat, color = NA, fill = col_background)

  if (!is.null(biogeo_palette)) {

    canary_map <- canary_map +
    geom_sf(data = biogeo_sf, aes(fill = .data$short_name), color = NA)

    if (biogeo_palette == "viridis") {
      canary_map <- canary_map + scale_fill_viridis_d()
    } else if (biogeo_palette == "biogeo_col") {
      canary_map <- canary_map + scale_fill_manual(values = biogeo_col)
    }
  }

  canary_map <- canary_map +
    theme(legend.title = element_text(size = 10),
          text = element_text(family = "sans", color = col_front),
          plot.title = element_text(size = 10,
                                    face = "bold",
                                    margin = margin(0, 0, 0, 0)),
          legend.justification = c("right", "top"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_rect(fill = sea_col)) +
    geom_sf(data = world_spat, color = country_border_col,
            fill = NA, linewidth = 0.5)

  if (!is.null(legend_classes) &&
      is.null(variable_cat)) {

    canary_map <- canary_map +
      geom_sf(data = spat_layer_1,
              aes(color = legend_classes[1]),
              size = point_size) +
      scale_color_manual(name = legend_title,
                         breaks = legend_classes,
                         values = legend_values)


  } else if (!is.null(legend_classes) &&
             !is.null(variable_cat)) {

    canary_map <- canary_map +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = name),
              # Colour should refer to the legend class
              size = point_size) + # Input argument
      # Add a manual colour scale for each of the layers and legend classes
      # in the input arguments
      scale_color_manual(name = legend_title,
                         breaks = legend_classes,
                         values = legend_values)

  } else {

    # Continuous variable

    canary_map <- canary_map +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = spat_layer_1[[variable_continuous]]),
              size = point_size) +
      scale_color_viridis_c(name = legend_title,
                            breaks =
                              seq(round(min(
                                spat_layer_1[[variable_continuous]])),
                                100 * ceiling(max(0.01 *
                                        spat_layer_1[[variable_continuous]])),
                                by = 100),
                            option = "plasma",
                            direction = 1,
                            guide = guide_colorbar(title = legend_title,
                                                   order = 1))

  }

  canary_map <- canary_map +
    ggtitle("Canary Islands (Spain)") +
    theme(plot.title = element_text(hjust = 0.02, size = 7.5))





  ## Cyprus inset map ----

  cyprus_map <-
    ggplot() +
    geom_sf(data = world_spat, color = NA, fill = col_background)

  if (!is.null(biogeo_palette)) {

    cyprus_map <- cyprus_map +
    geom_sf(data = biogeo_sf, aes(fill = .data$short_name), color = NA)

    if (biogeo_palette == "viridis") {
      cyprus_map <- cyprus_map + scale_fill_viridis_d()
    } else if (biogeo_palette == "biogeo_col") {
      cyprus_map <- cyprus_map + scale_fill_manual(values = biogeo_col)
    }
  }

  cyprus_map <- cyprus_map +
    theme(legend.title = element_text(size = 10),
          text = element_text(family = "sans", color = col_front),
          plot.title = element_text(size = 10,
                                    face = "bold",
                                    # Leave 2 pt of margin below the title
                                    # So that there is some space between the
                                    # title and the map (which is necessary
                                    # for Cyprus only)
                                    margin = margin(0, 0, 2, 0)),
          legend.justification = c("right", "top"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_rect(fill = sea_col)) +
    geom_sf(data = world_spat, color = country_border_col,
            fill = NA, linewidth = 0.5)

  if (!is.null(legend_classes) &&
      is.null(variable_cat)) {

    cyprus_map <- cyprus_map +
      geom_sf(data = spat_layer_1,
              aes(color = legend_classes[1]),
              size = point_size) +
      scale_color_manual(name = legend_title,
                         breaks = legend_classes,
                         values = legend_values)

  } else if (!is.null(legend_classes) &&
             !is.null(variable_cat)) {

    cyprus_map <- cyprus_map +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = name),
              # Colour should refer to the legend class
              size = point_size) + # Input argument
      # Add a manual colour scale for each of the layers and legend classes
      # in the input arguments
      scale_color_manual(name = legend_title,
                         breaks = legend_classes,
                         values = legend_values)

  } else {

    # Continuous variable

    cyprus_map <- cyprus_map +
      # Map the points of the first sf layer
      geom_sf(data = spat_layer_1,
              aes(color = spat_layer_1[[variable_continuous]]),
              size = point_size) +
      scale_color_viridis_c(name = legend_title,
                            breaks =
                              seq(round(min(
                                spat_layer_1[[variable_continuous]])),
                                100 * ceiling(max(0.01 *
                                        spat_layer_1[[variable_continuous]])),
                                by = 100),
                            option = "plasma",
                            direction = 1,
                            guide = guide_colorbar(title = legend_title,
                                                   order = 1))

  }

  cyprus_map <- cyprus_map +
    ggtitle("Cyprus") +
    theme(plot.title = element_text(hjust = 0.02, size = 7.5))





  # Add coordinate system (limits of x and y) in case of one layer ----

  if (length(layers) == 1) {
    base_map <- base_map +
    coord_sf(xlim = c(2400000, 6000000),
             ylim = c(1000000, 5400000))

    azores_map <- azores_map +
      coord_sf(xlim = c(929000, 1370000),
               ylim = c(2800000, 2200000))

    canary_map <- canary_map +
      coord_sf(xlim = c(1550000, 2050000),
               ylim = c(1200000, 920000))

    cyprus_map <- cyprus_map +
      coord_sf(xlim = c(6330000, 6550000),
               ylim = c(1770000, 1550000))
  }


  # Add the second layer and coordinate system in case of two layers ----

  if (length(layers) == 2) {

    # Retrieve the second spatial layer of the 'layers' argument
    spat_layer_2 <- get(layers[2], envir = .GlobalEnv)

    base_map <- base_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      coord_sf(xlim = c(2400000, 6000000),
               ylim = c(1000000, 5400000))

    azores_map <- azores_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      coord_sf(xlim = c(929000, 1370000),
               ylim = c(2800000, 2200000))

    canary_map <- canary_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      coord_sf(xlim = c(1550000, 2050000),
               ylim = c(1200000, 920000))

    cyprus_map <- cyprus_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      coord_sf(xlim = c(6330000, 6550000),
               ylim = c(1770000, 1550000))
    }


  # Add the second + third layer and coordinate system in case of 3 layers ----

  if (length(layers) == 3) {

    # Retrieve the second + third spatial layer of the 'layers' argument
    spat_layer_2 <- get(layers[2], envir = .GlobalEnv)
    spat_layer_3 <- get(layers[3], envir = .GlobalEnv)

    base_map <- base_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      geom_sf(data = spat_layer_3,
              aes(color = legend_classes[3]),
              size = point_size) +
      coord_sf(xlim = c(2400000, 6000000),
               ylim = c(1000000, 5400000))

    azores_map <- azores_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      geom_sf(data = spat_layer_3,
              aes(color = legend_classes[3]),
              size = point_size) +
      coord_sf(xlim = c(929000, 1370000),
               ylim = c(2800000, 2200000))

    canary_map <- canary_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      geom_sf(data = spat_layer_3,
              aes(color = legend_classes[3]),
              size = point_size) +
      coord_sf(xlim = c(1550000, 2050000),
               ylim = c(1200000, 920000))

    cyprus_map <- cyprus_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      geom_sf(data = spat_layer_3,
              aes(color = legend_classes[3]),
              size = point_size) +
      coord_sf(xlim = c(6330000, 6550000),
               ylim = c(1770000, 1550000))
    }





  # Add the second + third + 4th layer and coordinate system in case of 4 ----

  if (length(layers) == 4) {

    # Retrieve the second + third spatial layer of the 'layers' argument
    spat_layer_2 <- get(layers[2], envir = .GlobalEnv)
    spat_layer_3 <- get(layers[3], envir = .GlobalEnv)
    spat_layer_4 <- get(layers[4], envir = .GlobalEnv)

    base_map <- base_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      geom_sf(data = spat_layer_3,
              aes(color = legend_classes[3]),
              size = point_size) +
      geom_sf(data = spat_layer_4,
              aes(color = legend_classes[4]),
              size = point_size) +
      coord_sf(xlim = c(2400000, 6000000),
               ylim = c(1000000, 5400000))

    azores_map <- azores_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      geom_sf(data = spat_layer_3,
              aes(color = legend_classes[3]),
              size = point_size) +
      geom_sf(data = spat_layer_4,
              aes(color = legend_classes[4]),
              size = point_size) +
      coord_sf(xlim = c(929000, 1370000),
               ylim = c(2800000, 2200000))

    canary_map <- canary_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      geom_sf(data = spat_layer_3,
              aes(color = legend_classes[3]),
              size = point_size) +
      geom_sf(data = spat_layer_4,
              aes(color = legend_classes[4]),
              size = point_size) +
      coord_sf(xlim = c(1550000, 2050000),
               ylim = c(1200000, 920000))

    cyprus_map <- cyprus_map +
      geom_sf(data = spat_layer_2,
              aes(color = legend_classes[2]),
              size = point_size) +
      geom_sf(data = spat_layer_3,
              aes(color = legend_classes[3]),
              size = point_size) +
      geom_sf(data = spat_layer_4,
              aes(color = legend_classes[4]),
              size = point_size) +
      coord_sf(xlim = c(6330000, 6550000),
               ylim = c(1770000, 1550000))
  }



  # Put base map and inset maps together ----

  full_map <-
    ggdraw() +
    # Draw base map
    draw_plot(base_map) +
    # Draw the left rectangle on which the Canary Islands inset map will be
    # plotted
    draw_grob(
      # Create a rectangular grob object
      rectGrob(
        # Set the x and y coordinates of the rectangles's location
        x = unit(2.21 + inset_maps_offset_x, "cm"),
        y = unit(1.1, "cm"),
        # Set the width and height of the rectangle
        width = unit(3.2, "cm"),
        height = unit(1.55, "cm"),
        # Set horizontal justification to left-justified
        hjust = 0,
        # Set vertical justification to top-justified
        vjust = 0,
        # Set the graphical parameters, i.e. the fill and border colour
        gp = gpar(fill = sea_col, col = "white"))) +
    # Draw the middle rectangle on which the Azores inset map will be plotted
    draw_grob(rectGrob(x = unit(5.59 + inset_maps_offset_x, "cm"),
                       y = unit(1.1, "cm"),
                       width = unit(2.6, "cm"),
                       height = unit(1.55, "cm"),
                       hjust = 0,
                       vjust = 0,
                       gp = gpar(fill = sea_col, col = "white"))) +
    # Draw the right rectangle on which the Cyprus inset map will be plotted
    draw_grob(rectGrob(x = unit(8.37 + inset_maps_offset_x, "cm"),
                       y = unit(1.1, "cm"),
                       width = unit(1.3, "cm"), #2.6
                       height = unit(1.55, "cm"),
                       hjust = 0,
                       vjust = 0,
                       gp = gpar(fill = sea_col, col = "white"))) +
    # Add Canary Islands inset map
    draw_plot(canary_map,
              # Set height of map
              height = 0.14,
              # Set the x and y coordinates of the map location
              x = -0.307 + inset_maps_offset_x2,
              y = 0.06) +
    # Add Azores inset map
    draw_plot(azores_map,
              height = 0.14,
              x = -0.145 + inset_maps_offset_x2,
              y = 0.06) +
    # Add Cyprus inset map
    draw_plot(cyprus_map,
              height = 0.14,
              x = 0.021 + inset_maps_offset_x2,
              y = 0.06)


  # Save map ----

  # Create path folder if this does not exist
  if (!dir.exists(export_folder)) {
    dir.create(export_folder, recursive = TRUE)
  }

  # Set path name based on input arguments
  path <- paste0(export_folder,
                 export_name, ".png")

  # Save as .png
  ggsave(path,
         # Set the resolution of the saved plot (dots per inch)
         # Recommendation: at least 500 dpi
         plot = full_map,
         dpi = 500,
         # Set the width and height of the saved plot in inches
         width = width,
         height = 5.3)


  if (return == TRUE) {
    return(full_map)
  }




  if (with_logo == TRUE) {


    # Call back the plot
    plot <- magick::image_read(path = path)

    # Import the logo
    logo <-
      magick::image_read(paste0("./data/additional_data/",
                                "ICP_LOGO_transparent_background.png")) %>%
      magick::image_resize("500x")

    # Calculate the offset for the lower right corner
    offset_x <- magick::image_info(plot)$width -
      magick::image_info(logo)$width - 480  # Adjust the margin as needed
    offset_y <- magick::image_info(plot)$height -
      magick::image_info(logo)$height - 180  # Adjust the margin as needed

    diff_x <- 300

    # Overlay the logo onto the old graph
    final_image <-
      magick::image_composite(plot, logo,
                              offset = paste0("+", offset_x,
                                              "+", offset_y)) %>%
      magick::image_crop(paste0((magick::image_info(plot)$width - diff_x), "x",
                                magick::image_info(plot)$height, "+",
                                diff_x / 2, "+0"))

    # Save or display the final composite image
    image_write(final_image, path = path)

  }

  # if (count_plots_legend == TRUE) {
  #
  #
  #   # Call back the plot
  #   plot <- magick::image_read(path = path)
  #
  #   # Import the logo
  #   logo <-
  #     magick::image_read(paste0("./data/additional_data/",
  #                               "ICP_LOGO_transparent_background.png")) %>%
  #     magick::image_resize("500x")
  #
  #   # Calculate the offset for the lower right corner
  #   offset_x <- magick::image_info(plot)$width - 980
  #   offset_y <- magick::image_info(plot)$height - 420
  #
  #   # Overlay the logo onto the old graph
  #   final_image <-
  #     magick::image_composite(plot,
  #                             magick::image_crop(plot,
  #                                                "900x1000+2450+320"),
  #                             offset = paste0("+", "2450", "+", "380"))
  #
  #   # Save or display the final composite image
  #   image_write(final_image, path = path)
  #
  # }






}
