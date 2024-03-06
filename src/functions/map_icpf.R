
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
#' classes (in the same order as the layers).
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
                     export_name,
                     export_folder,
                     point_size,
                     point_col = NULL,
                     biogeo_palette = "biogeo_col") {

  # Install packages ----

  # Define packages to install
  packages_map_icpf <- c("sf",
                         "tidyverse",
                         "grid",
                         "cowplot",    # To put maps together
                         "ggspatial",  # For scale bar and north arrow
                         "geodata",    # To download country borders
                         "ggtext")     # Markdown text

  # Install all packages that are not already installed
  install.packages(setdiff(packages_map_icpf, rownames(installed.packages())))

  # Load packages
  sapply(packages_map_icpf, library, character.only = TRUE)



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
                          "Black Sea", .data$code))

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

  if (is.null(point_col)) {

    if (!is.null(biogeo_palette)) {

       point_col <- c("#b34646",  #"#b03232",  #"#E66101",
                      "#F8C4B4",
                      "#780116",
                      "#FF0000")
    } else {

      point_col <- c("#8C9605",
                     "#C99800",
                     "white",#006E6A",
                     "#03593b")

      point_col <- c("#039c96",
                     "#02c74b",
                     "#b3de02",
                     "#69413D")

      point_col <- c("#c40275",
                     "#ed5e2f",
                     "#fabe02",
                     "#F0F921")

      country_border_col <- "#5b7273" #"#2b4445"

    }

    point_col <- point_col[seq_len(length(layers))]

  }


  # Legend: create a character vector with the colours and the corresponding
  # class as name

  legend_values <- point_col

  for (i in seq_along(layers)) {
    names(legend_values)[i] <- legend_classes[i]
    }

  # Define the background fill colour for countries outside of Europe
  background_col <- "#7D9191"

  # Define the panel background colours (i.e. colour of the sea)
  sea_col <- "#D8E0E0"

  # Define colour palette biogeographical regions
  biogeo_col <- c("#14293a",
                  "#556400",
                  "#6b9c6c",
                  "#005f0e",
                  "#004a30",
                  "#006e6a",
                  "#649600",
                  "#0097a5",
                  "#c99800",
                  "#00001f",
                  "#33002d")

  if (is.null(biogeo_palette)) {
    background_col <- "black" # "#293333" #"#203233" #"#14293a"
  }


  # Make a map for the first layer ----

  # Retrieve the first spatial layer of the 'layers' argument
  spat_layer_1 <- get(layers[1], envir = .GlobalEnv)


  # Make a base map
  base_map <-
    ggplot() +
    # Map the fill colour of the countries (outside of Europe)
    geom_sf(data = world_spat, color = NA, fill = background_col) +
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
    guides(fill = guide_legend(title = "Biogeographical region", # Title
                               # Size of legend symbols (squares)
                               override.aes = list(size = 5),
                               # Order of this legend relative to other legends
                               # in the plot, i.e. second (from top)
                               order = 2))
  }


  base_map <- base_map +
    # Customise the theme of the plot
    theme(
      # # Set the size of the legend title to 10
      # legend.title = element_text(size = 10),
      # Set the font family to "sans" and colour to black for text elements
      text = element_text(family = "sans", color = "black"),
      # Black coordinates
      axis.text = element_text(color = "black"),
      # Set the size of the plot title to 10 and make it bold
      legend.title = element_markdown(size = 10),
      plot.title = element_markdown(size = 10),
      # Position the legend at the top right corner of the plot
      legend.justification = c("right", "top"),
      # Set the background colour of the plot panel using the 'sea_col' variable
      panel.background = element_rect(fill = sea_col),
      # Set the color of major grid lines (i.e. coordinate lines)
      # in the plot to white
      panel.grid.major = element_line(color = "white"),
      # Remove the grey background underneat the legend keys
      legend.key = element_blank(),
      plot.margin = margin(t = 0.5,  # Set top margin of the plot to 0.5 cm
                           r = 0.5,  # Set right margin of the plot to 0.5 cm
                           b = 0.5,  # Set bottom margin of the plot to 0.5 cm
                           l = 0.5,  # Set left margin of the plot to 0.5 cm
                           # Set the unit of measurement for margins
                           # to centimeters
                           unit = "cm"),
      legend.text =
        element_text(vjust = 0,
                     margin =
                       margin(b = 5))) +
    # Map the borders of the countries (without fill)
    geom_sf(data = world_spat, color = country_border_col,
            fill = NA, linewidth = 0.5) +
    # Map a square to cover Greenland (in order to plot the scale bar on top)
    # in 'sea_col' colour.
    geom_rect(aes(xmin = 2000000,
                  xmax = 4005151,
                  ymin = 5861986,
                  ymax = 5294329),
              fill = sea_col) +
    # Map the points of the first sf layer
    geom_sf(data = spat_layer_1,
            aes(color = legend_classes[1]), # Colour should refer to the legend
                                            # class
            size = point_size) + # Input argument
    # Add a manual colour scale for each of the layers and legend classes
    # in the input arguments
    scale_color_manual(name = legend_title,
                       breaks = legend_classes,
                       values = legend_values) +
    guides(color = guide_legend(title = legend_title, # Input argument
                                # Size of legend symbols (points)
                                override.aes = list(size = 2),
                                # Order of this legend relative to other legends
                                # in the plot, i.e. first (from top)
                                order = 1)) +
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


  # Create the Azores inset map
  azores_map <-
    ggplot() +
    geom_sf(data = world_spat, color = NA, fill = background_col)

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
          text = element_text(family = "sans", color = "black"),
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
            fill = NA, linewidth = 0.5) +
    geom_sf(data = spat_layer_1,
            aes(color = legend_classes[1]),
            size = point_size) +
    scale_color_manual(name = legend_title,
                       breaks = legend_classes,
                       values = legend_values) +
    ggtitle("Azores (Portugal)") +
    # Update the size of the plot title and justify horizontally to the left
    theme(plot.title = element_text(hjust = 0.02, size = 7.5))

  # Create the Canary Islands inset map
  canary_map <-
    ggplot() +
    geom_sf(data = world_spat, color = NA, fill = background_col)

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
          text = element_text(family = "sans", color = "black"),
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
            fill = NA, linewidth = 0.5) +
    geom_sf(data = spat_layer_1,
            aes(color = legend_classes[1]),
            size = point_size) +
    scale_color_manual(name = legend_title,
                       breaks = legend_classes,
                       values = legend_values) +
    ggtitle("Canary Islands (Spain)") +
    theme(plot.title = element_text(hjust = 0.02, size = 7.5))

  # coord_sf(xlim = c(1550000,2050000), ylim = c(1200000,920000)) +

  # Create the Cyprus inset map
  cyprus_map <-
    ggplot() +
    geom_sf(data = world_spat, color = NA, fill = background_col)

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
          text = element_text(family = "sans", color = "black"),
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
            fill = NA, linewidth = 0.5) +
    geom_sf(data = spat_layer_1,
            aes(color = legend_classes[1]),
            size = point_size) +
    scale_color_manual(name = legend_title,
                       breaks = legend_classes,
                       values = legend_values) +
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
        x = unit(2.21, "cm"),
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
    draw_grob(rectGrob(x = unit(5.59, "cm"),
                       y = unit(1.1, "cm"),
                       width = unit(2.6, "cm"),
                       height = unit(1.55, "cm"),
                       hjust = 0,
                       vjust = 0,
                       gp = gpar(fill = sea_col, col = "white"))) +
    # Draw the right rectangle on which the Cyprus inset map will be plotted
    draw_grob(rectGrob(x = unit(8.37, "cm"),
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
              x = -0.307,
              y = 0.06) +
    # Add Azores inset map
    draw_plot(azores_map,
              height = 0.14,
              x = -0.145,
              y = 0.06) +
    # Add Cyprus inset map
    draw_plot(cyprus_map,
              height = 0.14,
              x = 0.021,
              y = 0.06)


  # Save map ----

  # Set path name based on input arguments
  path_folder <- paste0("./output/",
                 export_folder, "/")

  # Create path folder if this does not exist
  if (!dir.exists(path_folder)) {
    dir.create(path_folder, recursive = TRUE)
    }

  # Set path name based on input arguments
  path <- paste0("./output/",
                 export_folder, "/",
                 export_name, ".png")

  # Save as .png
  ggsave(path,
         # Set the resolution of the saved plot (dots per inch)
         # Recommendation: at least 500 dpi
         plot = full_map,
         dpi = 500,
         # Set the width and height of the saved plot in inches
         width = 6.81,
         height = 5.3)

}
