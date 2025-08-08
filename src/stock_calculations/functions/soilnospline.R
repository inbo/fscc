
soilnospline <- function(prof_input) {

  # This function returns a value for every single centimeter from 1 cm
  # until the lower depth of the lowest original layer. It takes any gaps
  # between the layers into account.

  # Find the overall depth range
  min_depth <- min(prof_input$depth_top)
  max_depth <- max(prof_input$depth_bottom)

  # Create a complete sequence of 1 cm depths
  all_depths <- seq(from = min_depth + 1, to = max_depth, by = 1)

  # Initialize result vector
  result_values <- numeric(length(all_depths))

  # Process each depth
  for (j in seq_along(all_depths)) {
    current_depth <- all_depths[j]

    # Check if current depth falls within any existing layer
    layer_match <- which(prof_input$depth_top < current_depth &
                           prof_input$depth_bottom >= current_depth)

    if (length(layer_match) > 0) {
      # Depth falls within an existing layer
      result_values[j] <- prof_input$variab[layer_match[1]]
    } else {
      # Depth falls in a gap - need interpolation
      # Find the layers before and after this depth
      before_layer <- which(prof_input$depth_bottom < current_depth)
      after_layer <- which(prof_input$depth_top >= current_depth)

      assertthat::assert_that(
        length(before_layer) > 0 && length(after_layer) > 0)

      # Get the closest layers
      before_idx <- before_layer[length(before_layer)] # Last layer before gap
      after_idx <- after_layer[1]  # First layer after gap

      # Interpolation points
      x1 <- prof_input$depth_bottom[before_idx]
      y1 <- prof_input$variab[before_idx]
      x2 <- prof_input$depth_top[after_idx]
      y2 <- prof_input$variab[after_idx]

      # Linear interpolation
      result_values[j] <- y1 + (y2 - y1) * (current_depth - x1) / (x2 - x1)

    }
  }

  return(result_values)

}
