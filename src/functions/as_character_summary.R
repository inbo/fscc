
#' Convert a vector to character summary
#'
#' This function converts a vector to a character summary,
#' providing a concise representation of the vector contents.
#'
#' @param vector_to_convert Vector to be converted to character summary
#' @return A character summary of the vector contents.
#' @examples
#' as_character_summary(1:3)
#' # Returns "1, 2 and 3"
#'
#' as_character_summary(c("red", "blue"))
#' # Returns "red and blue"
#'
#' as_character_summary("green")
#' # Returns "green"

as_character_summary <- function(vector_to_convert) {

  vec_caution <- if (length(vector_to_convert) == 1) {
    as.character(vector_to_convert)

  } else if (length(vector_to_convert) == 2) {
    paste(vector_to_convert, collapse = " and ")

  } else {
    paste(vector_to_convert[-length(vector_to_convert)], collapse = ", ")
    paste(" and ", vector_to_convert[length(vector_to_convert)],
          sep = "", collapse = "")
    paste0(paste(vector_to_convert[-length(vector_to_convert)],
                 collapse = ", "),
           paste(" and ", vector_to_convert[length(vector_to_convert)],
                 sep = ""))
  }

  return(vec_caution)

}
