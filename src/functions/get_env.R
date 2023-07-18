
#' Get the value of an object from the appropriate environment.
#'
#' This function retrieves the value of an object with the specified name. The
#' object is retrieved from either the R global environment or the knitr
#' environment, depending on whether the code is being executed in an R Markdown
#' document or a regular R script.
#'
#' @param x The name of the object to be retrieved.
#'
#' @return The value of the object.
#'
#' @examples
#' get_env("result")
#'
#' @importFrom base assign
#' @importFrom base getOption
#' @importFrom knitr knit_global

get_env <- function(x) {
  
  if (isTRUE(getOption("knitr.in.progress"))) {

    value <- get(x = x, envir = knitr::knit_global())
    return(value)
    
  } else {

    value <- get(x = x, envir = globalenv())
    return(value)

  }
}
