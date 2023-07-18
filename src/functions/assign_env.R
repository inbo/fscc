
#' Assign a value to an object in the appropriate environment.
#'
#' This function assigns a given value to an object with the specified name.
#' The object is created either in the R global environment or the knitr
#' environment, depending on whether the code is being executed in an R
#' Markdown document or a regular R script.
#'
#' @param x The name of the object to be created or modified.
#' @param value The value to be assigned to the object.
#'
#' @return None (The object is assigned in the appropriate environment).
#'
#' @examples
#' assign_env("result", 42)
#'
#' @importFrom base assign
#' @importFrom base getOption
#' @importFrom knitr knit_global

assign_env <- function(x, value) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    envir <- knitr::knit_global()
    env_type <- "knitr environment"
  } else {
    envir <- globalenv()
    env_type <- "global environment"
  }

  if (exists(x, envir = envir)) {
    assign(x = x, value = value, envir = envir)
    cat(paste0("Object '", x, "' is updated in ", env_type, ".\n"))
  } else {
    assign(x = x, value = value, envir = envir)
    cat(paste0("Object '", x, "' is imported in ", env_type, ".\n"))
  }
}
