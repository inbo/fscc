
#' Check if a function gives a warning
#'
#' This function evaluates a given expression or test and checks if it
#' generates a warning.
#'
#' @param test The expression or test to be evaluated
#'
#' @return A logical value indicating whether the expression/test generated
#' a warning
#'
#' @details
#' This function clears the previous warning by assigning `NULL` to the
#' `last.warning` variable in the base environment. It then evaluates the
#' provided \code{test} expression/test. If any warnings are generated during
#' the evaluation, the function returns \code{TRUE}; otherwise, it returns
#' \code{FALSE}.
#'
#' @examples
#' # Check if the division by zero generates a warning
#' gives_warning(dir.create("./data/pir_checked/"))  # Returns TRUE
#'
#' # Check if a normal calculation generates a warning
#' gives_warning(1 + 2)  # Returns FALSE
#'
#'
#' @keywords warning check test
#' @export

# gives_warning <- function(test) {
#
#   assign("last.warning", NULL, envir = baseenv())  # Clear the previous warning
#
#   withCallingHandlers(
#     expr = {
#       test
#     },
#     warning = function(w) {
#       # Assign the warning to last.warning
#       assign("last.warning", w, envir = baseenv())
#       # Muffle the warning to prevent it from being printed
#       invokeRestart("muffleWarning")
#     }
#   )
#
#   # Check if a warning was captured
#   !is.null(get("last.warning", envir = baseenv()))
#
# }


gives_warning <- function(test) {
  # Clear the last warning
  last_warning <- NULL

  # Evaluate the expression and capture warnings/errors
  result <- tryCatch({
    withCallingHandlers(
      expr = {
        test
      },
      warning = function(w) {
        last_warning <<- w
        invokeRestart("muffleWarning")
      }
    )
  }, warning = function(w) {
    last_warning <<- w
    invokeRestart("muffleWarning")
  }, error = function(e) {
    NULL
  })

  # Check if a warning was captured
  result_warning <- !is.null(last_warning)

  return(result_warning)
}
