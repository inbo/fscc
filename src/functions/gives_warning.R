
#' Check if a function gives a warning
#'
#' This function evaluates a given expression or test and checks if it
#' generates a warning.
#'
#' @param test The expression or test to be evaluated
#'
#' @return A logical value indicating whether the expression/test generated a warning
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
#' @seealso
#' The \code{\link{warnings}} function can be used to retrieve the warnings generated during the evaluation.
#'
#' @keywords warning check test
#' @export
gives_warning <- function(test) {
  
  # assign("last.warning", NULL, envir = baseenv()) # Clear the previous warning
  # 
  # test
  # 
  # if(!is.null(warnings())) {
  #   return(TRUE)} else {
  #     return(FALSE)}
  
  assign("last.warning", NULL, envir = baseenv())  # Clear the previous warning
  
  withCallingHandlers(
    expr = {
      test
    },
    warning = function(w) {
      assign("last.warning", w, envir = baseenv())  # Assign the warning to last.warning
      invokeRestart("muffleWarning")  # Muffle the warning to prevent it from being printed
    }
  )
  
  !is.null(get("last.warning", envir = baseenv()))  # Check if a warning was captured
  
}