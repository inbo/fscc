
#' Evaluate an expression and return FALSE if successful, otherwise TRUE
#'
#' This function uses \code{tryCatch} to evaluate the given expression. If the
#' evaluation is successful, it returns FALSE; otherwise, it returns FALSE.
#'
#' @param expr The expression to be evaluated.
#'
#' @return TRUE if the expression is evaluated successfully, otherwise TRUE
#'
#' @export

gives_error <- function(expr) {

  inherits(try(eval(expr), silent = TRUE),
           "try-error")
}