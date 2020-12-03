#' @keywords internal
#' @export
to_string <- function(x, ...) {
  UseMethod("to_string")
}


#' @keywords internal
#' @export
to_string.default <- function(x, ...) {
  x
}


#' @keywords internal
#' @export
to_string.Tuple <- function(x, left = "(", right = ")", ...) {
  x %>%
    sapply(to_string) %>%
    paste(collapse = ", ") %>%
    paste0(left, ., right)
}


#' @keywords internal
#' @export
to_string.Line <- function(x, collapse = ", ", ...) {
  x %>%
    sapply(to_string) %>%
    paste(collapse = collapse)
}


#' @keywords internal
#' @export
print.Printable <- function(x, ...) {
  x %>%
    to_string(...) %>%
    cat("\n")
}
