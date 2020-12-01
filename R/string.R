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
to_string.Tuple <- function(x, ...) {
  x %>%
    sapply(to_string) %>%
    paste(collapse = ", ") %>%
    paste0("(", ., ")")
}


#' @keywords internal
#' @export
to_string.Line <- function(x, ...) {
  x %>%
    sapply(to_string) %>%
    paste(collapse = ", ")
}


#' @keywords internal
#' @export
print.Printable <- function(x, ...) {
  x %>%
    to_string(...) %>%
    cat("\n")
}
