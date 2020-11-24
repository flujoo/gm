#' @keywords internal
#' @export
to_string <- function(x, ...) {
  UseMethod("to_string")
}


to_string.chord <- function(x, ...) {
  x %>%
    sapply(to_string) %>%
    paste(collapse = ", ") %>%
    paste0("(", ., ")")
}


to_string.line <- function(x, ...) {
  x %>%
    sapply(to_string) %>%
    paste(collapse = ", ")
}


print._ <- function(x, ...) {
  x %>%
    to_string(...) %>%
    cat("\n")
}
