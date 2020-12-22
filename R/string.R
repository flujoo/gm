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
to_string.List <- function(x, collapse = ", ", ...) {
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


shorten_string <- function(string, width) {
  l <- nchar(string)

  if (l > width) {
    string <- string %>%
      substr(1, width) %>%
      paste("...")
  }

  string
}


generate_string <- function(general, specifics, env) {
  specifics %>%
    sapply(function(s) paste("*", s)) %>%
    paste(collapse = "\n") %>%
    {ifelse(. == "", "", paste0("\n\n", .))} %>%
    glue::glue(general, ., .envir = env)
}
