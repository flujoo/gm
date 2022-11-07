#' @keywords internal
#' @export
to_string <- function(x, ...) {
  UseMethod("to_string")
}


#' @keywords internal
#' @export
to_value <- function(x, ...) {
  UseMethod("to_value")
}


data_frame <- if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::tibble
} else {
  data.frame
}


capitalize <- function(x) {
  paste0(
    toupper(substr(x, 1, 1)),
    substr(x, 2, nchar(x))
  )
}
