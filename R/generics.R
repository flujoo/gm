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


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}


data_frame <- if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::tibble
} else {
  data.frame
}
