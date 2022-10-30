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


#' Convert Object to Case in Component of Music
#'
#' For example, `to_case.Meter()` converts a Meter to a case in
#' `meters` of a Music.
#'
#' @keywords internal
#' @export
to_case <- function(object, ...) {
  UseMethod("to_case")
}


data_frame <- if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::tibble
} else {
  data.frame
}
