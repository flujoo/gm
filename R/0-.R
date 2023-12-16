url_musicxml <- "https://w3c.github.io/musicxml/musicxml-reference/"


#' @keywords internal
#' @export
to_string <- function(x, ...) {
  UseMethod("to_string")
}


#' @keywords internal
#' @export
to_value <- function(x) {
  UseMethod("to_value")
}
