#' Normalize Duration Value or Notation to `Duration` Object
#'
#' @keywords internal
#' @export
to_Duration <- function(x, ...) {
  UseMethod("to_Duration")
}
