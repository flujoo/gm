#' Normalize Duration Value or Notation to `Duration` Object
#'
#' @keywords internal
#' @export
Duration <- function(x, ...) {
  UseMethod("Duration")
}
