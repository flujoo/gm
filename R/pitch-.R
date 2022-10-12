#' Normalize Pitch Value or Notation to `Pitch` Object
#'
#' @keywords internal
#' @export
Pitch <- function(x, ...) {
  UseMethod("Pitch")
}
