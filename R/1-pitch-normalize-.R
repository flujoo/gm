#' Normalize Pitch Value or Notation to `Pitch` Object
#'
#' @keywords internal
#' @export
to_Pitch <- function(x, ...) {
  UseMethod("to_Pitch")
}
