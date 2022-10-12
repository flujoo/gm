#' Normalize Pitch Value or Notation to `Pitch` Object
#'
#' @keywords internal
#' @export
Pitch <- function(x, ...) {
  UseMethod("Pitch")
}


#' @keywords internal
#' @export
print.Pitch <- function(x, ...) {
  cat(to_string(x), "\n")
}
