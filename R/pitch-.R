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


#' @keywords internal
#' @export
to_string.Pitch <- function(x, ...) {
  accidental <- c("--", "-", "", "#", "##")[x$alter == -2:2]
  paste0(x$step, accidental, x$octave)
}
