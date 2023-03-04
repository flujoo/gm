#' Normalize Pitch Value or Notation to `Pitch` Object
#'
#' @keywords internal
#' @export
Pitch <- function(x, ...) {
  UseMethod("Pitch")
}


#' @keywords internal
#' @export
Pitch.character <- function(x, ...) {
  pitch <- parse_pitch_notation(x)
  class(pitch) <- "Pitch"
  pitch
}


parse_pitch_notation <- function(pitch) {
  pitch <- trimws(pitch)
  l <- nchar(pitch)

  step <- toupper(substr(pitch, 1, 1))
  accidental <- substr(pitch, 2, l - 1)
  alter <- (-2:2)[accidental == c("--", "-", "", "#", "##")]
  octave <- as.integer(substr(pitch, l, l))

  list(step = step, alter = alter, octave = octave)
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


#' @keywords internal
#' @export
to_value.Pitch <- function(x) {
  steps <- c("C", "D", "E", "F", "G", "A", "B")
  step_values <- c(0, 2, 4, 5, 7, 9, 11)
  step_value <- step_values[x$step == steps]

  as.integer(step_value + x$alter + (x$octave + 1) * 12)
}
