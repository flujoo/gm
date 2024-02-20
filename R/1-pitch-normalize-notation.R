#' @keywords internal
#' @export
Pitch.character <- function(x, ...) {
  structure(
    parse_pitch_notation(x),
    class = "Pitch"
  )
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
