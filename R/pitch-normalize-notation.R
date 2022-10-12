#' @keywords internal
#' @export
Pitch.character <- function(x, ...) {
  x <- trimws(x)
  l <- nchar(x)

  step <- toupper(substr(x, 1, 1))
  accidental <- substr(x, 2, l - 1)
  alter <- (-2:2)[accidental == c("--", "-", "", "#", "##")]
  octave <- as.integer(substr(x, l, l))

  pitch <- list(step = step, alter = alter, octave = octave)
  class(pitch) <- "Pitch"
  pitch
}
