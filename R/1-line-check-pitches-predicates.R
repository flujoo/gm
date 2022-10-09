#' Check If Object Is MIDI Note Number
#'
#' Character MIDI note numbers (e.g. `"60"`) are acceptable.
#'
#' @noRd
is_pitch_value <- function(x) {
  predicate <- function(x) {
    !is.na(x) && x >= 12 && x <= 127 && x == as.integer(x)
  }

  if (is.character(x)) {
    x <- suppressWarnings(as.numeric(x))
    predicate(x)

  } else if (is.numeric(x)) {
    predicate(x)

  } else {
    FALSE
  }
}
