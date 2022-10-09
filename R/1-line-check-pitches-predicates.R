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


#' Check If Object Is Pitch Notation
#' @noRd
is_pitch_notation <- function(x) {
  if (!is.character(x)) return(FALSE)

  reg <- paste0(
    "^",
    # a valid pitch notation always starts with a note name
    # either in uppercase or lowercase
    "([A-G]|[a-g])",
    # maybe followed by an accidental
    "(#{0,2}|-{0,2})",
    # followed by an octave
    "[0-9]",
    "$"
  )

  grepl(reg, x)
}
