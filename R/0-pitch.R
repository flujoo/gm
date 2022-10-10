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
    "^", "\\s*",
    # a valid pitch notation always starts with a note name
    # either in uppercase or lowercase
    "([A-G]|[a-g])",
    # maybe followed by an accidental
    "(#{0,2}|-{0,2})",
    # followed by an octave
    "[0-9]",
    "\\s*", "$"
  )

  grepl(reg, x)
}


parse_pitch_notation <- function(notation) {
  notation <- trimws(notation)
  l <- nchar(notation)

  step <- toupper(substr(notation, 1, 1))
  accidental <- substr(notation, 2, l - 1)
  alter <- (-2:2)[accidental == c("--", "-", "", "#", "##")]
  octave <- as.integer(substr(notation, l, l))

  list(step = step, alter = alter, octave = octave)
}


#' Convert Pitch Notation to MIDI Note Number
#' @noRd
to_value_pitch <- function(notation) {
  pitch <- parse_pitch_notation(notation)
  step_notations <- c("C", "D", "E", "F", "G", "A", "B")
  step_values <- c(0, 2, 4, 5, 7, 9, 11)
  step_value <- step_values[pitch$step == step_notations]
  as.integer(step_value + pitch$alter + (pitch$octave + 1) * 12)
}
