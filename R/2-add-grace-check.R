#' Various Validations for Grace Adding
#'
#' 1. Check if `i` is not less than the Line length.
#' 2. Check if it is a rest at position `i`.
#' 3. Check if it is a rest after position `i`.
#'
#' @noRd
check_grace <- function(i, line, notes) {
  # the notes of the Line
  line_notes <- notes[notes$line == line, ]

  # the length of the Line
  line_length <- max(line_notes$i)

  # check if `i` exceeds the Line length
  if (i >= line_length) abort_grace_i(i, line, line_length)

  # the note/chord/rest at position `i`
  grace <- line_notes[line_notes$i == i, ]

  # check if it is a rest at position `i`
  if (anyNA(grace$pitch) && anyNA(grace$midi)) abort_grace_i_rest(i, line)

  # the note/chord/rest after position `i`
  graced <- line_notes[line_notes$i == i + 1, ]

  # check if it is a rest after position `i`
  is_rest <- anyNA(graced$pitch) && anyNA(graced$midi)
  if (is_rest) abort_grace_after_rest(i, line)
}


abort_grace_i <- function(i, line, line_length) {
  general <- "`i` must be less than the Line length."

  specifics <- sprintf(
    "`i` is %s, while the length of Line %s is %s.",
    i, line, line_length
  )

  erify::throw(general, specifics)
}


abort_grace_i_rest <- function(i, line) {
  general <- "Can not add a Grace to a rest."
  specifics <- sprintf("It is a rest at position %s of Line %s.", i, line)
  erify::throw(general, specifics)
}


abort_grace_after_rest <- function(i, line) {
  general <- "It must not be a rest after the Grace."
  specifics <- sprintf("It is a rest after position %s of Line %s.", i, line)
  erify::throw(general, specifics)
}
