#' Various Validations for Tie Adding
#'
#' 1. Check if the start position exceeds the Line length.
#' 2. Check if it is a rest at the start position.
#' 3. Check if `j` exceeds the chord length.
#' 4. Check if the stop position exceeds the Line length.
#' 5. Check if it is a rest at the stop position.
#' 6. Check if the notes to be tied have equivalent pitches.
#'
#' @noRd
check_tie <- function(i, j, line, notes) {
  # the notes of the Line
  line_notes <- notes[notes$line == line, ]

  # the length of the Line
  line_length <- max(line_notes$i)

  # check if the start position of the Tie exceeds the Line length
  if (i > line_length) abort_tie_i(i, line, line_length)

  # the note/chord/rest at the start position
  start_chord <- line_notes[line_notes$i == i, ]

  # check if it is a rest at the start position
  is_rest <- anyNA(start_chord$pitch) && anyNA(start_chord$midi)
  if (is_rest) abort_tie_i_rest(i, line)

  # check if `j` exceeds the chord length
  chord_length <- nrow(start_chord)
  if (!is.na(j) && j > chord_length) abort_tie_j(j, i, line, chord_length)

  i_stop <- i + 1

  # check if the stop position exceeds the Line length
  if (i_stop > line_length) abort_tie_stop(i_stop, line, line_length)

  # the note/chord/rest at the stop position
  stop_chord <- line_notes[line_notes$i == i_stop, ]

  # check if it is a rest at the stop position
  is_rest <- anyNA(stop_chord$pitch) && anyNA(stop_chord$midi)
  if (is_rest) abort_tie_stop_rest(i, line)

  # check if the notes to be tied have equivalent pitches
  common_pitches <- intersect(start_chord$midi, stop_chord$midi)
  if (length(common_pitches) == 0) abort_tie_equivalent(i, line)
}


abort_tie_i <- function(i, line, line_length) {
  general <- paste(
    "The start position of the Tie",
    "must not exceed the Line length."
  )

  specifics <- sprintf(
    "`i` is %s, while the length of Line %s is %s.",
    i, line, line_length
  )

  erify::throw(general, specifics)
}


abort_tie_i_rest <- function(i, line) {
  general <- "Can not add a Tie to a rest."
  specifics <- sprintf("It is a rest at position %s of Line %s.", i, line)
  erify::throw(general, specifics)
}


abort_tie_j <- function(j, i, line, chord_length) {
  general <- "`j` must not exceed the chord length."

  specifics <- sprintf(
    "`j` is %s, while the chord length at position %s of Line %s is %s.",
    j, i, line, chord_length
  )

  erify::throw(general, specifics)
}


abort_tie_stop <- function(i_stop, line, line_length) {
  general <- paste(
    "The stop position of the Tie",
    "must not exceed the Line length."
  )

  specifics <- sprintf(
    "The stop position would be %s, while the length of Line %s is %s.",
    i_stop, line, line_length
  )

  erify::throw(general, specifics)
}


abort_tie_stop_rest <- function(i, line) {
  general <- "It must not be a rest after the Tie."
  specifics <- sprintf("It is a rest after position %s of Line %s.", i, line)
  erify::throw(general, specifics)
}


abort_tie_equivalent <- function(i, line) {
  general <- "The notes to be tied must have equivalent pitches."

  specifics <- paste(
    "The notes before and after the Tie added at position", i,
    "of Line", line,
    "do not have equivalent pitches."
  )

  erify::throw(general, specifics)
}
