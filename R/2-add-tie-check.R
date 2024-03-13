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
  notes_line <- notes[notes[["line"]] == line, ]

  n_line <- max(notes_line[["i"]])
  if (i > n_line) abort_tie_i(i, line, n_line)

  notes_start <- notes_line[notes_line[["i"]] == i, ]
  if (anyNA(notes_start[["midi"]])) abort_tie_i_rest(i, line)

  n_start <- NROW(notes_start)
  if (!is.na(j) && j > n_start) abort_tie_j(j, i, line, n_start)

  i_stop <- i + 1
  if (i_stop > n_line) abort_tie_stop(i_stop, line, n_line)

  notes_stop <- notes_line[notes_line[["i"]] == i_stop, ]
  if (anyNA(notes_stop[["midi"]])) abort_tie_stop_rest(i, line)

  if (!is.na(j) && n_start > 1) {
    notes_start <- notes_start[notes_start[["j"]] == j, ]
  }

  common_pitches <- intersect(notes_start[["midi"]], notes_stop[["midi"]])
  if (length(common_pitches) == 0) abort_tie_equivalent(i, j, line)
}


abort_tie_i <- function(i, line, n_line) {
  general <- paste(
    "The start position of the Tie",
    "must not exceed the Line length."
  )

  specifics <- sprintf(
    "`i` is %s, while the length of Line %s is %s.",
    i, line, n_line
  )

  erify::throw(general, specifics)
}


abort_tie_i_rest <- function(i, line) {
  general <- "Can not add a Tie to a rest."
  specifics <- sprintf("It is a rest at position %s of Line %s.", i, line)
  erify::throw(general, specifics)
}


abort_tie_j <- function(j, i, line, n_start) {
  general <- "`j` must not exceed the chord length."

  specifics <- sprintf(
    "`j` is %s, while the chord length at position %s of Line %s is %s.",
    j, i, line, n_start
  )

  erify::throw(general, specifics)
}


abort_tie_stop <- function(i_stop, line, n_line) {
  general <- paste(
    "The stop position of the Tie",
    "must not exceed the Line length."
  )

  specifics <- sprintf(
    "The stop position would be %s, while the length of Line %s is %s.",
    i_stop, line, n_line
  )

  erify::throw(general, specifics)
}


abort_tie_stop_rest <- function(i, line) {
  general <- "It must not be a rest after the Tie."
  specifics <- sprintf("It is a rest after position %s of Line %s.", i, line)
  erify::throw(general, specifics)
}


abort_tie_equivalent <- function(i, j, line) {
  general <- "The notes to be tied must have equivalent pitches."

  specifics <- paste(
    "The notes at and after position",
    if (is.na(j)) i else sprintf("(%s, %s)", i, j),
    "of Line", line,
    "do not have equivalent pitches."
  )

  erify::throw(general, specifics)
}
