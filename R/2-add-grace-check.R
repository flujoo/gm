#' Various Validations for Grace Adding
#'
#' 1. Check if `i` is not less than the Line length.
#' 2. Check if it is a rest, a tuplet, or a dotted duration at position `i`.
#' 3. Check if it is a rest after position `i`.
#'
#' @noRd
check_grace <- function(i, line, notes) {
  # the notes of the Line
  notes <- notes[notes$line == line, ]

  # the length of the Line
  l <- max(notes$i)

  # check if `i` exceeds the Line length
  if (i >= l) {
    general <- "`i` must be less than the Line length."
    specifics <- sprintf("`i` is %s, while the Line length is %s.", i, l)
    erify::throw(general, specifics)
  }

  # the note/chord/rest at position `i`
  grace <- notes[notes$i == i, ]

  # check the note/chord/rest at position `i`
  general <- "Can not add a Grace to a rest, a tuplet, or a dotted duration."
  specifics <- character()

  if (anyNA(grace$pitch) && anyNA(grace$midi)) {
    specifics <- sprintf("It is a rest at position %s.", i)

  } else if (grepl("/", grace$duration[1])) {
    specifics <- sprintf("It is a tuplet at position %s.", i)

  } else if (!(grace$length[1] %in% duration_types$value)) {
    specifics <- sprintf("It is a dotted duration at position %s.", i)
  }

  erify::throw(general, specifics)

  # the note/chord/rest after position `i`
  graced <- notes[notes$i == i + 1, ]

  # check if it is a rest after position `i`
  if (anyNA(graced$pitch) && anyNA(graced$midi)) {
    general <- "It must not be a rest after the Grace."
    specifics <- sprintf("It is a rest after position %s.", i)
    erify::throw(general, specifics)
  }
}
