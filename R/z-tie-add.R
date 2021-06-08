#' @keywords internal
#' @export
add.Tie <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_to_exist(to, lines, "Tie")

  # unpack `$position`
  position <- object$position
  i <- position[1]
  j <- position[2]

  # get the row number of Line `to`
  line <- locate_line(to, lines)

  # get the notes of Line `to`
  notes <- music$notes
  notes_line <- notes[notes$line == line, ]

  # get the length of Line `to`
  line_length <- max(notes_line$i)

  s_to <- signify_to(to)

  # check if the tie's start position is beyond the Line length
  check_i_line_length(i, line_length, s_to)

  # get notes at position `i`
  notes_i <- notes_line[notes_line$i == i, ]

  # check if it's a rest at the tie's start position
  check_i_rest(notes_i, i, s_to)

  # check if the tie's start position is beyond the chord length
  check_j_chord_length(j, notes_i, i, s_to)

  # check if the tie's stop position is beyond the Line length
  check_stop_line_length(i, line_length, s_to)

  # get notes at the tie's stop position
  notes_stop <- notes_line[notes_line$i == i + 1, ]

  # check if it's a rest at the tie's stop position
  check_stop_rest(notes_stop, i, s_to)
}


# check if the tie's start position is beyond the Line length
check_i_line_length <- function(i, line_length, s_to) {
  if (i <= line_length) {
    return(invisible())
  }

  general <-
    "`position[1]` in `Tie()` must not be larger than the Line length."

  specific <-
    "`position[1]` is {i}, but the length of Line {s_to} is {line_length}."

  class <- "start_position_beyond_line_length"
  erify::throw(general, specific, environment(), class = class)
}


# check if it's a rest at the tie's start position
check_i_rest <- function(notes_i, i, s_to) {
  pitch <- notes_i$pitch[[1]]

  if (!is.null(pitch)) {
    return(invisible())
  }

  general <- "`position` in `Tie()` must not refer to a rest in the Music."
  specific <- "It's a rest at position {i} in Line {s_to}."
  class <- "rest_at_start_position"
  erify::throw(general, specific, environment(), class = class)
}


# check if the tie's start position is beyond the chord length
check_j_chord_length <- function(j, notes_i, i, s_to) {
  if (is.na(j)) {
    return(invisible())
  }

  chord_length <- nrow(notes_i)

  if (j <= chord_length) {
    return(invisible())
  }

  general <-
    "`position[2]` in `Tie()` must not be larger than the chord length."

  specific <- paste(
    "`position[2]` is {j},",
    "but the length of the chord at position {i} in Line {s_to}",
    "is {chord_length}."
  )

  class <- "start_position_beyond_chord_length"
  erify::throw(general, specific, environment(), class = class)
}


# check if the tie's stop position is beyond the Line length
check_stop_line_length <- function(i, line_length, s_to) {
  if (i + 1 <= line_length) {
    return(invisible())
  }

  general <- paste(
    "There must be a note after the position indicated",
    "by `position` in `Tie()`."
  )

  specific <- paste(
    "`position[1]` is {i},",
    "but the length of Line {s_to} is also {line_length}."
  )

  class <- "stop_position_beyond_line_length"
  erify::throw(general, specific, environment(), class = class)
}


# check if it's a rest at the tie's stop position
check_stop_rest <- function(notes_stop, i, s_to) {
  pitch <- notes_stop$pitch[[1]]

  if (!is.null(pitch)) {
    return(invisible())
  }

  general <- paste(
    "There must be a note after the position indicated",
    "by `position` in `Tie()`."
  )

  specific <- "It's a rest after position {i} in Line {s_to}."
  class <- "rest_at_stop_position"
  erify::throw(general, specific, environment(), class = class)
}
