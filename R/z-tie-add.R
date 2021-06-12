#' @keywords internal
#' @export
add.Tie <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_to_exist(to, lines, "Tie")

  i <- object$i
  j <- object$j

  # get the row number of Line `to`
  line <- locate_line(to, lines)

  # get the pitches of Line `to`
  pitches <- music$pitches
  pitches_line <- pitches[pitches$line == line, ]

  # get the length of Line `to`
  line_length <- max(pitches_line$i)

  s_to <- signify_to(to)

  # check if the tie's start position is beyond the Line length
  check_i_line_length(i, line_length, s_to)

  # get pitches at position `i`
  pitches_i <- pitches_line[pitches_line$i == i, ]

  # check if it's a rest at the tie's start position
  check_i_rest(pitches_i, i, s_to)

  # get the length of `pitches_i`
  chord_length <- nrow(pitches_i)

  # check if the tie's start position is beyond the chord length
  check_j_chord_length(j, chord_length, i, s_to)

  # check if the tie's stop position is beyond the Line length
  check_stop_line_length(i, line_length, s_to)

  # get pitches at the tie's stop position
  pitches_stop <- pitches_line[pitches_line$i == i + 1, ]

  # check if it's a rest at the tie's stop position
  check_stop_rest(pitches_stop, i, s_to)

  # check if any note at the stop position has equivalent pitch
  check_equivalent_pitch(pitches_i, pitches_stop, chord_length, j, i, s_to)

  local <- music$local

  check_stop_used(i, j, chord_length, local, notes_i, notes_stop)

  # expand `position` and put the output into a list
  positions <- normalize_tie_position(i, j, chord_length, position)
}


# check if the tie's start position is beyond the Line length
check_i_line_length <- function(i, line_length, s_to) {
  if (i <= line_length) {
    return(invisible())
  }

  general <- "`i` in `Tie()` must not be larger than the Line length."
  specific <- "`i` is {i}, but the length of Line {s_to} is {line_length}."
  class <- "start_position_beyond_line_length"
  erify::throw(general, specific, environment(), class = class)
}


# check if it's a rest at the tie's start position
check_i_rest <- function(pitches_i, i, s_to) {
  pitch <- pitches_i$pitch[[1]]

  if (!is.null(pitch)) {
    return(invisible())
  }

  general <- "`i` in `Tie()` must not refer to a rest in the Music."
  specific <- "It's a rest at position {i} in Line {s_to}."
  class <- "rest_at_start_position"
  erify::throw(general, specific, environment(), class = class)
}


# check if the tie's start position is beyond the chord length
check_j_chord_length <- function(j, chord_length, i, s_to) {
  if (is.null(j) || j <= chord_length) {
    return(invisible())
  }

  general <- paste(
    "`j` in `Tie()` must not be larger than",
    "the number of notes at position `i`."
  )

  specific <- paste(
    "`j` is {j},",
    "but the number of notes at position {i} in Line {s_to}",
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
    "There must be some note after the position indicated",
    "by `i` in `Tie()`."
  )

  specific <- paste(
    "`i` is {i},",
    "but the length of Line {s_to} is also {line_length}."
  )

  class <- "stop_position_beyond_line_length"
  erify::throw(general, specific, environment(), class = class)
}


# check if it's a rest at the tie's stop position
check_stop_rest <- function(pitches_stop, i, s_to) {
  pitch <- pitches_stop$pitch[[1]]

  if (!is.null(pitch)) {
    return(invisible())
  }

  general <- paste(
    "There must be some note after the position indicated",
    "by `i` in `Tie()`."
  )

  specific <- "It's a rest after position {i} in Line {s_to}."
  class <- "rest_at_stop_position"
  erify::throw(general, specific, environment(), class = class)
}


# expand `position` and put the output into a list
normalize_tie_position <- function(i, j, chord_length, position) {
  if (!is.na(j) && chord_length == 1) {
    list(i)
  } else if (is.na(j) && chord_length > 1) {
    Map(c, i, 1:chord_length)
  } else {
    list(position)
  }
}


# check if any note at the stop position has equivalent pitch
check_equivalent_pitch <- function(pitches_i, pitches_stop, chord_length, j,
                                   i, s_to) {
  if (chord_length > 1 && !is.null(j)) {
    pitches_i <- pitches_i[pitches_i$j == j, ]
  }

  pass <- intersect(pitches_i$value, pitches_stop$value) %>%
    length() %>%
    as.logical()

  if (pass) {
    return(invisible())
  }

  general <- "The notes a tie connects must have equivalent pitches."

  specific <- paste(
    "Can't find note with equivalent pitch after position {i}",
    "in Line {s_to}."
  )

  class <- "no_equivalent_pitch"
  erify::throw(general, specific, environment(), class = class)
}


# suppose you have two notes with the same pitch at the start position,
# and only one note with the same pitch at the stop position,
# then you can only add one tie to connect one of the notes at the start
# position, with the note at the stop position
check_stop_used <- function(i, j, chord_length, local, notes_i, notes_stop) {
  # when this should be a concern
  con <- chord_length > 1 && !is.na(j) &&
    is.null(locate_tie(local, i, j, "start"))

  if (!con) {
    return(invisible())
  }

  pv_start <- notes_i[notes_i$j == j, ]$pv

  for (k in seq_len(nrow(notes_stop))) {
    pass <- notes_stop$pv[k] == pv_start &&
      is.null(locate_tie(local, i + 1, notes_stop$j[k], "stop"))

    if (pass) {
      return(invisible())
    }
  }

  general <- "Any note can be tied with only one note."

  specific <- paste(
    "Can't find note after position {i} that is untied",
    "and has equivalent pitch."
  )

  class <- "stop_position_used"
  erify::throw(general, specific, environment(), class = class)
}


locate_tie <- function(local, i, j, type) {
  for (k in seq_len(nrow(local))) {
    object <- local$object[[k]]

    con <- inherits(object, "Tie") &&
      object$type == type &&
      local$i[k] == i &&
      local$j[k] == j

    if (con) {
      return(k)
    }
  }

  # `NULL` is returned otherwise
}
