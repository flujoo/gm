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

  ties <- music$ties

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
check_stop_used <- function(i, j, chord_length, ties, pitches_i,
                            pitches_stop) {
  # if it is a note at the start position,
  # then there is no other note to compete with it
  pass <- chord_length == 1 ||
    # if `j` is not specified, then there is no other note to compete
    # with the note or chord at position `i` as a whole
    is.null(j) ||
    # if the start position is already in `ties`, then no note can
    # take its tied note at the stop position
    any(ties$type == "start" & ties$i == i & ties$j == j)

  if (pass) {
    return(invisible())
  }

  value_start <- pitches_i[pitches_i$j == j, ]$value

  for (k in seq_len(nrow(pitches_stop))) {
    # when there is some pitch at the stop position is equivalent to
    # the pitch at the start position,
    pass <- pitches_stop$value[k] == value_start &&
      # and this pitch is not used
      !any(ties$type == "stop" & ties$i == i + 1 &
             ties$j == pitches_stop$j[k])

    if (pass) {
      return(invisible())
    }
  }

  general <- "Any note can be tied with only one note."

  s_ij <- paste0("(", i, ", ", j, ")")

  specific <- paste(
    "Can't find note after position {s_ij} that is untied",
    "and has equivalent pitch."
  )

  class <- "stop_position_used"
  erify::throw(general, specific, environment(), class = class)
}


initialize_ties <- function() {
  tibble::tibble(
    line = integer(),
    i = integer(),
    j = integer(),
    type = character(),
    tie = list()
  )
}


add_tie <- function(ties, chord_length, i, j, pitches_i, pitches_stop,
                    object, line) {
  # normalize `j`
  if (is.null(j)) {
    j <- 1L
  }

  # initialize `j_stop`
  j_stop <- NA_integer_

  # return if the start position is already in `ties`
  if (any(ties$type == "start" & ties$i == i & ties$j == j)) {
    return(ties)
  }

  value_start <- pitches_i[pitches_i$j == j, ]$value

  # set `j_stop`
  if (chord_length == 1) {
    j_stop <- pitches_stop[pitches_stop$value == value_start, ]$j[1]

  } else if (chord_length > 1) {
    for (k in seq_len(nrow(pitches_stop))) {
      value_stop <- pitches_stop$value[k]
      j_k <- pitches_stop$j[k]

      con <- (value_stop == value_start) &&
        !any(ties$type == "stop" & ties$i == i + 1 & ties$j == j_k)

      if (con) {
        j_stop <- j_k
        break
      }
    }
  }

  if (is.na(j_stop)) {
    return(ties)
  }

  # add the start position
  ties %<>% tibble::add_case(
    line = line,
    i = i,
    j = j,
    type = "start",
    tie = list(object)
  )

  # add the stop position
  ties %<>% tibble::add_case(
    line = line,
    i = i + 1L,
    j = j_stop,
    type = "stop",
    tie = list(object)
  )

  ties
}
