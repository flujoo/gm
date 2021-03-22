#' @title Create `Line` Object
#'
#' @description Create a `Line` object.
#'
#' `Line` objects represent musical lines.
#'
#' @param pitches A list whose members are
#' 1. single pitch notations, like "C4", to represent the pitch contents of
#' notes,
#'
#' 2. single MIDI note numbers, like 60 or "60", also to represent the pitch
#' contents of notes,
#'
#' 3. single logical `NA`s to represent the pitch contents of rests, or
#'
#' 4. vectors of pitch notations and MIDI note numbers, like `c("C4", "61")`,
#' to represent the pitch contents of chords.
#'
#' @param durations A list whose members are
#' 1. single duration notations or their abbreviations, like "quarter" or
#' just "q",
#'
#' 2. single duration values, like 1, which is equivalent to "quarter", or
#'
#' 3. `Duration` objects returned by `tuplet()`, which is used to create
#' complex tuplets.
#'
#' @param tie Optional. A list of indices of argument `pitches`, which
#' indicates at which positions to add ties.
#'
#' @param name Optional. A single character to name the `Line` object.
#'
#' @param as Optional. "part", "staff" or "voice", to specify the state
#' of the `Line` object. The default value is "part".
#'
#' @param to Optional. An index or a `Line` name, which indicates with which
#' `Line` object as the reference to add the `Line` object.
#'
#' @param after Optional. A single logical which indicates whether to add the
#' `Line` object after or before a reference `Line` object. The default value
#' is `TRUE`.
#'
#' @param bar Optional. A positive integer which indicates the number of
#' the measure to which to insert the `Line` object. By default, a `Line`
#' object will be inserted to the first measure.
#'
#' @param offset Optional. A duration value, sum of duration values or 0,
#' which indicates the position in a measure, at which to insert the `Line`
#' object. The default value is 0.
#'
#' @return A list with class `Line`.
#'
#' @seealso [gm::+.Music()] for adding `Line` objects to a `Music` object.
#'
#' `vignette("gm", package = "gm")` for more details about `Line` objects.
#'
#' @examples
#' # create a Music object
#' m <- Music() + Meter(4, 4) + Line(list("C4"), list(8), name = "a")
#'
#' # create a Line object
#' l <- Line(
#'   pitches = list("C5", "C5", "C5"),
#'   durations = list(1, 1, 1),
#'
#'   # tie the first two notes
#'   tie = list(1),
#'
#'   # add the Line as a voice
#'   as = "voice",
#'
#'   # with Line "a" as reference
#'   to = "a",
#'
#'   # before Line "a"
#'   after = FALSE,
#'
#'   # insert the Line to bar 2 with offset 1
#'   bar = 2,
#'   offset = 1
#' )
#' l
#'
#' # add the Line object to the Music object
#' m <- m + l
#' m
#'
#' \donttest{show(m)}
#' @export
Line <- function(pitches, durations, tie = NULL, name = NULL, as = NULL,
                 to = NULL, after = NULL, bar = NULL, offset = NULL) {
  # check and normalize `pitches`, `durations` and `tie` ------------------
  # normalize `pitches`
  if (class(pitches)[1] != "PitchLine") {
    pitches <- PitchLine(pitches)
  }

  # normalize `durations`
  if (class(durations)[1] != "DurationLine") {
    durations <- DurationLine(durations)
  }

  # unpack
  ps <- pitches$pitches
  ds <- durations$durations

  # check if `ps` and `ds` have same length
  check_same_length(ps, ds, "pitches", "durations")

  tie %<>% normalize_line_tie(ps)


  # check other arguments -------------------------------------------------
  if (!is.null(name)) {
    check_name(name)
  }

  check_line_as(as)

  if (!is.null(to)) {
    check_line_to(to)
  }

  check_line_after(after)

  if (!is.null(bar)) {
    check_positive_integer(bar)
  }

  check_line_offset(offset)


  # create Line -----------------------------------------------------------
  list(
    pitches = pitches,
    durations = durations,
    tie = tie,
    name = name,
    as = as,
    to = to,
    after = after,
    bar = bar,
    offset = offset
  ) %>% `class<-`("Line")
}


#' @export
print.Line <- function(x, context = "console", silent = FALSE, i, ...) {
  # rationale behind string generation ------------------------------------
  # to make the output as concise as possible,
  # do not print a component, if it is `NULL`
  # the same goes for Key, Meter and Music


  # initialize `general` and `specifics` ----------------------------------
  general <- "Line"
  specifics <- character(0)


  # convert `x$pitches` and `x$durations` to string -----------------------
  ps <- x$pitches
  ds <- x$durations

  # get length
  . <- ps$pitches
  l <- length(.)
  # check whether the first pitch has length 1
  .. <- .[[1]]
  l_1 <- length(..)

  s_ps_pre <- ifelse(l == 1 && l_1 == 1, "pitch", "pitches")
  s_ps <- ps %>%
    print(silent = TRUE) %>%
    paste("of", s_ps_pre, .) %>%
    shorten_string(globals$width)

  s_ds_pre <- ifelse(l == 1, "duration", "durations")
  s_ds <- ds %>%
    print(context = "inside", silent = TRUE) %>%
    paste("of", s_ds_pre, .) %>%
    shorten_string(globals$width)


  # generate string about Line length -------------------------------------
  s_l <- "of length {l}"


  # convert `x$tie` to string ---------------------------------------------
  tie <- x$tie
  if (is.null(tie)) {
    s_tie <- NULL
  } else {
    s_tie <-
      paste(
        "tied at",
        ifelse(length(tie$positions) == 1, "position", "positions"),
        print(tie, silent = TRUE)
      ) %>%
      shorten_string(globals$width)
  }


  # convert `x$name` to string --------------------------------------------
  name <- x$name
  if (is.null(name)) {
    s_name <- NULL
  } else {
    s_name <- 'of name "{name}"'
  }


  # convert `x$bar` and `x$offset` to string ------------------------------
  bar <- x$bar
  offset <- x$offset
  s_bar <- "to be inserted in bar {bar}"
  s_offset <- "with offset {offset}"

  if (!is.null(bar)) {
    if (is.null(offset)) {
      s_bar_offset <- s_bar
    } else {
      s_bar_offset <- paste(s_bar, s_offset)
    }
  } else {
    if (!is.null(offset)) {
      bar <- 1
      s_bar_offset <- paste(s_bar, s_offset)
    } else {
      s_bar_offset <- NULL
    }
  }


  # convert `x` to string -------------------------------------------------
  if (context == "inside") {
    # convert `x$number` to string
    number <- x$number
    if (is.null(number)) {
      s_number <- NULL
    } else {
      s_number <- "as part {number[1]} staff {number[2]} voice {number[3]}"
    }

    # adjust `general` and `specifics`
    general <- paste(general, i)
    specifics <- c(
      s_number, s_l, s_ps, s_ds, s_tie, s_name, s_bar_offset
    )

  } else if (context == "console") {
    # convert `x$as` to string
    as <- x$as
    if (is.null(as)) {
      s_as <- NULL
    } else {
      s_as <- "as a {as}"
    }

    # convert `x$to` and `x$after` to string
    to <- x$to
    after <- x$after
    s_after <- ifelse(is.null(after) || after == TRUE, "after", "before")

    if (!is.null(to)) {
      if (is.character(to)) {
        s_after_to <- 'to be inserted {s_after} Line "{to}"'
      } else if (is.numeric(to)) {
        s_after_to <- "to be inserted {s_after} Line {to}"
      }
    } else {
      if (!is.null(after)) {
        # if `to` is not specified, always insert the Line AFTER the last Line,
        # no matter how `after` is specified
        s_after_to <- "to be inserted after the last Line"
      } else {
        s_after_to <- NULL
      }
    }

    # adjust `specifics`
    specifics <- c(
      s_l, s_ps, s_ds, s_tie, s_name, s_bar_offset, s_as, s_after_to
    )
  }


  # print or return string ------------------------------------------------
  s <- generate_string(general, specifics, environment())

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


#' @keywords internal
#' @export
add.Line <- function(term, music) {
  # unpack `music`
  lines <- music$lines
  names <- lines %>%
    sapply(function(line) line$name) %>%
    unlist()
  l <- length(lines)

  # unpack `term`
  name <- term$name
  to <- term$to

  # check `term$name`, `term$to`
  check_line_name_unique(name, names)
  check_line_to_exist(to, names, l)

  # initialize `music$lines`
  if (l == 0) {
    term$number <- c(1, 1, 1)
    music$lines <- list(term)
    return(music)
  }

  # unpack `term` again
  as <- term$as
  after <- term$after

  # add the Line to the end if `to` is not specified
  # `after` is ignored, or it would be very confusing
  if (is.null(to)) {
    term$number <- generate_line_number(lines[[l]]$number, as, TRUE, to)
    music$lines <- c(lines, list(term))
    return(music)
  }

  # add the Line to a specified `to`
  # get `number` of the Line `to` refers to
  number <- get_to_number(lines, to, l)
  # generate `number` in `term`
  term$number <- generate_line_number(number, as, after, to)
  # find where to insert `term`
  # can't insert `term` directly,
  # because numbers must be updated before insertion
  k <- locate_line_insertion(lines, number, as, after, l)
  # update `lines` and insert `term`
  music$lines <- lines %>%
    update_line_numbers(number, as, after, l) %>%
    append(list(term), k)
  # update `music$key_lines`
  key_lines <- music$key_lines
  if (!is.null(key_lines)) {
    music$key_lines <-
      update_key_line_numbers(key_lines, number, as, after)
  }
  # update `music$clef_lines`
  clef_lines <- music$clef_lines
  if (!is.null(clef_lines)) {
    music$clef_lines <-
      update_key_line_numbers(clef_lines, number, as, after)
  }

  music
}



# Line validators ---------------------------------------------------------

check_line_as <- function(as) {
  if (!is.null(as)) {
    check_type(as, "character")
    check_length(as, 1)
    check_content(as, c("part", "staff", "voice"))
  }
}


check_line_to <- function(to) {
  check_type(to, c("character", "double", "integer"))
  check_length(to, 1)

  if (is.character(to)) {
    valid <- expression(!is.na(x))
    general <- "If `to` is a character, it must not be NA."
    check_content(to, valid, general = general)

  } else if (is.numeric(to)) {
    valid <- expression(!is.na(x) & as.integer(x) == x & x > 0)
    general <- "If `to` is a numeric, it must be a positive integer."
    check_content(to, valid, general = general)
  }
}


check_line_after <- function(after) {
  if (!is.null(after)) {
    check_type(after, "logical")
    check_length(after, 1)
    check_content(after, c(TRUE, FALSE))
  }
}


check_line_offset <- function(offset) {
  if (!is.null(offset)) {
    check_type(offset, c("double", "integer"))
    check_length(offset, 1)

    valid <- expression(x == 0 || is_tied_duration_value(x))
    general <-
      "`offset` must be 0, a duration value or sum of duration values."
    check_content(offset, valid, general = general)
  }
}



# Music + Line validators -------------------------------------------------

check_line_name_unique <- function(name, names) {
  if (!is.null(name) && name %in% names) {
    glue::glue(
      "Each Line in a Music must have a unique name or no name.",
      "\n\n",
      '* Name "{name}" has been used.'
    ) %>% rlang::abort()
  }
}


check_line_to_exist <- function(to, names, l) {
  if (!is.null(to)) {
    general <- "`to` must refer to a Line in the Music."

    if (is.character(to) && !(to %in% names)) {
      '* Can\'t find Line of name "{to}".' %>%
        glue::glue(general, "\n\n", .) %>%
        rlang::abort()

    } else if (is.numeric(to) && to > l) {
      if (l == 0) {
        s_l <- "no Line"
      } else if (l == 1) {
        s_l <- "only 1 Line"
      } else {
        s_l <- "only {l} Lines"
      }

      '* Can\'t find Line {to}, the Music contains ' %>%
        paste0(s_l, ".") %>%
        glue::glue(general, "\n\n", .) %>%
        rlang::abort()
    }
  }
}


check_voice_number <- function(number, to) {
  if (number >= 4) {
    if (is.null(to)) {
      s_to <- "the last Line"
    } else if (is.character(to)) {
      s_to <- 'Line "{to}"'
    } else if (is.numeric(to)) {
      s_to <- "Line {to}"
    }

    glue::glue(
      "Each staff in a Music can have at most 4 voices.",
      "\n\n",
      paste("* Staff containing", s_to, "already has 4 voices.")
    ) %>% rlang::abort()
  }
}



# Music + Line utils ------------------------------------------------------

# get `number` of the Line `to` refers to
get_to_number <- function(lines, to, l) {
  for (i in 1:l) {
    line <- lines[[i]]

    con <- `||`(
      is.numeric(to) && i == to,
      is.character(to) && line$name == to
    )

    if (con) {
      return(line$number)
    }
  }
}


# generate `number` for the Line to be inserted
generate_line_number <- function(number, as, after, to) {
  d <- ifelse(isFALSE(after), 0, 1)

  if (is.null(as) || as == "part") {
    number[1] <- number[1] + d
    number[2] <- 1
    number[3] <- 1

  } else if (as == "staff") {
    number[2] <- number[2] + d
    number[3] <- 1

  } else if (as == "voice") {
    check_voice_number(number[3], to)
    number[3] <- number[3] + d
  }

  number
}


# get the index at which to insert the Line
locate_line_insertion <- function(lines, number, as, after, l) {
  # unpack `number`
  p <- number[1]
  s <- number[2]
  v <- number[3]

  ks <- numeric(0)

  for (i in 1:l) {
    line <- lines[[i]]
    number_i <- line$number
    p_i <- number_i[1]
    s_i <- number_i[2]
    v_i <- number_i[3]

    if (is.null(as) || as == "part") {
      if (p_i == p) {
        ks <- c(ks, i)
      } else if (p_i > p) {
        break
      }

    } else if (as == "staff") {
      if (p_i == p) {
        if (s_i == s) {
          ks <- c(ks, i)
        } else if (s_i > s) {
          break
        }
      }

    } else if (as == "voice") {
      if (p_i == p && s_i == s) {
        if (v_i == v) {
          ks <- c(ks, i)
        } else if (v_i > v) {
          break
        }
      }
    }
  }

  if (isFALSE(after)) {
    ks[1] - 1
  } else {
    ks[length(ks)]
  }
}


# update the numbers of affected Lines
update_line_numbers <- function(lines, number, as, after, l) {
  # unpack `number`
  p <- number[1]
  s <- number[2]
  v <- number[3]

  d <- ifelse(isFALSE(after), 0, 1)

  for (i in 1:l) {
    # unpack `number`
    line <- lines[[i]]
    number_i <- line$number
    p_i <- number_i[1]
    s_i <- number_i[2]
    v_i <- number_i[3]

    # update `number`
    if (is.null(as) || as == "part") {
      if (p_i >= p + d) {
        lines[[i]]$number[1] <- p_i + 1
      }

    } else if (as == "staff") {
      if (p_i == p && s_i >= s + d) {
        lines[[i]]$number[2] <- s_i + 1
      }

    } else if (as == "voice") {
      if (p_i == p && s_i == s && v_i >= v + d) {
        lines[[i]]$number[3] <- v_i + 1
      }
    }
  }

  lines
}


update_key_line_numbers <- function(key_lines, number, as, after) {
  if (identical(as, "voice")) {
    return(key_lines)
  }

  p <- number[1]
  s <- number[2]

  d <- ifelse(isFALSE(after), 0, 1)
  l <- length(key_lines)

  for (i in 1:l) {
    key_line <- key_lines[[i]]
    number_i <- key_line$number
    p_i <- number_i[1]
    s_i <- number_i[2]

    if (is.null(as) || as == "part") {
      if (p_i >= p + d) {
        key_lines[[i]]$number[1] <- p_i + 1
      }

    } else if (as == "staff") {
      if (p_i == p && s_i >= s + d) {
        key_lines[[i]]$number[2] <- s_i + 1
      }
    }
  }

  key_lines
}



# normalize `lines` -------------------------------------------------------

# `offset` may be larger than the value of the Meter for `bar`,
# i.e. `offset` may be beyond the scope of `bar`
# normalize them to make the offset be within the scope of the bar
# `up` decide if round up offset when it has the length of the current Meter
normalize_bar_offset <- function(bar, offset, meters, up = TRUE) {
  repeat {
    v <- find_meter(bar, meters) %>% to_value()

    # e.g., bar = 2, offset = 0 vs bar = 1, offset = 4
    if (up && offset < v) {
      break
    } else if (!up && offset <= v) {
      break
    }

    bar <- bar + 1L
    offset <- offset - v
  }

  list(bar = bar, offset = offset)
}


# normalize `bar` and `offset` in each Line
normalize_bar_offset.lines <- function(lines, meters) {
  for (i in 1:length(lines)) {
    line <- lines[[i]]

    if (is.null(line$bar)) {
      lines[[i]]$bar <- 1L
    }

    offset <- line$offset

    if (is.null(offset)) {
      lines[[i]]$offset <- 0

    } else {
      . <- normalize_bar_offset(lines[[i]]$bar, offset, meters)
      lines[[i]]$bar <- .$bar
      lines[[i]]$offset <- .$offset
    }
  }

  lines
}
