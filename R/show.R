#' @export
show.Music <- function(x, to = NULL, width = NULL, ...) {
  # check arguments
  check_type(x, "Music", method = "class")
  check_show_to(to)
  check_show_width(width)
  # `x$lines` must not be empty
  check_show_lines(x$lines)
  # `x$meter_line` must have a Meter at bar 1
  check_show_meter_line(x$meter_line)

  # normalize `bar` and `offset` in each Line
  x$lines %<>% normalize_bar_offset.lines(x$meter_line$meters)

  # check if there is any tuplet group which crosses barline
  check_tuplet_group_over_bar(x$lines, x$meter_line$meters)

  x$key_lines %<>% normalize_key_lines()

  # convert each PitchNotation/Value to Pitch
  x %<>% to_Pitch()

  # leave marks in tied Pitches in each Line
  x$lines %<>% mark_tie.lines()

  # add `$measures`
  x$lines %<>% segment.lines(x$meter_line$meters)

  # append Measures to some Lines
  x$lines %<>% equalize(x$meter_line$meters)

  x$clef_lines %<>% normalize_clef_lines(x$lines)

  # merge any staff or voice to its parent part
  x$lines %<>% to_part()
}



# validators --------------------------------------------------------------

check_show_to <- function(to) {
  # early return
  if (is.null(to)) {
    return()
  }

  # basic checking
  check_type(to, "character")
  check_length(to, 1:2)

  # check content
  valid <- c("score", "audio")
  general <- '`to` must be "score", "audio" or both, if specified.'
  specifics <- character(0)
  l <- length(to)

  # the wording is more nuanced, don't merge this clause
  if (l == 1) {
    check_content(to, valid, general = general)

  } else {
    for (i in 1:l) {
      to_i <- to[[i]]
      if (!(to_i %in% valid)) {
        specifics[length(specifics) + 1] <-
          '`to[{i}]` is "{to_i}."' %>%
          glue::glue() %>%
          unclass()
      }
    }

    show_errors(general, specifics)
  }
}


check_show_width <- function(width) {
  if (!is.null(width)) {
    check_type(width, c("integer", "double"))
    check_length(width, 1)

    general <- '`width` must be a positive number, if specified.'
    check_content(width, expression(!is.na(x) && x > 0), general = general)
  }
}


check_show_lines <- function(lines) {
  if (is.null(lines)) {
    general <- "`x` must contain some Line."

    specifics <- c(
      "`x` contains no Line.",
      "Use `+ Line()` to add a Line."
    )

    show_errors(general, specifics)
  }
}


check_show_meter_line <- function(meter_line) {
  general <- "`x` must have a Meter at bar 1."
  specifics <- character(0)

  if (is.null(meter_line)) {
    specifics <- "`x` contains no Meter."
  } else if (meter_line$meters[[1]]$bar != 1) {
    specifics <- "`x` has no Meter at bar 1."
  }

  if (length(specifics) != 0) {
    specifics %<>% c("Use `+ Meter()` to add a Meter.")
    show_errors(general, specifics)
  }
}



# normalizers -------------------------------------------------------------

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


normalize_key_lines <- function(key_lines) {
  # add a global KeyLine to `key_lines`,
  con <- any(
    # when `key_lines` is empty,
    is.null(key_lines),
    length(key_lines) == 0,
    # or the first KeyLine is not global KeyLine
    any(key_lines[[1]]$number != c(0, 0))
  )

  if (con) {
    # create a global KeyLine
    key_line <- KeyLine() + Key(0)
    key_line$number <- c(0L, 0L)

    # insert it into `key_lines`
    key_lines %<>% append(list(key_line), 0)
  }

  # add `Key(0)` to any KeyLine that has no Key at bar 1
  for (i in 1:length(key_lines)) {
    key_line <- key_lines[[i]]

    if (key_line$keys[[1]]$bar != 1) {
      key_lines[[i]] <- key_line + Key(0)
    }
  }

  key_lines
}



# constructors ------------------------------------------------------------

# generalization of MusicXML elements backup and forward
# `direction` is "backup" or "forward"
# `...` includes `staff` and `voice`
Move <- function(duration, direction, ...) {
  list(duration = duration, direction = direction, ...) %>%
    `class<-`("Move")
}


#' @keywords internal
#' @export
print.Move <- function(x, silent = FALSE, ...) {
  # convert `x$direction`
  s_direction <- switch(
    x$direction,
    "backup" = "<-",
    "forward" = "->"
  )

  # convert `x`
  s <- paste0(s_direction, x$duration)

  # print or return
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


# to represent MusicXML element note
# `...` includes `invisible`, `staff` and `voice`
# it's more convenient to add marks in Pitches rather than in Notes,
# since a Note may contain more than one Pitch at its early stage
Note <- function(duration, pitch = PitchRest(), ...) {
  list(duration = duration, pitch = pitch, ...) %>%
    `class<-`("Note")
}


#' @keywords internal
#' @export
print.Note <- function(x, silent = FALSE, ...) {
  s_duration <- print(x$duration, "inside", TRUE)
  s_pitch <- print(x$pitch, TRUE)

  s <- "({s_duration}, {s_pitch})" %>%
    glue::glue() %>%
    unclass()

  # print or return
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


# rest for whole measure
Rest <- function(duration, ...) {
  list(duration = duration, ...) %>%
    `class<-`("Rest")
}


#' @keywords internal
#' @export
print.Rest <- function(x, silent = FALSE, ...) {
  s <- x$duration

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


Measure <- function(notes, number) {
  list(notes = notes, number = number) %>%
    `class<-`("Measure")
}


#' @keywords internal
#' @export
print.Measure <- function(x, silent = FALSE, ...) {
  s <-
    sapply(x$notes, print, silent = TRUE) %>%
    paste(collapse = ", ") %>%
    paste0(x$number, ": ", .)

  # print or return
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# segment -----------------------------------------------------------------

# convert Line to Measures:

# 1. combine pitches and Durations to Notes

# 2. segment Notes into Measures

# 3. add a backup to each Measure if the Line is not a part

# 4. convert offset to a forward if the Line is a voice, or untie offset
# into rests if not

# 5. generate empty Measures for bars before specified `$bar`
# if the Line is a voice, or Measures of Rest if not


segment <- function(line, meters) {
  # unpack `line`
  bar <- line$bar
  offset <- line$offset
  pitches <- line$pitches$pitches
  durations <- line$durations$durations
  l <- length(durations)

  # unpack `line$number`
  number <- line$number
  n2 <- number[2]
  n3 <- number[3]
  voice <- (n2 - 1) * 4 + n3
  # every staff has four voices

  # generate Measures before `bar`
  if (bar == 1) {
    ms <- list()
  } else {
    ms <- generate_measures(1:(bar - 1), meters, n2, n3, voice)
  }

  # initialize current measure
  m <- normalize_offset(offset, n2, n3, voice)

  # meter value for current measure
  v_meter <- find_meter(bar, meters) %>% to_value()
  # accumulated value of current measure
  v_accum <- offset

  for (i in 1:l) {
    # unpack
    d <- durations[[i]]
    v <- to_value(d)
    p <- pitches[[i]]
    c_ <- class(p)

    # if to untie `v`
    untie <- FALSE

    repeat {
      v_temp <- v_accum + v

      # deal with cross-barline `d`
      if (v_temp > v_meter) {
        ds <-
          # get rest value of current measure,
          (v_meter - v_accum) %>%
          # untie it,
          untie_duration_value(decreasing = FALSE) %>%
          # and convert it to Durations
          lapply(to_Duration)

        # mark tie and convert `ds` to Notes
        for (j in 1:length(ds)) {
          ds[[j]] %<>% Note(
            pitch = mark_tie_in_segment(p, c_, j),
            staff = n2,
            voice = voice
          )
        }

        # mark tie in `p` for next measure
        p %<>% mark_tie_in_segment(c_)
      }

      # add `d` or whatever to `m`
      if (v_temp <= v_meter) {
        if (isFALSE(untie)) {
          # generate Note and add it to `m`
          m %<>% c(list(Note(d, p, staff = n2, voice = voice)))
        } else {
          m %<>% c(to_Notes(v, pitch = p, staff = n2, voice = voice))
          untie <- FALSE
        }
      } else {
        m %<>% c(ds)
      }

      # complete the last measure with rests or forward
      if (v_temp < v_meter && i == l) {
        m %<>% c(normalize_offset(v_meter - v_temp, n2, n3, voice))
      }

      # add `m` to `ms`
      if (v_temp >= v_meter || i == l) {
        # add backup to any staff and voice
        # do this only when the current measure is complete and
        # ready to be appended
        if (n2 > 1 || n3 > 1) {
          m %<>% append(list(Move(v_meter, "backup")), 0)
        }

        ms %<>% c(list(Measure(m, bar)))
      }

      # update and reset variables
      if (v_temp < v_meter) {
        v_accum <- v_temp
      } else if (v_temp == v_meter) {
        v_accum <- 0
      } else if (v_temp > v_meter) {
        v <- v_temp - v_meter
        # note that `v` may not be a duration value
        untie <- TRUE
        v_accum <- 0
      }

      if (v_temp >= v_meter) {
        m <- list()
        bar <- bar + 1
        v_meter <- find_meter(bar, meters) %>% to_value()
      }

      # break
      if (v_temp <= v_meter) {
        break
      }
    }
  }

  ms
}


# generate Measures for specified bars
generate_measures <- function(bars, meters, n2, n3, voice) {
  ms <- list()

  for (bar in bars) {
    if (n3 > 1) {
      m <- Measure(list(), bar)

    } else if (n3 == 1) {
      m <-
        find_meter(bar, meters) %>%
        to_value() %>%
        Rest(staff = n2, voice = voice) %>%
        list() %>%
        Measure(bar)
    }

    ms %<>% c(list(m))
  }

  ms
}


# convert (tied) duration value to Notes
to_Notes <- function(value, ...) {
  value %>%
    untie_duration_value(decreasing = FALSE) %>%
    lapply(to_Duration) %>%
    lapply(Note, ...)
}


# convert offset to a empty list, a forward in a list, or rests
normalize_offset <- function(offset, n2, n3, voice) {
  if (offset == 0) {
    return(list())
  }

  # convert `offset` to rests when the Line is not a voice
  if (n3 == 1) {
    to_Notes(offset, invisible = TRUE, staff = n2, voice = voice)

  # convert `offset` to a forward when the Line is a voice
  } else if (n3 > 1) {
    Move(offset, "forward", staff = n2, voice = voice) %>% list()
    # add it to list for convenience of `segment`
  }
}


# mark tie in untied Notes
mark_tie_in_segment <- function(pitch, type, i = NULL) {
  if (type == "Pitch") {
    if (!is.null(i)) {
      pitch$tie_start <- TRUE
    }

    if (is.null(i) || i != 1) {
      pitch$tie_stop <- TRUE
    }

  } else if (type == "PitchChord") {
    if (!is.null(i)) {
      for (j in 1:length(pitch)) {
        pitch[[j]]$tie_start <- TRUE
      }
    }

    if (is.null(i) || i != 1) {
      for (j in 1:length(pitch)) {
        pitch[[j]]$tie_stop <- TRUE
      }
    }
  }

  pitch
}


# convert each Line to Measures and add it to `$measures`
segment.lines <- function(lines, meters) {
  for (i in 1:length(lines)) {
    lines[[i]]$measures <- segment(lines[[i]], meters)
  }

  lines
}


# append Measures to some Lines,
# to make all Lines contain the same number of Measures
equalize <- function(lines, meters) {
  # get the max length
  l <- lines %>%
    lapply(function(line) line$measures) %>%
    sapply(length) %>%
    max()

  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    measures <- line$measures
    l_ <- length(measures)
    number <- line$number
    n2 <- number[2]
    n3 <- number[3]
    voice <- (n2 - 1) * 4 + n3

    if (l_ < l) {
      lines[[i]]$measures <-
        (l_ + 1):l %>%
        generate_measures(meters, n2, n3, voice) %>%
        c(measures, .)
    }
  }

  lines
}



# merge -------------------------------------------------------------------

# merge any staff or voice to its parent part
to_part <- function(lines) {
  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    number <- line$number

    # skip if `line` is a part
    if (all(number[2:3] == c(1, 1))) {
      next
    }

    # get its parent part's number
    number_part <- c(number[1], 1, 1)
    # locate the part
    k <- locate_key_line(lines, number_part)

    measures <- line$measures

    # merge
    for (j in 1:length(measures)) {
      lines[[k]]$measures[[j]]$notes %<>% c(measures[[j]]$notes)
    }
  }

  lines
}
