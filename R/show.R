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

  # add Clef at position 1 in each Line if no
  x$lines %<>% normalize_clefs()

  # leave marks in tied pitches in each Line
  x$lines %<>% mark_tie.lines()
}



# `show.Music` validators -------------------------------------------------

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



# `show.Music` normalizers ------------------------------------------------

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


# infer a Clef and add it at position 1 in each Line if no
normalize_clefs <- function(lines) {
  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    number <- line$number

    # not add Clef to voices
    if (number[3] == 1) {
      clefs <- line$clefs

      # get first Clef's position if any
      if (!is.null(clefs)) {
        po <- clefs$clefs[[1]]$position
      }

      # add a Clef if no Clef at position 1
      if (is.null(clefs) || po != 1) {
        pitches <- line$pitches$pitches

        # initialize `clefs`
        if (is.null(clefs)) {
          clefs <- ClefLine()
        # take only pitches before the first Clef
        } else {
          pitches <- pitches[1:(po - 1)]
        }

        clef <- infer_clef(pitches)
        clefs %<>% merge_clef(clef)
        lines[[i]]$clefs <- clefs
      }
    }
  }

  lines
}


# leave marks in tied pitches in each Line
mark_tie.lines <- function(lines) {
  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    tie <- line$tie

    # mark
    if (!is.null(tie)) {
      pitches <- line$pitches$pitches
      positions <- tie$positions
      lines[[i]]$pitches$pitches <- mark_tie(pitches, positions)
    }
  }

  lines
}
