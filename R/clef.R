# https://www.dolmetsch.com/musictheory14.htm
#' @export
Clef <- function(sign, line = NULL, octave = NULL, to = NULL, bar = NULL,
                 offset = NULL) {
  # check and normalize arguments -----------------------------------------
  check_clef_sign(sign)

  # normalize `sign`
  sign %<>% toupper()

  check_clef_line(line, sign)

  # normalize `line`
  if (is.null(line)) {
    line <- switch(sign, "G" = 2, "F" = 4, "C" = 3)
  }

  check_clef_octave(octave, sign, line)

  if (!is.null(to)) {
    check_line_to(to)
  }

  if (!is.null(bar)) {
    check_positive_integer(bar)
  }

  check_line_offset(offset)


  # create Clef -----------------------------------------------------------
  list(
    sign = sign,
    line = line,
    octave = octave,
    to = to,
    bar = bar,
    offset = offset
  ) %>% `class<-`("Clef")
}


#' @export
print.Clef <- function(x, context = "console", silent = FALSE, ...) {
  # convert `x$sign` and `x$line` -----------------------------------------
  sign <- x$sign
  line <- as.character(x$line)

  if (sign == "G") {
    s <- switch(
      line,
      "1" = "French Clef",
      "2" = "treble Clef"
    )

  } else if (sign == "F") {
    s <- switch(
      line,
      "3" = "baritone F-Clef",
      "4" = "bass Clef",
      "5" = "subbass Clef"
    )

  } else if (sign == "C") {
    s <- switch(
      line,
      "1" = "soprano Clef",
      "2" = "mezzo-soprano Clef",
      "3" = "alto Clef",
      "4" = "tenor Clef",
      "5" = "baritone C-Clef"
    )
  }


  # convert `x$octave` ----------------------------------------------------
  octave <- x$octave

  if (!is.null(octave)) {
    s_octave <- ifelse(octave == 1, "octave up", "octave down")
    s %<>% paste(s_octave, .)
  }


  # convert `x` -----------------------------------------------------------
  if (context == "inside") {

  } else if (context == "console") {
    specifics <- character(0)

    # convert `x$to`
    to <- x$to
    if (!is.null(to)) {
      s_to <- quote_string(to)
      specifics %<>% c("to be added to the staff containing Line {s_to}")
    }

    # convert `x$bar` and `x$offset`
    bar <- x$bar
    offset <- x$offset
    s_bar <- "to be added at bar {bar}"
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

    specifics %<>% c(s_bar_offset)

    s %<>% generate_string(specifics, environment())
  }


  # print or return -------------------------------------------------------
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# validators --------------------------------------------------------------

check_clef_sign <- function(sign) {
  check_type(sign, "character")
  check_length(sign, 1)

  general <- '`sign` must be "G", "F" or "C".'
  check_content(sign, c("G", "F", "C", "g", "f", "c"), general = general)
}


check_clef_line <- function(line, sign) {
  if (is.null(line)) {
    return()
  }

  check_positive_integer(line)

  valid <- switch(
    sign,
    "G" = 1:2,
    "F" = 3:5,
    "C" = 1:5
  )

  phrase <- coordinate(valid)
  general <- 'When `sign` is "{sign}", `line` must be {phrase}.'
  check_content(line, valid, general = general, sign = sign, phrase = phrase)
}


check_clef_octave <- function(octave, sign, line) {
  if (is.null(octave)) {
    return()
  }

  con <- (sign == "G" && line == 2) || (sign == "F" && line == 4)

  if (!con) {
    general <- paste(
      'Only when `sign` is "G" and `line` is 2,',
      'or `sign` is "F" and `line` is 4,',
      '`octave` can be set.'
    )
    specific <- '`sign` is "{sign}", `line` is {line}.'
    show_errors(general, specific, env = environment())

  } else {
    check_type(octave, c("double", "integer"))
    check_length(octave, 1)
    check_content(octave, c(-1, 1))
  }
}


# check its `to` when adding a Clef to a Music
check_clef_to <- function(to) {
  if (is.null(to)) {
    general <- "`to` in the Clef must be specified."
    specific <- "`to` is NULL."
    show_errors(general, specific)
  }
}



# Music + Clef ------------------------------------------------------------

# rationale behind this section is similar to it in key.R,
# from which some functions are borrowed


# initialize a ClefLine
ClefLine <- function() {
  list(
    clefs = list(),
    number = NULL
  ) %>% `class<-`("ClefLine")
}


# add a Clef to ClefLine
#' @keywords internal
#' @export
`+.ClefLine` <- function(clef_line, clef) {
  # normalize bar and offset in `clef`
  if (is.null(clef$bar)) {
    clef$bar <- 1L
  }

  if (is.null(clef$offset)) {
    clef$offset <- 0
  }

  # unpack
  clefs <- clef_line$clefs
  l <- length(clefs)

  # early return
  if (l == 0) {
    clef_line$clefs[[1]] <- clef
    return(clef_line)
  }

  # replace the Clef with the same bar and offset in `clef_line`,
  # or just append `clef` and sort `clefs` latter
  for (i in 1:l) {
    clef_i <- clefs[[i]]
    con <- clef_i$bar == clef$bar && clef_i$offset == clef$offset

    if (con) {
      clefs[[i]] <- clef
      break
    }

    if (!con && i == l) {
      clefs %<>% c(list(clef))
    }
  }

  clef_line$clefs <- sort_clefs(clefs)
  clef_line
}


sort_clefs <- function(clefs) {
  # get all `$bar`'s
  bars <- sapply(clefs, function(clef) clef$bar)
  # sort `clefs` by `$bar`
  clefs <- clefs[order(bars)]

  # re-assign `bars`
  bars <- sapply(clefs, function(clef) clef$bar)
  # sort `clefs` by `$offset`
  for (bar in unique(bars)) {
    ks <- which(bars == bar)

    if (length(ks) > 1) {
      cs <- clefs[ks]
      offsets <- sapply(cs, function(clef) clef$offset)
      clefs[ks] <- cs[order(offsets)]
    }
  }

  clefs
}


add.Clef <- function(term, music) {
  # unpack
  lines <- music$lines
  l <- length(lines)
  to <- term$to
  names <- lines %>%
    sapply(function(line) line$name) %>%
    unlist()

  # check `to`
  check_clef_to(to)
  check_line_to_exist(to, names, l)

  # get the number of the targeted ClefLine
  number <- get_to_number(lines, to, l)[1:2]

  # add the Clef (`term`)
  clef_lines <- music$clef_lines
  k <- locate_key_line(clef_lines, number)

  if (is.na(k)) {
    clef_line <- ClefLine() + term
    clef_line$number <- number
    clef_lines %<>% insert_key_line(clef_line, number)
  } else {
    clef_lines[[k]] <- clef_lines[[k]] + term
  }

  music$clef_lines <- clef_lines
  music
}


#' @keywords internal
#' @export
print.ClefLine <- function(x, silent = FALSE, ...) {
  # unpack
  clefs <- x$clefs
  l <- length(clefs)

  # convert `x$number` ----------------------------------------------------
  number <- x$number

  if (is.null(number)) {
    s_number <- ""
  } else {
    s_number <- " for part {number[1]} staff {number[2]}"
  }

  # empty form ------------------------------------------------------------
  if (l == 0) {
    s <- ""
  }

  # short form ------------------------------------------------------------
  if (l == 1) {
    clef <- clefs[[1]]
    bar <- clef$bar
    offset <- clef$offset

    if (offset != 0) {
      s_bar_offset <- " at bar {bar} with offset {offset}"
    } else if (bar != 1) {
      s_bar_offset <- " at bar {bar}"
    } else {
      s_bar_offset <- ""
    }

    s_clef <- print(clef, "inside", TRUE)
    s <- glue::glue(s_clef, s_bar_offset, s_number)
  }

  # long form -------------------------------------------------------------
  if (l > 1) {
    general <- paste0("Clefs", s_number)

    specifics <- sapply(clefs, function(clef) {
      bar <- clef$bar
      offset <- clef$offset

      if (offset != 0) {
        s_bar_offset <- " at bar {bar} with offset {offset}"
      } else {
        s_bar_offset <- " at bar {bar}"
      }

      s_clef <- print(clef, "inside", TRUE)
      glue::glue(s_clef, s_bar_offset) %>% unclass()
    })

    s <- generate_string(general, specifics, environment())
  }

  # print or return -------------------------------------------------------
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# Music -> MusicXML -------------------------------------------------------

# guess a Clef for `pitches`, which contains only Pitches and PitchRests
infer_clef <- function(pitches) {
  if (length(pitches) == 0) {
    return(Clef("G"))
  }

  # convert `pitches` to values
  pitches %<>%
    to_values.pitches() %>%
    unlist() %>%
    # remove any PitchRest
    {.[. != 0]}

  l <- length(pitches)

  if (l == 0) {
    return(Clef("G"))
  }

  # ranges for typical Clefs
  ranges <- list(
    # treble: C4, A5
    c(60, 81),
    # bass: E2, C4
    c(40, 60),
    # octave up treble: C5, A6
    c(72, 93),
    # octave down bass: E1, C3
    c(28, 48),
    # alto: D3, B4
    c(50, 71)
  )

  # find the fittest
  fs <- sapply(ranges, function(range) {
    con <- pitches >= range[1] & pitches <= range[2]
    table(con)["TRUE"]
  })

  k <- which(fs == max(fs, na.rm = TRUE))[1]

  # get the corresponding Clef
  switch(
    as.character(k),
    "1" = Clef("G"),
    "2" = Clef("F"),
    "3" = Clef("G", 2, 1),
    "4" = Clef("F", 4, -1),
    "5" = Clef("C")
  )
}


# extract Pitches from the corresponding staff
extract_pitches.lines <- function(lines, number, bar = Inf, offset = Inf) {
  # normalize `bar` and `offset`
  if (offset == 0) {
    bar <- bar - 1
    offset <- Inf
  }

  # unpack `number`
  n1 <- number[1]
  n2 <- number[2]

  pitches <- list()

  for (line in lines) {
    # unpack
    number_ <- line$number
    n1_ <- number_[1]
    n2_ <- number_[2]

    # break or skip
    if (n1_ > n1 || (n1_ == n1 && n2_ > n2)) {
      break
    } else if (n1_ != n1 || n2_ != n2) {
      next
    }

    pitches %<>% c(extract_pitches(line$measures, bar, offset))
  }

  pitches
}


# extract Pitches from `measures`
extract_pitches <- function(measures, bar, offset) {
  pitches <- list()

  for (measure in measures) {
    number <- measure$number

    if (number > bar) {
      return(pitches)
    }

    # accumulator
    v <- 0

    for (note in measure$notes) {
      if (number == bar && v >= offset) {
        return(pitches)
      }

      c_ <- class(note)

      # skip current measure if it contains only a Rest
      if (c_ == "Rest") {
        break
      }

      # ignore backup, add forward's duration to `v`
      if (c_ == "Move") {
        direction <- note$direction

        if (direction == "backup") {
          next
        } else if (direction == "forward") {
          v <- v + note$duration
        }
      }

      if (c_ == "Note") {
        # update `v`
        v <- v + to_value(note$duration)

        # update `pitches`
        pitch <- note$pitch
        c_p <- class(pitch)

        if (c_p == "Pitch") {
          pitches %<>% c(list(pitch))
        } else if (c_p == "PitchChord") {
          pitches %<>% c(pitch)
        }
      }
    }
  }

  pitches
}


# get staffs' numbers (first two digits)
get_staff_numbers <- function(lines) {
  lines %>%
    lapply(function(line) line$number[1:2] %>% as.integer()) %>%
    unique()
}


normalize_clef_lines <- function(clef_lines, lines, meters) {
  # get staffs' numbers
  numbers <- get_staff_numbers(lines)

  for (number in numbers) {
    # locate corresponding ClefLine
    k <- locate_key_line(clef_lines, number)

    if (is.na(k)) {
      pitches <- extract_pitches.lines(lines, number)
      clef <- infer_clef(pitches)
      clef_line <- ClefLine() + clef
      clef_line$number <- number
      clef_lines %<>% insert_key_line(clef_line, number)

    } else {
      # get the ClefLine
      clef_line <- clef_lines[[k]]
      clefs <- clef_line$clefs
      l <- length(clefs)

      # normalize bar and offset of each Clef
      for (i in 1:l) {
        clef_i <- clefs[[i]]
        bar_i <- clef_i$bar
        offset_i <- clef_i$offset
        . <- normalize_bar_offset(bar_i, offset_i, meters)
        clef_line$clefs[[i]]$bar <- .$bar
        clef_line$clefs[[i]]$offset <- .$offset
      }

      # check first Clef
      clef_1 <- clefs[[1]]
      bar_1 <- clef_1$bar
      offset_1 <- clef_1$offset

      if (bar_1 != 1 || offset_1 != 0) {
        pitches <- extract_pitches.lines(lines, number, bar_1, offset_1)
        clef <- infer_clef(pitches)
        clef_line <- clef_line + clef
      }

      clef_lines[[k]] <- clef_line
    }
  }

  clef_lines
}


# merge Clefs to its parent part
merge_clef_lines <- function(lines, clef_lines, meters) {
  # merge ClefLines backwards
  for (clef_line in rev(clef_lines)) {
    # get current ClefLine's number
    number <- clef_line$number
    # locate its parent part and unpack it
    k <- locate_key_line(lines, c(number[1], 1, 1))
    measures <- lines[[k]]$measures
    l <- length(measures)

    # merge Clefs backwards
    for (clef in rev(clef_line$clefs)) {
      # unpack
      bar <- clef$bar
      offset <- clef$offset

      # skip Clef with bar beyond `l`
      if (bar > l) {
        next
      }

      # add staff number to `clef`
      clef$number <- number[2]

      # Clefs with offset 0 are merged in a different way
      if (offset == 0) {
        # get first item in current Measure
        a <- measures[[bar]]$notes[[1]]
        c_ <- class(a)

        if (c_ == "Attributes") {
          lines[[k]]$measures[[bar]]$notes[[1]]$attributes %<>%
            append(list(clef), 0)
        } else {
          lines[[k]]$measures[[bar]]$notes %<>%
            append(list(Attributes(list(clef))), 0)
        }

      } else {
        # get the value of the Meter at `bar`
        v_meter <- find_meter(bar, meters) %>% to_value()
        # generate forwards and backup
        f1 <- Move(offset, "forward")
        f2 <- Move(v_meter - offset, "forward")
        b <- Move(v_meter, "backup")

        lines[[k]]$measures[[bar]]$notes %<>%
          append(list(f1, Attributes(list(clef)), f2, b), 0)
      }
    }
  }

  lines
}


#' @keywords internal
#' @export
to_Element.Clef <- function(x, ...) {
  contents <- list(
    Element("sign", x$sign),
    Element("line", x$line)
  )

  octave <- x$octave
  if (!is.null(octave)) {
    contents %<>% c(list(Element("clef-octave-change", octave)))
  }

  number <- x$number
  if (!is.null(number)) {
    attributes <- list(number = number)
  } else {
    attributes <- NULL
  }

  Element("clef", contents, attributes)
}
