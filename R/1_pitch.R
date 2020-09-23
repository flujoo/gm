# pitch notation -> Pitch -------------------------------------------

#' @param pitch_notation An atomic vector.
#' Lists may cause undesirable results.
#' @return A logical vector.
validate.pitch_notation <- function(pitch_notation) {
  reg <- paste0(
    "^",
    # a valid pitch notation always starts with a note name
    # either in uppercase or lowercase
    "([A-G]|[a-g])",
    # maybe followed by an accidental
    "(#{0,2}|-{0,2})",
    # followed by an octave
    "[0-9]",
    "$"
  )
  con <- grepl(reg, pitch_notation)

  for (i in 1:length(con)) {
    c_ <- con[i]
    if (!identical(c_, FALSE) && !identical(c_, TRUE)) {
      con[i] <- FALSE
    }
  }

  con
}


split.pitch_notation <- function(pitch_notation) {
  l <- nchar(pitch_notation)
  list(
    # note name
    substr(pitch_notation, 1, 1),
    # accidental
    substr(pitch_notation, 2, l - 1),
    # octave
    substr(pitch_notation, l, l)
  )
}


to_alter.accidental <- function(accidental) {
  accidentals <- c("--", "-", "", "#", "##")
  alters <- -2:2
  i <- which(accidentals == accidental)
  alters[i]
}


to_Pitch.pitch_notation <- function(pitch_notation) {
  pn <- split.pitch_notation(pitch_notation)
  p <- list(
    note_name = toupper(pn[[1]]),
    alter = to_alter.accidental(pn[[2]]),
    octave = as.double(pn[[3]])
  )
  class(p) <- "Pitch"
  p
}



# Pitch/pitch notation -> midi --------------------------------------

to_value.note_name <- function(note_name) {
  nns <- c("C", "D", "E", "F", "G", "A", "B")
  pcs <- c(0, 2, 4, 5, 7, 9, 11)
  i <- which(nns == note_name)
  pcs[i]
}


to_midi.pitch_notation <- function(pitch_notation) {
  pn <- split.pitch_notation(pitch_notation)
  to_value.note_name(toupper(pn[[1]])) + to_alter.accidental(pn[[2]]) +
    (as.double(pn[[3]]) + 1) * 12
}


to_midi.Pitch <- function(pitch) {
  to_value.note_name(pitch$note_name) + pitch$alter +
    (pitch$octave + 1) * 12
}



# midi -> Pitch -----------------------------------------------------

#' @param midi An atomic vector.
#' Lists may cause undesirable results.
#' @return A logical vector.
validate.midi <- function(midi) {
  con <- (class(midi) %in% c("integer", "numeric")) & !(is.na(midi)) &
    midi >= 12 & midi <= 127 & midi == as.integer(midi)

  for (i in 1:length(con)) {
    c_ <- con[i]
    if (!identical(c_, FALSE) && !identical(c_, TRUE)) {
      con[i] <- FALSE
    }
  }

  con
}


to_note_name.value <- function(value) {
  nns <- c("C", "D", "E", "F", "G", "A", "B")
  vs <- c(0, 2, 4, 5, 7, 9, 11)
  i <- which(vs == value)
  nns[i]
}


to_pitch_classes.value <- function(value) {
  vs <- (value + -2:2) %% 12
  alters <- 2:-2
  pcs <- list()
  for (i in 1:5) {
    nn <- to_note_name.value(vs[i])
    if (!identical(nn, character(0))) {
      pcs[[length(pcs) + 1]] <- list(
        note_name = nn,
        alter = alters[i]
      )
    }
  }
  pcs
}


get_pitch_classes.fifths <- function(fifths) {
  fs <- c("F", "C", "G", "D", "A", "E", "B")
  if (fifths >= 0) {
    as_ <- c(rep(1, fifths), rep(0, 7 - fifths))
  } else {
    as_ <- c(rep(0, 7 + fifths), rep(-1, -fifths))
  }

  pcs <- list()
  for (i in 1:7) {
    pcs[[length(pcs) + 1]] <- list(note_name = fs[i], alter = as_[i])
  }
  pcs
}


get_sharp_5th.fifths <- function(fifths) {
  i <- which(-7:7 == fifths)
  nns <- c("G", "D", "A", "E", "B", "F", "C")
  list(
    note_name = nns[((i - 1) %% 7) + 1],
    alter = (i - 1 + 2) %/% 7
  )
}


get_chromatic_previous_pitch_classes.Pitch <- function(pitch) {
  nn <- pitch$note_name
  pc <- to_value.note_name(nn)
  a <- pitch$alter
  nns <- c("C", "D", "E", "F", "G", "A", "B")
  i <- which(nns == nn)

  # ascending
  asc_nn <- nns[((i - 1 - 1) %% 7) + 1]
  asc_pc <- to_value.note_name(asc_nn)
  if (abs(asc_pc - pc) == 2) {
    asc_a <- a + 1
  } else {
    asc_a <- a
  }
  asc <- list(note_name = asc_nn, alter = asc_a)

  # descending
  des_nn <- nns[((i + 1 - 1) %% 7) + 1]
  des_pc <- to_value.note_name(des_nn)
  if (abs(des_pc - pc) == 2) {
    des_a <- a - 1
  } else {
    des_a <- a
  }
  des <- list(note_name = des_nn, alter = des_a)

  list(asc, des)
}


select_pitch_class <- function(elements, container) {
  for (x in container) {
    for (y in elements) {
      nn <- y$note_name
      a <- y$alter
      if (nn == x$note_name && a == x$alter) {
        return(list(note_name = nn, alter = a))
      }
    }
  }
}


to_Pitch.midi <- function(midi, fifths = 0, next_ = NULL) {
  pc <- midi %% 12
  o <- midi %/% 12 - 1
  # all possible note name and alter pairs
  nnas <- to_pitch_classes.value(pc)

  p <- select_pitch_class(nnas, get_pitch_classes.fifths(fifths))
  if (!is.null(p)) {
    p$octave <- o
    class(p) <- "Pitch"
    return(p)
  }

  if (!is.null(next_) && abs(to_midi.Pitch(next_) - midi) == 1) {
    p <- select_pitch_class(nnas,
      get_chromatic_previous_pitch_classes.Pitch(next_))
    if (!is.null(p)) {
      p$octave <- o
      class(p) <- "Pitch"
      return(p)
    }
  }

  p <- select_pitch_class(nnas, list(get_sharp_5th.fifths(fifths)))
  if (!is.null(p)) {
    p$octave <- o
    class(p) <- "Pitch"
    return(p)
  }

  # select one with minimal absolute alter
  as_ <- sapply(nnas, function(x) x$alter)
  i <- which(abs(as_) == min(abs(as_)))
  if (length(i) > 1) {
    if (fifths >= 0) {
      # one with alter 1
      i <- i[1]
    } else {
      # one with alter -1
      i <- i[2]
    }
  }
  nna <- nnas[[i]]
  p <- list(note_name = nna[[1]], alter = nna[[2]], octave = o)
  class(p) <- "Pitch"
  p
}



# print -------------------------------------------------------------

to_accidental.alter <- function(alter) {
  accidentals <- c("--", "-", "", "#", "##")
  alters <- -2:2
  i <- which(alters == alter)
  accidentals[i]
}


to_string.Pitch <- function(pitch) {
  pitch$alter <- to_accidental.alter(pitch$alter)
  paste(pitch, collapse = "")
}


#' @export
print.Pitch <- function(x, ...) {
  s <- to_string.Pitch(x)
  cat(s, "\n")
  return(invisible(s))
}


to_string.PitchChord <- function(pitch_chord) {
  ss <- sapply(pitch_chord, to_string.Pitch)
  paste0("(", paste(ss, collapse = ", "), ")")
}


to_string.PitchLine <- function(pitch_line) {
  l <- length(pitch_line)
  if (l == 0) {
    s <- ""
  } else {
    for (i in 1:l) {
      p <- pitch_line[[i]]
      c_ <- class(p)
      if (c_ == "Pitch") {
        pitch_line[[i]] <- to_string.Pitch(p)
      } else if (c_ == "PitchChord") {
        pitch_line[[i]] <- to_string.PitchChord(p)
      } else {
        pitch_line[[i]] <- "NA"
      }
    }
    s <- paste(pitch_line, collapse = ", ")
  }
  s
}


#' @export
print.PitchLine <- function(x, ...) {
  s <- to_string.PitchLine(x)
  cat(s, "\n")
  return(invisible(s))
}



# PitchLine ---------------------------------------------------------

#' @title Normalize Various Data Structures to Pitch Structures
#' @description Valid data structures are handled as follows:
#' \itemize{
#'  \item MIDI note numbers (maybe as characters) and pitch notations will be
#'  converted to Pitches if of length 1, and to PitchChords if longer.
#'  \item \code{NULL}s, \code{NaN}s, \code{NA}s and 0-length atomic vectors
#'  will be converted to default \code{NA}s.
#'  \item Pitches and PitchChords are untouched.
#'  \item Lists of these just mentioned data structures are handled by
#'  function \code{PitchChord}.
#' }
#' @details Used in function \code{Voice}.
#' @param pitches A list of MIDI note numbers (maybe as characters), pitch
#' notations, \code{NA}s, \code{NaN}s, Pitches, PitchChords, \code{NULL}s and
#' lists of these just mentioned data structures.
#' @param fifths_list A list of duplets of position and fifths, in an
#' ascending order by position.
#' @return A list of Pitches, PitchChords, and \code{NA}s.
PitchLine <- function(pitches, fifths_list) {
  l <- length(pitches)
  if (l == 0) {
    class(pitches) <- "PitchLine"
    return(pitches)
  }
  is_ <- c()

  for (i in l:1) {
    p <- pitches[[i]]
    c_ <- class(p)
    l_ <- length(p)
    if (i == l) {
      next_ <- NULL
    } else {
      next_ <- pitches[[i + 1]]
      if (class(next_) != "Pitch") {
        next_ <- NULL
      }
    }
    fifths <- find_fifths(fifths_list, i)

    # let PitchChord handle lists and long atomic vectors
    if (c_ == "list" || (l_ > 1 && is.atomic(p))) {
      tryCatch(
        {pitches[[i]] <- PitchChord(p, fifths, next_)},
        error = function(e) {is_ <- c(is_, i)}
      )
    # convert NAs, NaNs, NULLs and 0-length atomics to default NAs
    } else if (is.atomic(p) && (is.na(p) || l_ == 0)) {
      pitches[[i]] <- NA
    # convert pitch notations and midis as characters
    } else if (c_ == "character") {
      if (validate.pitch_notation(p)) {
        pitches[[i]] <- to_Pitch.pitch_notation(p)
      } else {
        p <- suppressWarnings(as.double(p))
        if (validate.midi(p)) {
          pitches[[i]] <- to_Pitch.midi(p, fifths, next_)
        } else {
          is_ <- c(is_, i)
        }
      }
    # convert midis
    } else if (c_ %in% c("numeric", "integer") && validate.midi(p)) {
      pitches[[i]] <- to_Pitch.midi(p, fifths, next_)
    # keep Pitches and PitchChords untouched and deal with invalid items
    } else if (!(c_ %in% c("Pitch", "PitchChord"))) {
      is_ <- c(is_, i)
    }
  }

  l_is <- length(is_)
  if (l_is > 0) {
    if (l_is == i) {
      m <- paste('invalid item of argument "pitches" at position', is_)
    } else {
      is_ <- rev(is_)
      m <- paste(
        'invalid items of argument "pitches" at positions',
        paste(paste(is_[-l_is], collapse = ", "), "and", is_[l_is])
      )
    }
    stop(m)
  } else {
    class(pitches) <- "PitchLine"
    return(pitches)
  }
}


find_fifths <- function(fifths_list, i) {
  is_ <- sapply(fifths_list, function(x) x[1])
  fs <- sapply(fifths_list, function(x) x[2])
  f <- fs[is_ <= i]
  f[length(f)]
}


PitchChord <- function(pitches, fifths, next_) {
  ps <- list()
  for (p in pitches) {
    if (class(p) == "Pitch") {
      ps[[length(ps) + 1]] <- p
    } else if (length(p) == 1) {
      if (validate.pitch_notation(p)) {
        ps[[length(ps) + 1]] <- to_Pitch.pitch_notation(p)
      } else if (validate.midi(p)) {
        ps[[length(ps) + 1]] <- to_Pitch.midi(p, fifths, next_)
      } else {
        warning()
      }
    } else {
      warning()
    }
  }

  l <- length(ps)
  if (l == 0) {
    warning()
    return(NULL)
  } else if (l == 1) {
    warning()
    p <- ps[[1]]
    class(p) <- "Pitch"
    return(p)
  } else {
    ps <- sort.Pitches(ps)
    class(ps) <- "PitchChord"
    ps
  }
}


sort.Pitches <- function(pitches) {
  midis <- sapply(pitches, to_midi.Pitch)
  pitches[order(midis)]
}
