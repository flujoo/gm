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


to_Pitch.notation <- function(notation) {
  pn <- split.pitch_notation(notation)
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


to_midi.notation <- function(notation) {
  pn <- split.pitch_notation(notation)
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
print.Pitch <- function(pitch) {
  s <- to_string.Pitch(pitch)
  cat(s, "\n")
}


to_string.PitchChord <- function(pitch_chord) {
  ss <- c()
  for (p in pitch_chord) {
    if (class(p) == "integer") {
      ss <- c(ss, p)
    } else {
      ss <- c(ss, to_string.Pitch(p))
    }
  }
  paste0("(", paste(ss, collapse = ", "), ")")
}


#' @export
print.PitchChord <- function(pitch_chord) {
  s <- to_string.PitchChord(pitch_chord)
  cat(s, "\n")
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
print.PitchLine <- function(pitch_line) {
  s <- to_string.PitchLine(pitch_line)
  cat(s, "\n")
}



# PitchLine ---------------------------------------------------------

#' @title Normalize Various Data Structures to Pitch Structures
#'
#' @description Valid data structures are handled as follows:
#' \itemize{
#'  \item Pitch notations are converted to Pitches if of length 1,
#'  and to PitchChords if longer.
#'  \item MIDI note numbers (maybe as characters) are normalized to integers.
#'  \item \code{NULL}s, \code{NaN}s, \code{NA}s and 0-length atomic vectors
#'  are converted to default \code{NA}s.
#'  \item Pitches and PitchChords are untouched.
#'  \item Lists of these just mentioned data structures are handled by
#'  function \code{PitchChord}.
#' }
#'
#' @details Used in function \code{Voice}.
#'
#' @param pitch A list of MIDI note numbers (maybe as characters), pitch
#' notations, \code{NA}s, \code{NaN}s, Pitches, PitchChords, \code{NULL}s and
#' lists of these just mentioned data structures.
#'
#' @return A list of Pitches, PitchChords, integers and \code{NA}s.
PitchLine <- function(pitch) {
  l <- length(pitch)

  if (l == 0) {
    stop('argument "pitch" should not be empty')
  }

  m <- 'invalid item of argument "pitch" at position '

  for (i in 1:l) {
    p <- pitch[[i]]
    c_ <- class(p)
    l_ <- length(p)

    # let PitchChord handle lists and long atomic vectors
    if (c_ == "list" || (l_ > 1 && is.atomic(p))) {
      tryCatch(
        {pitch[[i]] <- PitchChord(p)},
        error = function(e) {stop(m, i)}
      )

    # convert NAs, NaNs, NULLs and 0-length atomics to default NAs
    } else if (is.atomic(p) && (is.na(p) || l_ == 0)) {
      pitch[[i]] <- NA

    # convert pitch notations
    } else if (c_ == "character") {
      if (validate.pitch_notation(p)) {
        pitch[[i]] <- to_Pitch.notation(p)
      # convert character midis to integers
      } else {
        p <- suppressWarnings(as.integer(p))
        if (validate.midi(p)) {
          pitch[[i]] <- p
        } else {
          stop(m, i)
        }
      }

    # keep integer midis untouched
    } else if (c_ == "integer" && validate.midi(p)) {

    # convert double midis to integers
    } else if (c_ == "numeric" && validate.midi(p)) {
      pitch[[i]] <- as.integer(p)

    # keep Pitches and PitchChords untouched
    } else if (c_ %in% c("Pitch", "PitchChord")) {

    # invalid items
    } else {
      stop(m, i)
    }
  }

  class(pitch) <- "PitchLine"
  return(pitch)
}


PitchChord <- function(pitches) {
  if (identical(pitches, list())) {
    return(NA)
  }

  # add vectors to lists to make it easy for the below recursive function
  if (class(pitches) != "list") {
    pitches <- list(pitches)
  }

  core <- function(pitches) {
    if (identical(pitches, list())) {
      stop()
    }

    p <- pitches[[1]]
    c_ <- class(p)
    ps <- list()

    if (c_ == "character") {
      for (p_i in p) {
        if (validate.pitch_notation(p_i)) {
          ps[[length(ps) + 1]] <- to_Pitch.notation(p_i)
        } else {
          p_i <- suppressWarnings(as.integer(p_i))
          if (validate.midi(p_i)) {
            ps[[length(ps) + 1]] <- p_i
          } else {
            stop()
          }
        }
      }
    } else if (c_ == "numeric") {
      for (p_i in p) {
        if (validate.midi(p_i)) {
          ps[[length(ps) + 1]] <- as.integer(p_i)
        } else {
          stop()
        }
      }
    } else if (c_ == "integer") {
      for (p_i in p) {
        if (validate.midi(p_i)) {
          ps[[length(ps) + 1]] <- p_i
        } else {
          stop()
        }
      }
    } else if (c_ == "Pitch") {
      ps[[length(ps) + 1]] <- p
    } else if (c_ == "PitchChord") {
      ps <- append(ps, unclass(p))
    } else if (c_ == "list") {
      ps <- core(p)
    } else {
      stop()
    }

    pitches <- pitches[-1]
    if (identical(pitches, list())) {
      return(ps)
    }

    append(ps, core(pitches))
  }

  ps <- core(pitches)
  if (length(ps) == 1) {
    return(ps[[1]])
  }
  class(ps) <- "PitchChord"
  ps
}


find_fifths <- function(fifths_list, i) {
  is_ <- sapply(fifths_list, function(x) x[1])
  fs <- sapply(fifths_list, function(x) x[2])
  f <- fs[is_ <= i]
  f[length(f)]
}


#' @title Convert MIDIs in a Pitch Line to Pitches
#' @details MIDI notations are converted to MIDIs already. MIDIs are
#' normalized to integers already.
to_Pitch.PitchLine <- function(pitch_line, fifths_list = list(c(1, 0))) {
  l <- length(pitch_line)

  for (i in l:1) {
    p <- pitch_line[[i]]
    c_ <- class(p)

    if (c_ %in% c("integer", "PitchChord")) {
      if (i == l) {
        next_ <- NULL
      } else {
        next_ <- pitch_line[[i + 1]]
        if (class(next_) != "Pitch") {
          next_ <- NULL
        }
      }

      fifths <- find_fifths(fifths_list, i)

      if (c_ == "integer") {
        pitch_line[[i]] <- to_Pitch.midi(p, fifths, next_)
      } else if (c_ == "PitchChord") {
        for (j in 1:length(p)) {
          p_ <- p[[j]]
          if (class(p_) == "integer") {
            pitch_line[[i]][[j]] <- to_Pitch.midi(p_, fifths, next_)
          }
        }
      }
    }
  }

  pitch_line
}


sort.Pitches <- function(pitches) {
  midis <- sapply(pitches, to_midi.Pitch)
  pitches[order(midis)]
}



# -> Element --------------------------------------------------------

#' @details MusicXML element "pitch", "step", "alter" and "octave" have no
#' attributes. See \url{https://usermanuals.musicxml.com/MusicXML/
#' Content/EL-MusicXML-pitch.htm}.
to_Element.Pitch <- function(pitch) {
  nn <- Element("step", pitch$note_name)
  o <- Element("octave", pitch$octave)

  a <- pitch$alter
  if (a == 0) {
    content <- list(nn, o)
  } else {
    content <- list(nn, Element("alter", a), o)
  }

  Element("pitch", content)
}


to_Element.PitchChord <- function(pitch_chord) {
  ps <- lapply(pitch_chord, to_Element.Pitch)
  for (i in 2:length(ps)) {
    ps[[i]] <- list(Element("chord"), ps[[i]])
  }
  ps
}


to_Element.PitchLine <- function(pitch_line) {
  pl <- unclass(pitch_line)
  for (i in 1:length(pl)) {
    p <- pitch_line[[i]]
    c_ <- class(p)
    if (c_ == "Pitch") {
      pl[[i]] <- to_Element.Pitch(p)
    } else if (c_ == "PitchChord") {
      pl[[i]] <- to_Element.PitchChord(p)
    } else if (c_ == "logical") {
      pl[[i]] <- Element("rest")
    }
  }
  pl
}
