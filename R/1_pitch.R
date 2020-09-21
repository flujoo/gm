# pitch notation -> Pitch -------------------------------------------

validate.pitch_notation <- function(pitch_notation) {
  reg <- paste0(
    "^",
    # a valid pitch notation always starts with a note name,
    # which is case insensitive
    "([A-G]|[a-g])",
    # maybe followed by an accidental
    "(#{0,2}|-{0,2})",
    # followed by an octave
    "[0-9]",
    "$"
  )
  con <- grepl(reg, pitch_notation)

  if (identical(con, logical(0))) {
    return(FALSE)
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

to_pitch_class.note_name <- function(note_name) {
  nns <- c("C", "D", "E", "F", "G", "A", "B")
  pcs <- c(0, 2, 4, 5, 7, 9, 11)
  i <- which(nns == note_name)
  pcs[i]
}


to_midi.pitch_notation <- function(pitch_notation) {
  pn <- split.pitch_notation(pitch_notation)
  to_pitch_class.note_name(toupper(pn[[1]])) + to_alter.accidental(pn[[2]]) +
    (as.double(pn[[3]]) + 1) * 12
}


to_midi.Pitch <- function(pitch) {
  to_pitch_class.note_name(pitch$note_name) + pitch$alter +
    (pitch$octave + 1) * 12
}



# midi -> Pitch -----------------------------------------------------

validate.midi <- function(midi) {
  con <- midi >= 12 && midi <= 127 && midi == as.integer(midi)
  if (is.na(con)) {
    return(FALSE)
  }
  con
}


to_note_name.pitch_class <- function(pitch_class) {
  nns <- c("C", "D", "E", "F", "G", "A", "B")
  pcs <- c(0, 2, 4, 5, 7, 9, 11)
  i <- which(pcs == pitch_class)
  nns[i]
}


#' @title Convert Pitch Class to All Possible Note Name and Alter Pairs
to_note_name_alters.pitch_class <- function(pitch_class) {
  pcs <- (pitch_class + -2:2) %% 12
  alters <- 2:-2
  ps <- list()
  for (i in 1:5) {
    nn <- to_note_name.pitch_class(pcs[i])
    if (!identical(nn, character(0))) {
      ps[[length(ps) + 1]] <- list(
        note_name = nn,
        alter = alters[i]
      )
    }
  }
  ps
}


# nna: note name and alter pair
#' @title Get All Note Name and Alter Pairs for a Given Key
get_nnas.fifths <- function(fifths) {
  fs <- c("F", "C", "G", "D", "A", "E", "B")
  if (fifths >= 0) {
    as_ <- c(rep(1, fifths), rep(0, 7 - fifths))
  } else {
    as_ <- c(rep(0, 7 + fifths), rep(-1, -fifths))
  }

  ps <- list()
  for (i in 1:7) {
    ps[[length(ps) + 1]] <- list(note_name = fs[i], alter = as_[i])
  }
  ps
}


# hl: leading tone of a harmonic scale
#' @title Get Hamonic Scale's Leading Tone's Note Name and Alter
#' for a Given Key
get_hl_nna.fifths <- function(fifths) {
  i <- which(-7:7 == fifths)
  nns <- c("G", "D", "A", "E", "B", "F", "C")
  list(
    note_name = nns[((i - 1) %% 7) + 1],
    alter = (i - 1 + 2) %/% 7
  )
}


# cp: chromatic previous (pitches)
#' @title Get a Pitch's Chromatic Previous Note Name and Alter Pairs
#' @details Used in function \code{to_Pitch.midi} and after
#' \code{get_nnas.fifths}.
get_cp_nnas.Pitch <- function(pitch) {
  nn <- pitch$note_name
  pc <- to_pitch_class.note_name(nn)
  a <- pitch$alter
  nns <- c("C", "D", "E", "F", "G", "A", "B")
  i <- which(nns == nn)

  # ascending
  asc_nn <- nns[((i - 1 - 1) %% 7) + 1]
  asc_pc <- to_pitch_class.note_name(asc_nn)
  if (abs(asc_pc - pc) == 2) {
    asc_a <- a + 1
  } else {
    asc_a <- a
  }
  asc <- list(note_name = asc_nn, alter = asc_a)

  # descending
  des_nn <- nns[((i + 1 - 1) %% 7) + 1]
  des_pc <- to_pitch_class.note_name(des_nn)
  if (abs(des_pc - pc) == 2) {
    des_a <- a - 1
  } else {
    des_a <- a
  }
  des <- list(note_name = des_nn, alter = des_a)

  list(asc, des)
}


#' @title Select Note Name and Alter Pair
#' @details Used in function \code{to_Pitch.midi}.
select_nna <- function(elements, container) {
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
  nnas <- to_note_name_alters.pitch_class(pc)

  p <- select_nna(nnas, get_nnas.fifths(fifths))
  if (!is.null(p)) {
    p$octave <- o
    class(p) <- "Pitch"
    return(p)
  }

  if (!is.null(next_) && abs(to_midi.Pitch(next_) - midi) == 1) {
    p <- select_nna(nnas, get_cp_nnas.Pitch(next_))
    if (!is.null(p)) {
      p$octave <- o
      class(p) <- "Pitch"
      return(p)
    }
  }

  p <- select_nna(nnas, list(get_hl_nna.fifths(fifths)))
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



# normalize pitches -------------------------------------------------

normalize.pitches <- function(pitches, fifths_list) {
  l <- length(pitches)

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

    if (c_ == "list" || l_ > 1 && !(c_ %in% c("Pitch", "PitchChord"))) {
      pitches[[i]] <- PitchChord(p, fifths, next_)
    } else {
      if (c_ == "character" && validate.pitch_notation(p)) {
        pitches[[i]] <- to_Pitch.pitch_notation(p)
      } else if (c_ %in% c("numeric", "integer") && validate.midi(p)) {
        pitches[[i]] <- to_Pitch.midi(p, fifths, next_)
      } else if (!is.null(p)) {
        stop()
      }
    }
  }

  pitches
}


# fifths_list looks like list(c(1, -1), c(3, -7), c(8, 0))
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
