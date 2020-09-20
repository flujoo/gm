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
  grepl(reg, pitch_notation)
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
  midi >= 12 && midi <= 127 && midi == as.integer(midi)
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


to_Pitch.midi <- function(midi, fifths, next_ = NULL) {
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
