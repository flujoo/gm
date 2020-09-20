# create Key object -------------------------------------------------

validate.fifths <- function(fifths) {
  fifths >= -7 && fifths <= 7 && as.integer(fifths) == fifths
}


Key <- function(fifths, positions = NULL) {
  if (!validate.fifths(fifths)) {
    stop('argument "fifths" is invalid')
  }

  # validate and normalize argument "positions" ...

  k <- list(fifths = fifths, positions = positions)
  class(k) <- "Key"
  k
}



# get information from Key object -----------------------------------

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
