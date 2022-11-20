#' Normalize `pitches` and `durations` in `Line()` to Notes
#' @noRd
normalize_notes <- function(pitches, durations) {
  . <- normalize_pitches_durations(pitches, durations)
  pitches <- .$pitches
  durations <- .$durations
  l <- .$length

  notes <- NULL

  for (i in 1:l) {
    note <- normalize_note(i, pitches[[i]], durations[[i]])
    notes <- rbind(notes, note)
  }

  notes
}


#' Recycle Shorter Argument Between `pitches` and `durations`
#' @noRd
normalize_pitches_durations <- function(pitches, durations) {
  lps <- length(pitches)
  lds <- length(durations)
  lm <- max(lps, lds)

  if (lps < lm) {
    if (lps == 0) pitches <- NA
    pitches <- rep_len(pitches, lm)

  } else if (lds < lm) {
    if (lds == 0) durations <- 1
    durations <- rep_len(durations, lm)
  }

  list(pitches = pitches, durations = durations, length = lm)
}


normalize_note <- function(i, pitch, duration) {
  j <- NA_integer_

  if (is.character(duration)) {
    duration_notation <- duration
    duration_value <- NA_real_
  } else {
    duration_notation <- NA_character_
    duration_value <- duration
  }

  core <- function(i, pitch) {
    l <- length(pitch)

    if (l == 0) {
      j <<- NA_integer_
      return(NULL)
    }

    if (l > 1 && is.na(j)) j <<- 1L
    p <- pitch[1]
    ps <- pitch[-1]

    if (is_pitch_notation(p)) {
      pitch_notation <- p
      pitch_value <- NA_integer_
    } else if (is_pitch_value(p)) {
      pitch_notation <- NA_character_
      pitch_value <- as.integer(p)
    } else {
      pitch_notation <- NA_character_
      pitch_value <- NA_integer_
    }

    note <- data_frame(
      i = i, j = j,
      pitch = pitch_notation, midi = pitch_value,
      duration = duration_notation, length = duration_value
    )

    if (!is.na(j)) j <<- j + 1L
    rbind(note, core(i, ps))
  }

  core(i, pitch)
}
