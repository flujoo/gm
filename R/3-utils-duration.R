indicate_measure_rests <- function(notes, meters) {
  notes[["measure_rest"]] <- FALSE
  bars <- meters[["bar"]]

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    if (!is.na(note[["midi"]])) next

    # Find the Meter for the current bar
    meter <- meters[find_by_bar(note[["start_bar"]], bars), ]
    value <- to_value(meter)

    if (note[["length"]] == value) notes[k, ][["measure_rest"]] <- TRUE
  }

  notes
}


infer_durations <- function(notes) {
  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    if (note[["measure_rest"]] || !is.na(note[["duration"]])) next

    durations <- duration_types[["name"]]
    values <- duration_types[["value"]]
    duration <- durations[values == note[["length"]]]
    notes[k, ][["duration"]] <- duration
  }

  notes
}


atomize_notes <- function(notes) {
  atomized <- notes[integer(), ]

  # Because notes are atomized one by one,
  # notes in chords can be separated.
  j <- 0L
  chord <- notes[integer(), ]

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    untied_note <- atomize_note(note)

    j_k <- note[["j"]]
    chord_not_empty <- NROW(chord) != 0

    if (is.na(j_k)) {
      if (chord_not_empty) {
        atomized <- rbind(atomized, sort_chord(chord))
        j <- 0L
        chord <- notes[integer(), ]
      }

      atomized <- rbind(atomized, untied_note)

    } else if (j_k > j) {
      j <- j_k
      chord <- rbind(chord, untied_note)

    } else if (j_k < j) {
      atomized <- rbind(atomized, sort_chord(chord))
      j <- j_k
      chord <- untied_note
    }
  }

  # In case the chord is at the end
  rbind(atomized, sort_chord(chord))
}


atomize_note <- function(note) {
  to_skip <-
    note[["grace"]] ||
    note[["measure_rest"]] ||
    !is.na(note[["duration"]])

  if (to_skip) return(note)

  values <- atomize_duration_value(note[["length"]])
  n <- length(values)

  if (n == 1) return(note)

  untied_note <- note[rep(1, n), ]
  rownames(untied_note) <- NULL
  untied_note[["length"]] <- values

  offsets <- cumsum(c(note[["start_offset"]], values))
  untied_note[["start_offset"]] <- offsets[-(n + 1)]
  untied_note[["end_offset"]] <- offsets[-1]

  untied_note
}


#' Split Duration Value Into Duration Type Values
#' @noRd
atomize_duration_value <- function(value) {
  values <- duration_types[["value"]]

  if (value %in% values) return(value)
  if (value < rev(values)[1]) stop()

  head <- values[value > values][1]
  c(atomize_duration_value(value - head), head)
}
