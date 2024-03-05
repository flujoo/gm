infer_durations <- function(music) {
  notes <- music[["notes"]]

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    if (!is.na(note[["duration"]])) next

    durations <- duration_types[["name"]]
    values <- duration_types[["value"]]
    duration <- durations[values == note[["length"]]]
    music[["notes"]][k, ][["duration"]] <- duration
  }

  music
}


untie_notes <- function(music) {
  notes <- music[["notes"]]
  untied_notes <- notes[integer(), ]

  # Because notes are untied one by one,
  # notes in chords can be separated.
  j <- 0L
  chord <- notes[integer(), ]

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    untied_note <- untie_note(note)

    j_k <- note[["j"]]
    chord_not_empty <- NROW(chord) != 0

    if (is.na(j_k)) {
      if (chord_not_empty) {
        untied_notes <- rbind(untied_notes, sort_chord(chord))
        j <- 0L
        chord <- notes[integer(), ]
      }

      untied_notes <- rbind(untied_notes, untied_note)

    } else if (j_k > j) {
      j <- j_k
      chord <- rbind(chord, untied_note)

    } else if (j_k < j) {
      untied_notes <- rbind(untied_notes, sort_chord(chord))
      j <- j_k
      chord <- untied_note
    }
  }

  # In case the chord is at the end
  untied_notes <- rbind(untied_notes, sort_chord(chord))

  music[["notes"]] <- untied_notes
  music
}


untie_note <- function(note) {
  if (note[["grace"]] || !is.na(note[["duration"]])) return(note)

  values <- untie_duration_value(note[["length"]])
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
untie_duration_value <- function(value) {
  values <- duration_types[["value"]]

  if (value %in% values) return(value)
  if (value < rev(values)[1]) stop()

  head <- values[value > values][1]
  c(untie_duration_value(value - head), head)
}
