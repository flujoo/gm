infer_pitches <- function(notes, lines, keys) {
  for (k in rev(seq_len(NROW(notes)))) {
    note <- notes[k, ]
    midi <- note[["midi"]]

    # Skip notes that already have a pitch notation and rests
    if (!is.na(note[["pitch"]]) || is.na(midi)) next

    key <- find_key(note, notes, keys, lines)
    after <- find_after(note, notes[notes[["line"]] == note[["line"]], ])
    pitch <- to_Pitch(midi, key, after)

    notes[k, ][["pitch"]] <- to_string(pitch)
  }

  notes
}


find_key <- function(note, notes, keys, lines) {
  # If it's a grace note, find the note it attaches to
  repeat {
    if (!note[["grace"]]) break
    i <- note[["i"]] + 1
    chord <- notes[notes[["line"]] == note[["line"]] & notes[["i"]] == i, ]
    note <- chord[1, ]
  }

  line <- lines[note[["line"]], ]

  # Select the relevant keys
  is_relevant <-
    keys[["part"]] == line[["part"]] &
    keys[["staff"]] == line[["staff"]] &
    keys[["bar"]] <= note[["start_bar"]]

  # Already reverse-ordered on `bar`
  keys[is_relevant, ][1, ][["key"]]
}


#' @param notes Only notes from the same Line.
#' @noRd
find_after <- function(note, notes) {
  i <- note[["i"]]
  midi <- note[["midi"]]
  not_grace <- !note[["grace"]]
  max_i <- max(notes[["i"]])

  repeat {
    i <- i + 1L
    if (i > max_i) return(NA_character_)

    # Keep in mind that it can be a chord
    chord <- notes[notes[["i"]] == i, ]

    # Skip grace notes if the current note is not one
    if (not_grace && all(chord[["grace"]])) next

    pitches <- chord[["pitch"]]
    if (all(is.na(pitches))) next

    # Select the closest pitch from the chord
    differences <- abs(chord[["midi"]] - midi)
    pitch <- pitches[differences == min(differences)][1]
    return(pitch)
  }
}
