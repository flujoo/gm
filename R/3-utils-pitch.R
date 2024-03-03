infer_pitches <- function(music) {
  notes <- music[["notes"]]
  lines <- music[["lines"]]
  keys <- locate_keys(music[["keys"]], lines)

  for (k in rev(seq_len(NROW(notes)))) {
    note <- notes[k, ]
    midi <- note[["midi"]]

    # Skip notes that already have a pitch notation and rests
    if (!is.na(note[["pitch"]]) || is.na(midi)) next

    key <- find_key(note, keys, lines)
    after <- find_after(note, notes[notes[["line"]] == note[["line"]], ])
    pitch <- to_Pitch(midi, key, after)

    music[["notes"]][k, ][["pitch"]] <- to_string(pitch)
  }

  music
}


locate_keys <- function(keys, lines) {
  located <- NULL

  for (k in seq_len(NROW(keys))) {
    key <- keys[k, ]
    . <- locate(key, lines)

    located_k <- data_frame(
      part = .[1],
      staff = .[2],
      bar = .[3],
      key = key[["key"]]
    )

    located <- rbind(located, located_k)
  }

  located
}


find_key <- function(note, keys, lines) {
  line <- lines[note[["line"]], ]
  part <- line[["part"]]
  staff <- line[["staff"]]
  bar <- note[["start_bar"]]

  # Select the relevant keys
  keys <- keys[keys[["part"]] %in% c(0L, part), ]
  keys <- keys[keys[["staff"]] %in% c(0L, staff), ]
  keys <- keys[keys[["bar"]] <= bar, ]

  . <- order(
    keys[["part"]], keys[["staff"]], keys[["bar"]],
    decreasing = TRUE
  )

  keys[., ][1, ][["key"]]
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
