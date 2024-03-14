indicate_original_ties <- function(notes, ties) {
  ids <- notes[["id"]]
  lines <- notes[["line"]]
  is <- notes[["i"]]
  midis <- notes[["midi"]]

  for (id_start in ties[["id"]]) {
    # Remember that after metricalization and atomization,
    # there can be more than one note with a common ID.
    notes_start <- notes[ids == id_start, ]

    # For the start position, only indicate the last one
    n <- NROW(notes_start)
    notes[ids == id_start, ][n, ][["tie_start"]] <- TRUE

    note_start <- notes_start[n, ]

    # If a note is possibly at the stop position
    is_stop <-
      lines == note_start[["line"]] &
      is == note_start[["i"]] + 1 &
      midis == note_start[["midi"]] # Don’t know the index or ID

    # There can also be more than one note
    notes_stop <- notes[is_stop, ]
    js <- notes_stop[["j"]]

    # Single note at the stop position
    if (all(is.na(js))) {

      # For the stop position, only indicate the first one
      notes[is_stop, ][1, ][["tie_stop"]] <- TRUE

      next
    }

    # There may be more than one note with an equivalent pitch
    for (j in unique(js)) {
      note_stop <- notes_stop[js == j, ][1, ]

      # Make sure the note is not already tied
      if (!note_stop[["tie_stop"]]) {

        # Only indicate the first one
        id_stop <- note_stop[["id"]]
        notes[ids == id_stop, ][1, ][["tie_stop"]] <- TRUE

        break
      }
    }
  }

  notes
}
