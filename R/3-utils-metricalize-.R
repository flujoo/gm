metricalize <- function(music) {
  lines <- music[["lines"]]
  notes <- music[["notes"]]
  meters <- music[["meters"]]
  metricalized <- notes[integer(), ]

  # Because notes are metricalized one by one,
  # notes in chords can be separated.
  j <- 0L
  chord <- notes[integer(), ]

  for (i in seq_len(NROW(lines))) {
    line <- lines[i, ]

    fillings <- fill_line(line, i, lines, meters)
    metricalized <- rbind(metricalized, fillings[["before"]])

    # Remember new column `line` was added
    notes_i <- notes[notes[["line"]] == line[["line"]], ]
    n <- NROW(notes_i)

    for (k in seq_len(n)) {
      note <- notes_i[k, ]
      j_k <- note[["j"]]
      metricalized_note <- metricalize_note(note, meters)
      chord_not_empty <- NROW(chord) != 0

      # Deal with the current chord and note
      if (is.na(j_k)) {
        if (chord_not_empty) {
          metricalized <- rbind(metricalized, sort_chord(chord))
          j <- 0L
          chord <- notes[integer(), ]
        }

        metricalized <- rbind(metricalized, metricalized_note)

      } else if (j_k > j) {
        j <- j_k
        chord <- rbind(chord, metricalized_note)

      } else if (j_k < j) {
        metricalized <- rbind(metricalized, sort_chord(chord))
        j <- j_k
        chord <- metricalized_note
      }

      # It has reached the end of the Line
      if (k == n && chord_not_empty) {
        metricalized <- rbind(metricalized, sort_chord(chord))
        j <- 0L
        chord <- notes[integer(), ]
      }
    }

    metricalized <- rbind(metricalized, fillings[["after"]])
  }

  music$notes <- metricalized
  music
}


sort_chord <- function(chord) {
  if (all(chord[["grace"]])) return(chord)
  chord[order(chord[["start_bar"]], chord[["start_offset"]]), ]
}
