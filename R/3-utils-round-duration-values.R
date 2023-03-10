round_duration_values <- function(music) {
  notes <- music$notes
  lengths <- notes$length

  filter <- is.na(notes$duration)
  notes[filter, "length"] <- round_duration_value(lengths[filter])

  music$notes <- notes
  music
}
