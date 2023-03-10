#' Degenerate Dotted or Tuplet Grace Notes
#'
#' MuseScore can't handle dotted or tuplet grace notes.
#' Degenerate the durations of them to duration values.
#'
#' @noRd
degenerate_grace_durations <- function(music) {
  graces <- music$graces
  if (is.null(graces)) return(music)

  notes <- music$notes

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]

    to_degenerate <-
      # check if it is a grace note
      any(graces$line == note$line & graces$i == note$i) &&
      # check if it is a dotted or tuplet note
      grepl("\\.|/", note$duration)

    if (to_degenerate) music$notes[k, "duration"] <- NA_character_
  }

  music
}


round_duration_values <- function(music) {
  notes <- music$notes
  lengths <- notes$length

  filter <- is.na(notes$duration)
  notes[filter, "length"] <- round_duration_value(lengths[filter])

  music$notes <- notes
  music
}
