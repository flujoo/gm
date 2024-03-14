prepare <- function(music) {
  check_first_bar_meter(music[["meters"]])
  check_empty_music(music[["lines"]])

  music[["meters"]] <- sort_meters(music[["meters"]])
  music <- round_offsets(music)

  notes <- music[["notes"]]
  lines <- music[["lines"]]
  meters <- music[["meters"]]

  notes <- indicate_graces(notes, music[["graces"]])

  notes <- delimit_notes(notes, lines, meters)
  lines <- delimit_lines(lines, notes)

  notes <- group_tuplets(notes)
  check_tuplet_groups(notes)
  check_over_bar_tuplet_groups(notes)

  check_segments(lines)

  music <- add_global_key(music)
  notes <- infer_pitches(notes, lines, music[["keys"]])

  lines <- sort_lines(lines)
  notes <- metricalize(notes, lines, meters)

  notes <- atomize_notes(notes)
  notes <- infer_durations(notes)

  notes <- indicate_ties(notes, music[["ties"]])

  music[["notes"]] <- notes
  music[["lines"]] <- lines
  music[["meters"]] <- meters

  music
}
