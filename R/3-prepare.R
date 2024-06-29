prepare <- function(music) {
  check_first_bar_meter(music[["meters"]])
  check_empty_music(music[["lines"]])

  music[["meters"]] <- sort_by_bar(music[["meters"]])
  music <- round_offsets(music)

  notes <- music[["notes"]]
  lines <- music[["lines"]]
  meters <- music[["meters"]]

  notes <- indicate_graces(notes, music[["graces"]])

  notes <- delimit_notes(notes, lines, meters)
  lines <- delimit_lines(lines, notes)
  check_segments(lines)

  notes <- group_tuplets(notes)
  check_tuplet_groups(notes)
  check_over_bar_tuplet_groups(notes)
  notes <- indicate_tuplets(notes)

  music[["keys"]] <- prepare_keys(music)
  notes <- infer_pitches(notes, lines, music[["keys"]])

  lines <- sort_lines(lines)
  notes <- metricalize(notes, lines, meters)

  notes <- indicate_measure_rests(notes, meters)
  notes <- atomize_notes(notes)
  notes <- infer_durations(notes)

  notes <- indicate_ties(notes, music[["ties"]])
  notes <- indicate_locations(notes, lines)
  notes <- indicate_velocities(notes, music[["velocities"]])
  notes <- locate_graces(notes)
  class(notes) <- c("Note", class(notes))

  music[["clefs"]] <- prepare_clefs(music[["clefs"]], lines)

  music[["notes"]] <- notes
  music[["lines"]] <- lines

  music <- prepare_linelike(music)

  music
}
