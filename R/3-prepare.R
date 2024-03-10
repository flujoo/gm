prepare <- function(music) {
  check_first_bar_meter(music[["meters"]])
  check_empty_music(music[["lines"]])

  music[["notes"]] <- indicate_grace(music[["notes"]], music[["graces"]])
  music[["meters"]] <- sort_meters(music[["meters"]])
  music <- round_offsets(music)

  music <- delimit_notes(music)
  music <- delimit_lines(music)

  music <- group_tuplets(music)
  check_tuplet_groups(music)
  check_over_bar_tuplet_groups(music)

  check_segments(music)

  music <- add_global_key(music)
  music <- infer_pitches(music)

  music <- sort_lines(music)
  music <- metricalize(music)

  music <- untie_notes(music)
  music <- infer_durations(music)

  music
}
