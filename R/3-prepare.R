prepare <- function(music) {
  music <- add_first_bar_meter(music)
  music <- fill_empty_music(music)
  music <- indicate_grace(music)

  music <- sort_meters(music)
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
