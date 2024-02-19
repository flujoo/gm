prepare <- function(music) {
  music <- add_first_bar_meter(music)
  music <- add_global_key(music)
  music <- fill_music(music)

  music <- sort_meters(music)
  music <- round_offsets(music)
  music <- locate_notes(music)
  music <- locate_lines(music)

  music <- group_tuplets(music)
  check_tuplet_groups(music)
  check_over_bar_tuplet_groups(music)

  check_segments(music)

  music <- sort_lines(music)
  music <- metricalize(music)

  music
}
