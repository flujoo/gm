prepare <- function(music) {
  # initialization
  music <- initialize_first_bar_meter(music)
  music <- initialize_notes_lines(music)
  music <- initialize_global_key(music)

  # round durations
  music <- round_offsets(music)
  music <- round_duration_values(music)

  music
}
