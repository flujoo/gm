normalize_music <- function(music) {
  music <- initialize_first_bar_meter(music)
  music <- initialize_notes_lines(music)
  music <- initialize_global_key(music)
  music <- normalize_bars_offsets(music)

  music
}
