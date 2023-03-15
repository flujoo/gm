prepare <- function(music) {
  check_empty(music)
  music <- initialize_notes_lines(music)
  music <- initialize_global_key(music)

  music
}


check_empty <- function(music) {
  if (!is.null(music$lines)) return(invisible())
  erify::throw("`music` can not be empty.")
}
