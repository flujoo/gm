#' @keywords internal
#' @export
add.Line <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_line_name(object$name, lines)
  check_to_exist(to, lines)

  music$notes <- update_notes(music$notes, object$notes, lines)
  music$lines <- add_line(lines, object)
  music
}


#' Append Notes from Line to Music
#' @noRd
update_notes <- function(music_notes, line_notes, lines) {
  notes <- rbind(
    music_notes,
    cbind(line = NROW(lines) + 1L, line_notes)
  )

  if (requireNamespace("tibble", quietly = TRUE)) {
    notes <- tibble::as_tibble(notes)
  }

  notes
}
