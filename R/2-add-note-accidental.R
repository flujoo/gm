#' @keywords internal
#' @export
add.Accidental <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  # validation
  check_to_exist(to, lines)

  line <- get_line_row(to, lines)
  i <- object$i

  check_i(i, line, notes)
  check_j(object$j, line, i, notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction
  music$accidentals <- update_chordal(music$accidentals, object, notes)
  music
}


#' @keywords internal
#' @export
locate.Accidental <- function(object, ...) {
  c(object$line, object$i, object$j)
}
