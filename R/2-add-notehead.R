#' @keywords internal
#' @export
add.Notehead <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  # Validation
  check_add_to(to, lines, object)

  line <- normalize_to(to, lines)
  i <- object$i

  check_i(i, line, notes)
  check_i_rest(object, line, notes)
  check_j(object$j, line, i, notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # Construction
  music$noteheads <- update_chordal(music$noteheads, object, notes)
  music
}


#' @keywords internal
#' @export
locate.Notehead <- function(object, ...) {
  c(object$line, object$i, object$j)
}
