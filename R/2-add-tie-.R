#' @keywords internal
#' @export
add.Tie <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_tie(object$i, object$j, line, notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # Construction
  music$ties <- update_chordal(music$ties, object, notes)
  music
}


#' @keywords internal
#' @export
locate.Tie <- function(object, ...) {
  c(object$line, object$i, object$j)
}
