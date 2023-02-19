#' @keywords internal
#' @export
add.Tie <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  # validation
  check_add_to(to, lines)
  line <- get_line_row(to, lines)
  check_tie(object$i, object$j, line, notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction
  music$ties <- update_chordal(music$ties, object, notes)
  music
}


#' @keywords internal
#' @export
locate.Tie <- function(object, ...) {
  c(object$line, object$i, object$j)
}
