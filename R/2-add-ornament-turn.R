#' @keywords internal
#' @export
add.Turn <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, notes)

  turn <- normalize(object, line)
  update_ornaments(music, turn)
}


#' @keywords internal
#' @export
normalize.Turn <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  object
}
