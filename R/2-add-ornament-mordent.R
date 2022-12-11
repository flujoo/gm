#' @keywords internal
#' @export
add.Mordent <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, notes)
  check_i_rest(object, line, notes)

  mordent <- normalize(object, line)
  update_ornaments(music, mordent)
}


#' @keywords internal
#' @export
normalize.Mordent <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  object
}
