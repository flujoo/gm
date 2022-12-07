#' @keywords internal
#' @export
add.Turn <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, music$notes)

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


#' @keywords internal
#' @export
locate.Turn <- function(object, ...) {
  c(object$line, object$i)
}
