#' @keywords internal
#' @export
add.Schleifer <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, music$notes)

  schleifer <- normalize(object, line)
  update_ornaments(music, schleifer)
}


#' @keywords internal
#' @export
normalize.Schleifer <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  object
}
