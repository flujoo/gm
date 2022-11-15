#' @keywords internal
#' @export
add.Fermata <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, music$notes)

  fermata <- normalize(object, line)
  music$fermatas <- update_cases(music$fermatas, fermata)
  music
}


#' @keywords internal
#' @export
normalize.Fermata <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  object
}


#' @keywords internal
#' @export
locate.Fermata <- function(object, ...) {
  c(object$line, object$i)
}
