#' @keywords internal
#' @export
add.Pause <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, music$notes)

  pause <- normalize(object, line)
  music$pauses <- update_cases(music$pauses, pause)
  music
}


#' @keywords internal
#' @export
normalize.Pause <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  object
}


#' @keywords internal
#' @export
locate.Pause <- function(object, ...) {
  c(object$line, object$i)
}
