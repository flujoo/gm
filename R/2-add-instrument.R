#' @keywords internal
#' @export
add.Instrument <- function(object, music) {
  lines <- music$lines
  check_to_exist(object$to, lines)
  instrument <- normalize(object, lines)
  music$instruments <- update_cases(music$instruments, instrument, lines)
  music
}


#' @keywords internal
#' @export
normalize.Instrument <- function(object, lines, ...) {
  object$to <- get_line_row(object$to, lines)
  names(object)[names(object) == "to"] <- "line"
  object
}


#' @keywords internal
#' @export
locate.Instrument <- function(object, lines, ...) {
  locate_to(object$line, lines)$part
}
