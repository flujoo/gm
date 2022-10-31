#' @keywords internal
#' @export
add.Instrument <- function(object, music) {
  lines <- music$lines
  check_to_exist(object, lines)
  instrument <- normalize(object, lines)
  music$instruments <- update_cases(music$instruments, instrument, lines)
  music
}


#' @keywords internal
#' @export
normalize.Instrument <- function(object, lines, ...) {
  instrument <- list(
    midi = object$midi,
    name = object$name,
    line = get_line_row(object$to, lines),
    volume = object$volume,
    pan = object$pan
  )
  class(instrument) <- "Instrument"
  instrument
}


#' @keywords internal
#' @export
locate.Instrument <- function(object, lines, ...) {
  locate_to(object$line, lines)$part
}
