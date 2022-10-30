#' @keywords internal
#' @export
add.Instrument <- function(object, music) {
  lines <- music$lines

  check_to_exist(object, lines)

  instrument <- to_case(object, lines)
  music$instruments <- update_cases(music$instruments, instrument, lines)
  music
}


#' @keywords internal
#' @export
to_case.Instrument <- function(object, lines, ...) {
  volume <- object$volume
  pan <- object$pan

  # normalization
  if (is.null(volume)) volume <- NA_integer_
  if (is.null(pan)) pan <- NA_integer_

  instrument <- data_frame(
    midi = object$midi,
    name = object$name,
    line = get_line_row(object$to, lines),
    volume = volume,
    pan = pan
  )
  class(instrument) <- c(class(instrument), "instrument")
  instrument
}


#' @keywords internal
#' @export
locate.instrument <- function(case, lines, ...) {
  locate_to(case$line, lines)$part
}
