#' @keywords internal
#' @export
add.Instrument <- function(object, music) {
  lines <- music$lines

  check_to_exist(object, lines)

  instrument <- to_case(object, lines)
  instruments <- update_instruments(music$instruments, instrument, lines)
  instruments <- rbind(instruments, instrument)
  music$instruments <- instruments
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

  data_frame(
    midi = object$midi,
    name = object$name,
    line = get_line_row(object$to, lines),
    volume = volume,
    pan = pan
  )
}


update_instruments <- function(instruments, instrument, lines) {
  part <- locate_instrument(instrument, lines)

  for (i in seq_len(NROW(instruments))) {
    part_i <- locate_instrument(instruments[i, ], lines)
    if (part_i == part) return(instruments[-i, ])
  }

  instruments
}


locate_instrument <- function(instrument, lines) {
  locate_line(lines, instrument$line)$part
}
