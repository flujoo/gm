#' @keywords internal
#' @export
add.Meter <- function(object, music) {
  meter <- generate_meter(object)
  meters <- update_meters(music$meters, meter)
  meters <- rbind(meters, meter)
  music$meters <- meters
  music
}


#' Generate Case for `meters` in Music
#' @noRd
generate_meter <- function(object) {
  bar <- object$bar
  actual_number <- object$actual_number
  actual_unit <- object$actual_unit
  invisible <- object$invisible

  # normalization
  if (is.null(bar)) bar <- NA_integer_
  if (is.null(actual_number)) actual_number <- NA_integer_
  if (is.null(actual_unit)) actual_unit <- NA_integer_
  if (is.null(invisible)) invisible <- NA

  data_frame(
    number = object$number,
    unit = object$unit,
    actual_number = actual_number,
    actual_unit = actual_unit,
    bar = bar,
    invisible = invisible
  )
}


update_meters <- function(meters, meter) {
  bar <- meter$bar

  for (i in seq_len(NROW(meters))) {
    bar_i <- meters$bar[i]

    match <- (bar_i %in% c(1L, NA) && bar %in% c(1L, NA)) ||
      identical(bar_i, bar)

    if (match) {
      meters <- meters[-i, ]
      return(meters)
    }
  }

  meters
}
