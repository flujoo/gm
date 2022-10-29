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


#' Update `meters` in Music
#'
#' Remove the case in `meters` that has the same `bar`
#' as the incoming case.
#'
#' @noRd
update_meters <- function(meters, meter) {
  bar <- locate_meter(meter)

  for (i in seq_len(NROW(meters))) {
    bar_i <- locate_meter(meters[i, ])
    if (bar_i == bar) return(meters[-i, ])
  }

  meters
}


locate_meter <- function(meter) {
  bar <- meter$bar
  if (is.na(bar)) bar <- 1L
  bar
}
