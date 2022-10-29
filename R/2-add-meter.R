#' @keywords internal
#' @export
add.Meter <- function(object, music) {
  meter <- generate_meter(object)
  meters <- rbind(music$meters, meter)

  if (requireNamespace("tibble", quietly = TRUE)) {
    meters <- tibble::as_tibble(meters)
  }

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

  data.frame(
    number = object$number,
    unit = object$unit,
    actual_number = actual_number,
    actual_unit = actual_unit,
    bar = bar,
    invisible = invisible
  )
}
