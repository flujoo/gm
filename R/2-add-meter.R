#' @keywords internal
#' @export
add.Meter <- function(object, music) {
  bar <- object$bar
  actual_number <- object$actual_number
  actual_unit <- object$actual_unit
  invisible <- object$invisible

  # normalization
  if (is.null(bar)) bar <- NA_integer_
  if (is.null(actual_number)) actual_number <- NA_integer_
  if (is.null(actual_unit)) actual_unit <- NA_integer_
  if (is.null(invisible)) invisible <- NA

  # generate the case
  meter <- data.frame(
    number = object$number,
    unit = object$unit,
    actual_number = actual_number,
    actual_unit = actual_unit,
    bar = bar,
    invisible = invisible
  )

  meters <- rbind(music$meters, meter)

  if (requireNamespace("tibble", quietly = TRUE)) {
    meters <- tibble::as_tibble(meters)
  }

  music$meters <- meters
  music
}
