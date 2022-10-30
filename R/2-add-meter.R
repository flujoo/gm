#' @keywords internal
#' @export
add.Meter <- function(object, music) {
  meter <- to_case(object)
  music$meters <- update_cases(music$meters, meter)
  music
}


#' @keywords internal
#' @export
to_case.Meter <- function(object, ...) {
  bar <- object$bar
  actual_number <- object$actual_number
  actual_unit <- object$actual_unit
  invisible <- object$invisible

  # normalization
  if (is.null(bar)) bar <- NA_integer_
  if (is.null(actual_number)) actual_number <- NA_integer_
  if (is.null(actual_unit)) actual_unit <- NA_integer_
  if (is.null(invisible)) invisible <- NA

  meter <- data_frame(
    number = object$number,
    unit = object$unit,
    actual_number = actual_number,
    actual_unit = actual_unit,
    bar = bar,
    invisible = invisible
  )
  class(meter) <- c(class(meter), "meter")
  meter
}


#' @keywords internal
#' @export
locate.meter <- function(case, ...) {
  bar <- case$bar
  if (is.na(bar)) bar <- 1L
  bar
}
