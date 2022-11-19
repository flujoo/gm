#' @keywords internal
#' @export
add.Meter <- function(object, music) {
  meter <- normalize(object)
  music$meters <- update_cases(music$meters, meter)
  music
}


#' @keywords internal
#' @export
normalize.Meter <- function(object, ...) {
  if (is.null(object$actual_number)) object$actual_number <- object$number
  if (is.null(object$actual_unit)) object$actual_unit <- object$unit
  if (is.null(object$bar)) object$bar <- 1L
  if (is.null(object$invisible)) object$invisible <- FALSE

  object
}


#' @keywords internal
#' @export
locate.Meter <- function(object, ...) {
  object$bar
}
