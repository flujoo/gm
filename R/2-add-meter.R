#' @keywords internal
#' @export
add.Meter <- function(object, music) {
  # normalization
  if (is.null(object$actual_number)) object$actual_number <- object$number
  if (is.null(object$actual_unit)) object$actual_unit <- object$unit
  if (is.null(object$bar)) object$bar <- 1L
  if (is.null(object$invisible)) object$invisible <- FALSE

  # construction
  music$meters <- update_cases(music$meters, object)
  music
}


#' @keywords internal
#' @export
locate.Meter <- function(object, ...) {
  object$bar
}
