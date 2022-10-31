#' @keywords internal
#' @export
add.Meter <- function(object, music) {
  music$meters <- update_cases(music$meters, object)
  music
}


#' @keywords internal
#' @export
locate.Meter <- function(object, ...) {
  bar <- object$bar
  if (is.na(bar)) bar <- 1L
  bar
}
