#' @keywords internal
#' @export
add.Tempo <- function(object, music) {
  music$tempos <- update_cases(music$tempos, object)
  music
}


#' @keywords internal
#' @export
locate.Tempo <- function(object, ...) {
  bar <- object$bar
  offset <- object$offset

  if (is.na(bar)) bar <- 1L
  if (is.na(offset)) offset <- 0

  c(bar, offset)
}
