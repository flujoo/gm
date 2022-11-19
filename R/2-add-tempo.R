#' @keywords internal
#' @export
add.Tempo <- function(object, music) {
  tempo <- normalize(object)
  music$tempos <- update_cases(music$tempos, tempo)
  music
}


#' @keywords internal
#' @export
normalize.Tempo <- function(object, ...) {
  if (is.null(object$bar)) object$bar <- 1L
  if (is.null(object$offset)) object$offset <- 0
  if (is.null(object$invisible)) object$invisible <- FALSE

  object
}


#' @keywords internal
#' @export
locate.Tempo <- function(object, ...) {
  c(object$bar, object$offset)
}
