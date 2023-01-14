#' @keywords internal
#' @export
add.Tempo <- function(object, music) {
  # normalization
  if (is.null(object$bar)) object$bar <- 1L
  if (is.null(object$offset)) object$offset <- 0
  if (is.null(object$invisible)) object$invisible <- FALSE

  # construction
  music$tempos <- update_cases(music$tempos, object)
  music
}


#' @keywords internal
#' @export
locate.Tempo <- function(object, ...) {
  c(object$bar, object$offset)
}
