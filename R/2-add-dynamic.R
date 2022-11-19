#' @keywords internal
#' @export
add.Dynamic <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, music$notes)

  dynamic <- normalize(object, line)
  music$dynamics <- update_cases(music$dynamics, dynamic)
  music
}


#' @keywords internal
#' @export
normalize.Dynamic <- function(object, line, ...) {
  above <- object$above
  if (is.null(above)) above <- FALSE

  dynamic <- list(
    marking = object$marking,
    velocity = object$velocity,
    line = line,
    i = object$i,
    above = above
  )

  class(dynamic) <- "Dynamic"
  dynamic
}


#' @keywords internal
#' @export
locate.Dynamic <- function(object, ...) {
  c(object$line, object$i)
}
