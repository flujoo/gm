#' @keywords internal
#' @export
add.Dynamic <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(object$i, line, music$notes)

  dynamic <- normalize(object, line)
  music$dynamics <- update_cases(music$dynamics, dynamic)
  music
}


#' @keywords internal
#' @export
normalize.Dynamic <- function(object, line, ...) {
  dynamic <- list(
    marking = object$marking,
    velocity = object$velocity,
    line = line,
    i = object$i,
    above = object$above
  )
  class(dynamic) <- "Dynamic"
  dynamic
}


#' @keywords internal
#' @export
locate.Dynamic <- function(object, ...) {
  c(object$line, object$i)
}
