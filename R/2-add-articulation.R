#' @keywords internal
#' @export
add.Articulation <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(object$i, line, music$notes)

  articulation <- normalize(object, line)
  music$articulations <- update_cases(music$articulations, articulation)
  music
}


#' @keywords internal
#' @export
normalize.Articulation <- function(object, line, ...) {
  articulation <- list(
    name = object$name,
    line = line,
    i = object$i
  )
  class(articulation) <- "Articulation"
  articulation
}


#' @keywords internal
#' @export
locate.Articulation <- function(object, ...) {
  c(object$line, object$i, object$name)
}
