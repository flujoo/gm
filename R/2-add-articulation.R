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
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  object
}


#' @keywords internal
#' @export
locate.Articulation <- function(object, ...) {
  c(object$line, object$i, object$name)
}