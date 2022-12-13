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
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  if (is.null(object$above)) object$above <- FALSE

  object
}


#' @keywords internal
#' @export
locate.Dynamic <- function(object, ...) {
  c(object$line, object$i)
}
