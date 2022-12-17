#' @keywords internal
#' @export
add.Stem <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, music$notes)

  stem <- normalize(object, line)
  music$stems <- update_cases(music$stems, stem)
  music
}


#' @keywords internal
#' @export
normalize.Stem <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  object
}


#' @keywords internal
#' @export
locate.Stem <- function(object, ...) {
  c(object$line, object$i)
}
