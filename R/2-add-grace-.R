#' @keywords internal
#' @export
add.Grace <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_grace(i, line, music$notes)

  grace <- normalize(object, line)
  music$graces <- update_cases(music$graces, grace)
  music
}


#' @keywords internal
#' @export
normalize.Grace <- function(object, line, ...) {
  if (is.null(object$slash)) object$slash <- TRUE

  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  object
}


#' @keywords internal
#' @export
locate.Grace <- function(object, ...) {
  c(object$line, object$i)
}
