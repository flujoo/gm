#' @keywords internal
#' @export
add.Clef <- function(object, music) {
  lines <- music$lines
  to <- object$to

  check_to(to)
  check_to_exist(to, lines)

  clef <- normalize(object, lines)
  music$clefs <- update_cases(music$clefs, clef, lines)
  music
}


#' @keywords internal
#' @export
normalize.Clef <- function(object, lines, ...) {
  names(object)[names(object) == "line"] <- "staff"

  names(object)[names(object) == "to"] <- "line"
  object$line <- get_line_row(object$line, lines)

  if (is.null(object$bar)) object$bar <- 1L
  if (is.null(object$offset)) object$offset <- 0

  object
}


#' @keywords internal
#' @export
locate.Clef <- function(object, lines, ...) {
  line_location <- locate_to(object$line, lines)
  c(line_location$part, line_location$staff, object$bar, object$offset)
}
