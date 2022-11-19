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
  bar <- object$bar
  offset <- object$offset

  if (is.null(bar)) bar <- 1L
  if (is.null(offset)) offset <- 0

  clef <- list(
    sign = object$sign,
    staff_line = object$line,
    octave = object$octave,
    name = to_string(object),
    line = get_line_row(object$to, lines),
    bar = bar,
    offset = offset
  )

  class(clef) <- "Clef"
  clef
}


#' @keywords internal
#' @export
locate.Clef <- function(object, lines, ...) {
  line_location <- locate_to(object$line, lines)
  c(line_location$part, line_location$staff, object$bar, object$offset)
}
