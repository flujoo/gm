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
  clef <- list(
    sign = object$sign,
    staff_line = object$line,
    octave = object$octave,
    name = to_string(object),
    line = get_line_row(object$to, lines),
    bar = object$bar,
    offset = object$offset
  )
  class(clef) <- "Clef"
  clef
}


#' @keywords internal
#' @export
locate.Clef <- function(object, lines, ...) {
  line <- object$line
  bar <- object$bar
  offset <- object$offset

  line_location <- locate_to(line, lines)
  part <- line_location$part
  staff <- line_location$staff

  if (is.na(bar)) bar <- 1L
  if (is.na(offset)) offset <- 0

  c(part, staff, bar, offset)
}
