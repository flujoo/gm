#' @keywords internal
#' @export
add.Clef <- function(object, music) {
  to <- object$to
  lines <- music$lines

  # validation
  check_add_to(to, lines, object)

  # normalization
  object$name <- to_string(object)
  names(object)[names(object) == "line"] <- "staff"
  names(object)[names(object) == "to"] <- "line"
  object$line <- normalize_to(object$line, lines)
  if (is.null(object$bar)) object$bar <- 1L
  if (is.null(object$offset)) object$offset <- 0

  # construction
  music$clefs <- update_cases(music$clefs, object, lines)
  music
}


#' @keywords internal
#' @export
locate.Clef <- function(object, lines, ...) {
  line_location <- locate_to(object$line, lines)
  c(line_location$part, line_location$staff, object$bar, object$offset)
}
