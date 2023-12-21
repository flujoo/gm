#' @keywords internal
#' @export
add.Instrument <- function(object, music) {
  to <- object$to
  lines <- music$lines

  # Validation
  check_add_to(to, lines, object)

  # Normalization
  object$to <- normalize_to(object$to, lines)
  names(object)[names(object) == "to"] <- "line"
  if (is.null(object$volume)) object$volume <- 100L
  if (is.null(object$pan)) object$pan <- 64L

  # Construction
  music$instruments <- update_cases(music$instruments, object, lines)
  music
}


#' @keywords internal
#' @export
locate.Instrument <- function(object, lines, ...) {
  locate_to(object$line, lines)$part
}
