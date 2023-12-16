#' @keywords internal
#' @export
add.Dynamic <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, music$notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  if (is.null(object$above)) object$above <- FALSE

  # Construction
  music$dynamics <- update_cases(music$dynamics, object)
  music
}


#' @keywords internal
#' @export
locate.Dynamic <- function(object, ...) {
  c(object$line, object$i)
}
