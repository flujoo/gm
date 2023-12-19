#' @keywords internal
#' @export
add.Fermata <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, music$notes)

  # Normalization
  if (is.null(object$above)) object$above <- TRUE
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # Construction
  music$fermatas <- update_cases(music$fermatas, object)
  music
}


#' @keywords internal
#' @export
locate.Fermata <- function(object, ...) {
  c(object$line, object$i)
}
