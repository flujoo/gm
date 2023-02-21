#' @keywords internal
#' @export
add.Dynamic <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  # validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, music$notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  if (is.null(object$above)) object$above <- FALSE

  # construction
  music$dynamics <- update_cases(music$dynamics, object)
  music
}


#' @keywords internal
#' @export
locate.Dynamic <- function(object, ...) {
  c(object$line, object$i)
}
