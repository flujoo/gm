#' @keywords internal
#' @export
add.Fermata <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  # validation
  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, music$notes)

  # normalization
  if (is.null(object$above)) object$above <- TRUE
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction
  music$fermatas <- update_cases(music$fermatas, object)
  music
}


#' @keywords internal
#' @export
locate.Fermata <- function(object, ...) {
  c(object$line, object$i)
}
