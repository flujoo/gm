#' @keywords internal
#' @export
add.Stem <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  # validation
  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, music$notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction
  music$stems <- update_cases(music$stems, object)
  music
}


#' @keywords internal
#' @export
locate.Stem <- function(object, ...) {
  c(object$line, object$i)
}
