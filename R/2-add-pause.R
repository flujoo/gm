#' @keywords internal
#' @export
add.Pause <- function(object, music) {
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
  music$pauses <- update_cases(music$pauses, object)
  music
}


#' @keywords internal
#' @export
locate.Pause <- function(object, ...) {
  c(object$line, object$i)
}
