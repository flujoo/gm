#' @keywords internal
#' @export
add.Articulation <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines
  notes <- music$notes

  # validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, notes)
  check_i_rest(object, line, notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction
  music$articulations <- update_cases(music$articulations, object)
  music
}


#' @keywords internal
#' @export
locate.Articulation <- function(object, ...) {
  c(object$line, object$i, object$name)
}
