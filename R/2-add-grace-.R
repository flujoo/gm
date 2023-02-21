#' @keywords internal
#' @export
add.Grace <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines

  # validation
  check_add_to(to, lines)
  line <- normalize_to(to, lines)
  check_grace(i, line, music$notes)

  # normalization
  if (is.null(object$slash)) object$slash <- TRUE
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction
  music$graces <- update_cases(music$graces, object)
  music
}


#' @keywords internal
#' @export
locate.Grace <- function(object, ...) {
  c(object$line, object$i)
}
