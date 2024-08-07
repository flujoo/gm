#' @keywords internal
#' @export
add.Accidental <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines
  notes <- music$notes

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, notes)
  check_i_rest(object, line, notes)
  check_j(object$j, line, i, notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  if (is.na(object[["bracket"]])) object[["bracket"]] <- FALSE

  # Construction
  music$accidentals <- update_chordal(music$accidentals, object, notes)
  music
}


#' @keywords internal
#' @export
locate.Accidental <- function(object, ...) {
  c(object$line, object$i, object$j)
}
