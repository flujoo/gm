#' @keywords internal
#' @export
add.Hairpin <- function(object, music) {
  to <- object$to
  i <- object$i
  j <- object$j
  lines <- music$lines
  notes <- music$notes

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, notes)
  check_i(j, line, notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  if (is.null(object$above)) object$above <- FALSE

  . <- sort(c(i, j))
  object$i <- .[1]
  object$j <- .[2]

  # Construction
  music$hairpins <- update_cases(music$hairpins, object)
  music
}


#' @keywords internal
#' @export
locate.Hairpin <- function(object, ...) {
  c(object$line, object$i, object$j)
}
