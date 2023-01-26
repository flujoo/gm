#' @keywords internal
#' @export
add.Hairpin <- function(object, music) {
  to <- object$to
  i <- object$i
  j <- object$j
  lines <- music$lines
  notes <- music$notes

  # validation
  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, notes)
  check_i(j, line, notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  if (is.null(object$above)) object$above <- FALSE

  . <- sort(c(i, j))
  object$i <- .[1]
  object$j <- .[2]

  # construction
  music$hairpins <- update_cases(music$hairpins, object)
  music
}
