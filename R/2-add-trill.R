#' @keywords internal
#' @export
add.Trill <- function(object, music) {
  to <- object$to
  i <- object$i
  j <- object$j
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, notes)
  check_i(j, line, notes)

  trill <- normalize(object, line)
  music$trills <- update_trills(music$trills, trill)
  music
}


#' @keywords internal
#' @export
normalize.Trill <- function(object, line, ...) {
  j <- object$j

  if (!is.na(j)) {
    . <- sort(c(object$i, j))
    object$i <- .[1]
    object$j <- .[2]
  }

  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  object
}


update_trills <- function(trills, trill) {

}
