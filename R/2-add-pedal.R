#' @keywords internal
#' @export
add.Pedal <- function(object, music) {
  to <- object$to
  i <- object$i
  j <- object$j
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, notes)
  check_i(j, line, notes)

  pedal <- normalize(object, line)
  music$pedals <- update_cases(music$pedals, pedal)
  music
}


#' @keywords internal
#' @export
normalize.Pedal <- function(object, line, ...) {
  . <- sort(c(object$i, object$j))
  object$i <- .[1]
  object$j <- .[2]

  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  object
}


#' @keywords internal
#' @export
locate.Pedal <- function(object, ...) {
  c(object$line, object$i, object$j)
}
