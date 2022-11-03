#' @keywords internal
#' @export
add.Pedal <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(object$i, line, notes)
  check_i(object$j, line, notes, "j")

  pedal <- normalize(object, line)
  music$pedals <- update_cases(music$pedals, pedal)
  music
}


#' @keywords internal
#' @export
normalize.Pedal <- function(object, line, ...) {
  . <- sort(c(object$i, object$j))
  i <- .[1]
  j <- .[2]

  pedal <- list(line = line, i = i, j = j)
  class(pedal) <- "Pedal"
  pedal
}


#' @keywords internal
#' @export
locate.Pedal <- function(object, ...) {
  c(object$line, object$i, object$j)
}
