#' @keywords internal
#' @export
add.Pedal <- function(object, music) {
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

  . <- sort(c(i, j))
  object$i <- .[1]
  object$j <- .[2]

  # construction
  music$pedals <- update_2d_cases(music$pedals, object)
  music
}
