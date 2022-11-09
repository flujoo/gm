#' @keywords internal
#' @export
add.Velocity <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)

  line <- get_line_row(to, lines)
  i <- object$i
  j <- object$j

  check_i(i, line, notes)
  check_j(j, line, i, notes)

  velocity <- normalize(object, line)
  music$velocities <- update_cases(music$velocities, velocity)
  music
}


#' @keywords internal
#' @export
normalize.Velocity <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  object
}


#' @keywords internal
#' @export
locate.Velocity <- function(object, ...) {
  location <- c(object$line, object$i, object$j)
  location[is.na(location)] <- 0L
  location
}
