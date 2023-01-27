#' @keywords internal
#' @export
add.Velocity <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  # validation
  check_to_exist(to, lines)

  line <- get_line_row(to, lines)
  i <- object$i
  j <- object$j

  check_i(i, line, notes)
  check_i_rest(object, line, notes)
  check_j(j, line, i, notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction
  music$velocities <- update_cases(music$velocities, object)
  music
}


#' @keywords internal
#' @export
locate.Velocity <- function(object, ...) {
  location <- c(object$line, object$i, object$j)
  location[is.na(location)] <- 0L
  location
}
