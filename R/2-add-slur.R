#' @keywords internal
#' @export
add.Slur <- function(object, music) {
  to <- object$to
  i <- object$i
  j <- object$j
  to_j <- object$to_j
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)
  check_to_exist(to_j, lines)

  line <- get_line_row(to, lines)
  line_j <- get_line_row(to_j, lines)

  check_i(i, line, notes)

  if (is.na(line_j)) {
    check_i(j, line, notes)
  } else {
    check_i(j, line_j, notes)
  }

  slur <- normalize(object, line, line_j)
  music$slurs <- update_cases(music$slurs, slur)
  music
}


#' @keywords internal
#' @export
normalize.Slur <- function(object, line, line_j, ...) {
  if (!is.na(line_j) && line_j == line) line_j <- NA_integer_

  if (is.na(line_j)) {
    . <- sort(c(object$i, object$j))
    object$i <- .[1]
    object$j <- .[2]

  } else if (line_j < line) {
    . <- line_j
    line_j <- line
    line <- .

    . <- object$i
    object$i <- object$j
    object$j <- .
  }

  names(object)[names(object) == "to"] <- "line"
  names(object)[names(object) == "to_j"] <- "line_j"
  object$line <- line
  object$line_j <- line_j

  object
}


#' @keywords internal
#' @export
locate.Slur <- function(object, ...) {
  c(object$line, object$i, object$j, object$line_j)
}
