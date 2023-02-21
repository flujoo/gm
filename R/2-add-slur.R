#' @keywords internal
#' @export
add.Slur <- function(object, music) {
  to <- object$to
  i <- object$i
  j <- object$j
  to_j <- object$to_j
  lines <- music$lines
  notes <- music$notes

  # validation -------------------------------------------------
  check_add_to(to, lines)
  check_add_to(to_j, lines)

  line <- normalize_to(to, lines)
  line_j <- normalize_to(to_j, lines)

  check_i(i, line, notes)

  if (is.na(line_j)) {
    check_i(j, line, notes)
  } else {
    check_i(j, line_j, notes)
  }

  # normalization ----------------------------------------------
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

  # construction -----------------------------------------------
  music$slurs <- update_cases(music$slurs, object)
  music
}


#' @keywords internal
#' @export
locate.Slur <- function(object, ...) {
  c(object$line, object$i, object$j, object$line_j)
}
