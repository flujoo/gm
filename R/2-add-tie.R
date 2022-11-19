#' @keywords internal
#' @export
add.Tie <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)

  line <- get_line_row(to, lines)
  i <- object$i
  j <- object$j

  check_i(i, line, notes)
  check_j(j, line, i, notes)
  check_i_rest(object, line, notes)

  tie <- normalize(object, line, notes)
  ties <- music$ties

  for (j in tie$j) {
    tie$j <- j
    ties <- update_cases(ties, tie)
  }

  music$ties <- ties
  music
}


#' @keywords internal
#' @export
normalize.Tie <- function(object, line, notes, ...) {
  l <- nrow(notes[notes$line == line & notes$i == object$i, ])

  if (l == 1) {
    object$j <- NA_integer_
  } else if (is.na(object$j)) {
    object$j <- 1:l
  }

  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  object
}


#' @keywords internal
#' @export
locate.Tie <- function(object, ...) {
  c(object$line, object$i, object$j)
}
