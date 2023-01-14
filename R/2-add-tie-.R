#' @keywords internal
#' @export
add.Tie <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  # validation -------------------------------------------------
  check_to_exist(to, lines)

  line <- get_line_row(to, lines)
  i <- object$i
  j <- object$j

  check_tie(i, j, line, notes)

  # normalization ----------------------------------------------
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  chord_length <- nrow(notes[notes$line == line & notes$i == i, ])

  if (chord_length == 1) {
    j <- NA_integer_
  } else if (is.na(j)) {
    j <- 1:chord_length
  }

  # construction -----------------------------------------------
  ties <- music$ties

  for (j_i in j) {
    object$j <- j_i
    ties <- update_cases(ties, object)
  }

  music$ties <- ties
  music
}


#' @keywords internal
#' @export
locate.Tie <- function(object, ...) {
  c(object$line, object$i, object$j)
}
