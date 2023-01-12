#' @keywords internal
#' @export
add.Accidental <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  # validation -------------------------------------------------
  check_to_exist(to, lines)

  line <- get_line_row(to, lines)
  i <- object$i
  j <- object$j

  check_i(i, line, notes)
  check_j(j, line, i, notes)

  # normalization ----------------------------------------------
  l <- nrow(notes[notes$line == line & notes$i == object$i, ])

  if (l == 1) {
    object$j <- NA_integer_
  } else if (is.na(object$j)) {
    object$j <- 1:l
  }

  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction -----------------------------------------------
  accidentals <- music$accidentals

  for (j in object$j) {
    object$j <- j
    accidentals <- update_cases(accidentals, object)
  }

  music$accidentals <- accidentals
  music
}


#' @keywords internal
#' @export
locate.Accidental <- function(object, ...) {
  c(object$line, object$i, object$j)
}
