#' @keywords internal
#' @export
add.Notehead <- function(object, music) {
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
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  chord_length <- nrow(notes[notes$line == line & notes$i == i, ])

  if (chord_length == 1) {
    j <- NA_integer_
  } else if (is.na(j)) {
    j <- 1:chord_length
  }

  # construction -----------------------------------------------
  noteheads <- music$noteheads

  for (j_i in j) {
    object$j <- j_i
    noteheads <- update_cases(noteheads, object)
  }

  music$noteheads <- noteheads
  music
}


#' @keywords internal
#' @export
locate.Notehead <- function(object, ...) {
  c(object$line, object$i, object$j)
}
