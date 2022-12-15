#' @keywords internal
#' @export
add.Notehead <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)

  line <- get_line_row(to, lines)
  i <- object$i
  j <- object$j

  notehead <- normalize(object, line, notes)
  noteheads <- music$noteheads

  for (j in notehead$j) {
    notehead$j <- j
    noteheads <- update_cases(noteheads, notehead)
  }

  music$noteheads <- noteheads
  music
}


#' @keywords internal
#' @export
normalize.Notehead <- function(object, line, notes, ...) {
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
locate.Notehead <- function(object, ...) {
  c(object$line, object$i, object$j)
}
