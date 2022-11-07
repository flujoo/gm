#' Check If Index Exceeds Line Length
#' @noRd
check_i <- function(i, line, notes, name = "i") {
  if (is.na(i)) return(invisible())

  # the length of the Line
  n <- max(notes[notes$line == line, ]$i)
  if (i <= n) return(invisible())

  general <- sprintf("`%s` must not exceed the Line length.", name)
  specifics <- sprintf("`%s` is %s, while the Line length is %s.", name, i, n)
  erify::throw(general, specifics)
}


#' Check If Index Exceeds Chord Length
#' @noRd
check_j <- function(j, line, i, notes) {
  if (is.na(j)) return(invisible())

  # the length of the chord
  n <- max(notes[notes$line == line & notes$i == i, ]$j)
  if (is.na(n)) n <- 1L
  if (j <= n) return(invisible())

  general <- "`j` must not exceed the chord length."
  specifics <- sprintf("`j` is %s, while the chord length is %s.", j, n)
  erify::throw(general, specifics)
}


#' Check If Object Is Added to Rest
#' @noRd
check_i_rest <- function(object, line, notes) {
  i <- object$i
  if (is.na(i)) return(invisible())

  chord <- notes[notes$line == line & notes$i == i, ]
  pass <- nrow(chord) > 1 || !(is.na(chord$pitch) && is.na(chord$midi))
  if (pass) return(invisible())

  class <- class(object)
  article <- if (substr(class, 1, 1) %in% c("A", "O")) "an" else "a"
  general <- sprintf("Can not add %s %s to a rest.", article, class)
  specifics <- sprintf("It is a rest at position %s.", i)
  erify::throw(general, specifics)
}
