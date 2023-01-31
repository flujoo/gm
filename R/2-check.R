#' Check If Line Exists in Music
#'
#' Check if `to` refers to an existing Line in a Music.
#'
#' @keywords internal
#' @export
check_to_exist <- function(to, lines) {
  UseMethod("check_to_exist")
}


#' @keywords internal
#' @export
check_to_exist.default <- function(to, lines) {
  # after `check_to()`, there can be only `NULL`
  return(invisible())
}


#' @keywords internal
#' @export
check_to_exist.character <- function(to, lines) {
  if (to %in% lines$name) return(invisible())
  specifics <- sprintf('Can not find Line "%s".', to)
  abort_to_exist(to, general, specifics)
}


#' @keywords internal
#' @export
check_to_exist.numeric <- function(to, lines) {
  n <- NROW(lines)
  if (to <= n) return(invisible())

  if (n == 0) {
    s_l <- "no Line"
  } else if (n == 1) {
    s_l <- "only one Line"
  } else {
    s_l <- sprintf("only %s Lines", n)
  }

  specifics <- c(
    sprintf("Can not find Line %s.", to),
    i = sprintf("The Music contains %s.", s_l)
  )

  abort_to_exist(to, general, specifics)
}


abort_to_exist <- function(to, general, specifics) {
  general <- sprintf(
    "`%s` must refer to an existing Line in the Music.",
    deparse(substitute(to)) # for `to_j`
  )

  erify::throw(general, specifics)
}


#' Check If Index Exceeds Line Length
#' @noRd
check_i <- function(i, line, notes) {
  if (is.na(i)) return(invisible())
  name <- deparse(substitute(i))

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
  if (is_not_rest(i, line, notes)) return(invisible())

  vowels <- c("Articulation", "Accidental")
  article <- if (inherits(object, vowels)) "an" else "a"

  general <- sprintf("Can not add %s %s to a rest.", article, class(object))
  specifics <- sprintf("It is a rest at position %s of Line %s.", i, line)
  erify::throw(general, specifics)
}


is_not_rest <- function(i, line, notes) {
  chord <- notes[notes$line == line & notes$i == i, ]
  nrow(chord) > 1 || !(is.na(chord$pitch) && is.na(chord$midi))
}
