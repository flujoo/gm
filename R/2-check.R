#' Check If Line Exists in Music
#'
#' Check if `to` refers to an existing Line in a Music.
#'
#' @noRd
check_to_exist <- function(to, lines) {
  if (is.null(to)) return(invisible())

  n_lines <- NROW(lines)

  if (is.character(to)) {
    if (to %in% lines$name) {
      return(invisible())
    } else {
      specifics <- sprintf('Can not find Line "%s".', to)
    }

  } else if (is.numeric(to)) {
    if (to <= n_lines) {
      return(invisible())
    } else {
      if (n_lines == 0) {
        s_l <- "no Line"
      } else if (n_lines == 1) {
        s_l <- "only one Line"
      } else {
        s_l <- sprintf("only %s Lines", n_lines)
      }

      specifics <- c(
        sprintf("Can not find Line %s.", to),
        i = sprintf("The Music contains %s.", s_l)
      )
    }
  }

  general <- sprintf(
    "`%s` must refer to an existing Line in the Music.",
    deparse(substitute(to))
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
