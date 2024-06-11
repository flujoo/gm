#' Check If `to` Refers to Existing Line in Music
#'
#' @keywords internal
#' @export
#' @noRd
check_add_to <- function(to, lines, ...) {
  UseMethod("check_add_to")
}


#' Check If Music Is Empty When `to` Is `NULL`
#'
#' @keywords internal
#' @export
#' @noRd
check_add_to.default <- function(to, lines, object, ...) {
  if (!is.null(lines)) return(invisible())
  vowels <- c("Articulation", "Accidental", "Instrument")

  general <- sprintf(
    "Can not add %s %s to an empty Music.",
    if (inherits(object, vowels)) "an" else "a",
    class(object)
  )

  erify::throw(general)
}


#' @keywords internal
#' @export
check_add_to.character <- function(to, lines, ...) {
  if (to %in% lines$name) return(invisible())

  general <- sprintf(
    "`%s` must refer to an existing Line in the Music.",
    deparse(substitute(to)) # For `to_j`
  )

  specifics <- sprintf('Can not find Line "%s".', to)
  erify::throw(general, specifics)
}


#' @keywords internal
#' @export
check_add_to.numeric <- function(to, lines, ...) {
  # In `Key()`, `Line()` and `Velocity()`, `to` is not mandatory,
  # while in `Slur()`, `to_j` is not mandatory,
  # they are normalized to `NA_integer_` to escape this validation.
  if (is.na(to)) return(invisible())

  n <- NROW(lines)
  if (to <= n) return(invisible())

  if (n == 0) {
    s_l <- "no Line"
  } else if (n == 1) {
    s_l <- "only one Line"
  } else {
    s_l <- sprintf("only %s Lines", n)
  }

  general <- sprintf(
    "`%s` must refer to an existing Line in the Music.",
    deparse(substitute(to)) # For `to_j`
  )

  specifics <- c(
    sprintf("Can not find Line %s.", to),
    i = sprintf("The Music contains %s.", s_l)
  )

  erify::throw(general, specifics)
}


#' Check If Index Exceeds Line Length
#' @noRd
check_i <- function(i, line, notes) {
  if (is.na(i)) return(invisible())
  name <- deparse(substitute(i))

  # Line length
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

  # Chord length
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

  chord <- notes[notes$line == line & notes$i == i, ]
  not_rest <- nrow(chord) > 1 || !(is.na(chord$pitch) && is.na(chord$midi))
  if (not_rest) return(invisible())

  vowels <- c("Articulation", "Accidental")
  article <- if (inherits(object, vowels)) "an" else "a"

  general <- sprintf("Can not add %s %s to a rest.", article, class(object))
  specifics <- sprintf("It is a rest at position %s.", i)
  erify::throw(general, specifics)
}
