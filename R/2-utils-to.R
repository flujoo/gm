#' Check If Line Exists in Music
#'
#' Check if `to` refers to an existing Line in a Music.
#'
#' @keywords internal
#' @export
check_add_to <- function(to, lines, ...) {
  UseMethod("check_add_to")
}


#' @keywords internal
#' @export
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
  specifics <- sprintf('Can not find Line "%s".', to)
  abort_add_to(to, general, specifics)
}


#' @keywords internal
#' @export
check_add_to.numeric <- function(to, lines, ...) {
  # in `Key()` and `Line()`, `to` is not mandatory
  # it is normalized to `NA_integer_` to escape this validation
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

  specifics <- c(
    sprintf("Can not find Line %s.", to),
    i = sprintf("The Music contains %s.", s_l)
  )

  abort_add_to(to, general, specifics)
}


abort_add_to <- function(to, general, specifics) {
  general <- sprintf(
    "`%s` must refer to an existing Line in the Music.",
    deparse(substitute(to)) # for `to_j`
  )

  erify::throw(general, specifics)
}


#' Get Line's Row in `lines` of Music
#'
#' Get the row number of the Line that `to` refers to.
#'
#' @noRd
normalize_to <- function(to, lines) {
  if (is.numeric(to)) {
    as.integer(to)

  } else if (is.character(to)) {
    which(lines$name == to)

  } else if (is.null(to)) {
    nrow(lines)
  }
}
