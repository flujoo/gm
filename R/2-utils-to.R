#' Check If Line Exists in Music
#'
#' Check if `to` refers to an existing Line in a Music.
#'
#' @keywords internal
#' @export
check_add_to <- function(to, lines) {
  UseMethod("check_add_to")
}


#' @keywords internal
#' @export
check_add_to.default <- function(to, lines) {
  # after `check_to()`, there can be only `NULL`
  return(invisible())
}


#' @keywords internal
#' @export
check_add_to.character <- function(to, lines) {
  if (to %in% lines$name) return(invisible())
  specifics <- sprintf('Can not find Line "%s".', to)
  abort_add_to(to, general, specifics)
}


#' @keywords internal
#' @export
check_add_to.numeric <- function(to, lines) {
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
get_line_row <- function(to, lines) {
  if (is.null(to) || is.null(lines)) {
    NA_integer_
  } else if (is.numeric(to)) {
    as.integer(to)
  } else if (is.character(to)) {
    which(lines$name == to)
  }
}
