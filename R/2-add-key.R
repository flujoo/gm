#' @keywords internal
#' @export
add.Key <- function(object, music) {
  check_to_exist(object$to, music$lines, "Key")

  key <- generate_key(object, music$lines)
  keys <- rbind(music$keys, key)

  if (requireNamespace("tibble", quietly = TRUE)) {
    keys <- tibble::as_tibble(keys)
  }

  music$keys <- keys
  music
}


#' Generate Case for `keys` in Music
#' @noRd
generate_key <- function(object, lines) {
  scope <- object$scope
  bar <- object$bar

  # normalization
  if (is.null(scope)) scope <- NA_character_
  if (is.null(bar)) bar <- NA_integer_

  data.frame(
    key = object$key,
    line = get_line_row(object$to, lines),
    scope = scope,
    bar = bar
  )
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
