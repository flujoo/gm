#' @keywords internal
#' @export
add.Key <- function(object, music) {
  lines <- music$lines
  check_to_exist(object$to, lines)
  key <- normalize(object, lines)
  music$keys <- update_cases(music$keys, key, lines)
  music
}


#' @keywords internal
#' @export
normalize.Key <- function(object, lines, ...) {
  key <- list(
    key = object$key,
    name = to_string(object, TRUE),
    line = get_line_row(object$to, lines),
    scope = object$scope,
    bar = object$bar
  )
  class(key) <- "Key"
  key
}


#' Indicate Location of Case in `keys`
#'
#' Indicate the part, staff, and bar of a case in `keys`.
#'
#' If `line` is `NA`, the part and staff are indicated by `0`;
#' If `scope` is `"part"`, the staff is indicated by `0`.
#'
#' @keywords internal
#' @export
locate.Key <- function(object, lines, ...) {
  line <- object$line
  scope <- object$scope
  bar <- object$bar

  if (is.na(line)) {
    part <- 0L
    staff <- 0L
  } else {
    line_location <- locate_to(line, lines)
    part <- line_location$part
    staff <- line_location$staff
    if (scope == "part") staff <- 0L
  }

  if (is.na(bar)) bar <- 1L

  c(part, staff, bar)
}
