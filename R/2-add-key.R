#' @keywords internal
#' @export
add.Key <- function(object, music) {
  lines <- music$lines

  check_to_exist(object, lines)

  key <- to_case(object, lines)
  music$keys <- update_cases(music$keys, key, lines)
  music
}


#' @keywords internal
#' @export
to_case.Key <- function(object, lines, ...) {
  scope <- object$scope
  bar <- object$bar

  # normalization
  if (is.null(scope)) scope <- NA_character_
  if (is.null(bar)) bar <- NA_integer_

  key <- data_frame(
    key = object$key,
    name = to_string(object, TRUE),
    line = get_line_row(object$to, lines),
    scope = scope,
    bar = bar
  )
  class(key) <- c(class(key), "key")
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
locate.key <- function(case, lines, ...) {
  line <- case$line
  scope <- case$scope
  bar <- case$bar

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
