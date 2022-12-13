#' @keywords internal
#' @export
add.Key <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_to_exist(to, lines)

  key <- normalize(object, lines)
  music$keys <- update_cases(music$keys, key, lines)
  music
}


#' @keywords internal
#' @export
normalize.Key <- function(object, lines, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- get_line_row(object$line, lines)

  if (is.null(object$bar)) object$bar <- 1L

  object
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

  if (is.na(line)) {
    part <- 0L
    staff <- 0L
  } else {
    line_location <- locate_to(line, lines)
    part <- line_location$part
    staff <- line_location$staff
    if (scope == "part") staff <- 0L
  }

  c(part, staff, object$bar)
}
