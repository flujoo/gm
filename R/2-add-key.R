#' @keywords internal
#' @export
add.Key <- function(object, music) {
  lines <- music$lines

  check_to_exist(object, lines)

  key <- to_case(object, lines)
  keys <- update_keys(music$keys, key, lines)
  keys <- rbind(keys, key)
  music$keys <- keys
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

  data_frame(
    key = object$key,
    line = get_line_row(object$to, lines),
    scope = scope,
    bar = bar
  )
}


update_keys <- function(keys, key, lines) {
  location <- locate_key(key, lines)

  for (i in seq_len(NROW(keys))) {
    location_i <- locate_key(keys[i, ], lines)
    if (all(location_i == location)) return(keys[-i, ])
  }

  keys
}


#' Indicate Location of Case in `keys`
#'
#' Indicate the part, staff, and bar of a case in `keys`.
#'
#' If `line` is `NA`, the part and staff are indicated by `0`;
#' If `scope` is `"part"`, the staff is indicated by `0`.
#'
#' @noRd
locate_key <- function(key, lines) {
  line <- key$line
  scope <- key$scope
  bar <- key$bar

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
