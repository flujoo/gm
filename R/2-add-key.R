#' @keywords internal
#' @export
add.Key <- function(object, music) {
  to <- object$to
  lines <- music$lines

  # Validation
  check_add_to(to, lines)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- normalize_to(object$line, lines)
  if (is.null(object$bar)) object$bar <- 1L
  object$name <- to_string(object, TRUE)

  # Construction
  music$keys <- update_cases(music$keys, object, lines)
  music
}


#' Indicate Key's Part, Staff, and Bar
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
    . <- locate_to(line, lines)
    part <- .$part
    staff <- if (scope == "part") 0L else .$staff
  }

  c(part, staff, object$bar)
}
