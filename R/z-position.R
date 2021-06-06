# check `position` in `Tie()`
check_ij <- function(position) {
  general <-
    "`position` must be a numeric vector of one or two positive integers."

  erify::check_type(position, c("double", "integer"), NULL, general)
  erify::check_length(position, c(1, 2), NULL, general)

  if (length(position) == 1) {
    erify::check_n(position, general = general)
  } else {
    erify::check_contents(position, erify::is_n, NULL, general)
  }
}
