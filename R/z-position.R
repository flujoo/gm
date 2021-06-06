check_ij <- function(position) {
  erify::check_type(position, c("double", "integer"))
  erify::check_length(position, c(1, 2))

  general <- "Each item of `position` must be a positive integer."
  erify::check_contents(position, erify::is_n, NULL, general)
}
