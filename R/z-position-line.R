# `to` must refer to a Line's name or index
check_to <- function(to) {
  erify::check_type(to, c("character", "double", "integer"))

  general <- "`to` must be a single character or a single positive integer."

  if (is.character(to)) {
    erify::check_string(to, general = general)
  } else if (is.numeric(to)) {
    erify::check_n(to, general = general)
  }
}
