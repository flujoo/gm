check_chord_positions <- function(positions) {
  erify::check_type(positions, c("double", "integer", "list"))
  erify::check_length(positions, c(0, NA))

  if (is.numeric(positions)) {
    general <- paste(
      "If `positions` is a numeric vector,",
      "each item of it must be a positive integer."
    )

    erify::check_contents(positions, erify::is_n, NULL, general)

  } else if (is.list(positions)) {
    general <- paste(
      "If `positions` is a list,",
      "each item of it must contain one or two positive integers."
    )

    erify::check_types(positions, c("double", "integer"), NULL, general)
    erify::check_lengths(positions, c(1, 2), NULL, general)

    valid <- expression(all(erify::is_n(x_i)))
    erify::check_contents(positions, valid, NULL, general)
  }
}
