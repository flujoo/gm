#' @export
Stem <- function(direction, i, to = NULL) {
  # Validation
  erify::check_content(direction, c("down", "up", "double", "none"))
  erify::check_n(i)
  check_to(to)

  # Normalization
  i <- as.integer(i)

  # Construction
  structure(
    list(to = to, i = i, direction = direction),
    class = "Stem"
  )
}


#' @export
print.Stem <- function(x, ...) {
  s_direction <- switch(
    x[["direction"]],
    "down" = "Downward",
    "up" = "Upward",
    "double" = "Double",
    "none" = "Empty"
  )

  cat(s_direction, "Stem", "\n\n")
  print_to_i_j(x[["to"]], x[["i"]])
}
