#' @export
Stem <- function(direction, to, i) {
  # validation
  erify::check_content(direction, c("down", "up", "double", "none"))
  check_to(to)
  erify::check_n(i)

  # normalization
  i <- as.integer(i)

  # construction
  stem <- list(to = to, i = i, direction = direction)
  class(stem) <- "Stem"
  stem
}


#' @export
print.Stem <- function(x, ...) {
  s_direction <- switch(
    x$direction,
    "down" = "Downward",
    "up" = "Upward",
    "double" = "Double",
    "none" = "Empty"
  )

  cat(s_direction, "Stem", "\n\n")
  print_to_ij(x$to, x$i)
}
