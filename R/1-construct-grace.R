#' @export
Grace <- function(i, to = NULL, slash = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(slash)) erify::check_bool(slash)

  # normalization
  i <- as.integer(i)

  # construction
  grace <- list(to = to, i = i, slash = slash)
  class(grace) <- "Grace"
  grace
}


#' @export
print.Grace <- function(x, ...) {
  slash <- x$slash

  cat("Grace Note", "\n\n")
  print_to_i_j(x$to, x$i)
  if (!is.null(slash)) cat("*", if (slash) "slashed" else "not slashed", "\n")
}
