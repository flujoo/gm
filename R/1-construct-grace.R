#' @export
Grace <- function(i, to = NULL, slash = NULL) {
  # Validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(slash)) erify::check_bool(slash)

  # Normalization
  i <- as.integer(i)

  # Construction
  structure(
    list(to = to, i = i, slash = slash),
    class = "Grace"
  )
}


#' @export
print.Grace <- function(x, ...) {
  slash <- x$slash

  cat("Grace Note", "\n\n")
  print_to_i_j(x$to, x$i)
  if (!is.null(slash)) cat("*", if (slash) "slashed" else "not slashed", "\n")
}
