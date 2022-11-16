#' @export
Grace <- function(to, i, slash = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(slash)) erify::check_bool(slash)

  # normalization
  i <- as.integer(i)
  if (is.null(slash)) slash <- NA

  # construction
  grace <- list(to = to, i = i, slash = slash)
  class(grace) <- "Grace"
  grace
}


#' @export
print.Grace <- function(x, ...) {
  to <- x$to
  i <- x$i
  slash <- x$slash

  cat("Grace Note", "\n\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be applied to Line", s_to, "\n")
  cat("* to be applied to position", i, "\n")

  if (!is.na(slash)) cat("*", if (slash) "slashed" else "not slashed", "\n")
}
