#' @export
Velocity <- function(velocity, to = NULL, i = NULL, j = NULL) {
  # validation
  erify::check_interval(velocity, c(0L, 127L))
  if (!is.null(to)) check_to(to)
  check_velocity_i(i, to)
  check_velocity_j(j, i)

  # normalization
  velocity <- as.integer(velocity)
  i <- if (!is.null(i)) as.integer(i) else NA_integer_
  j <- if (!is.null(j)) as.integer(j) else NA_integer_

  # construction
  velocity <- list(velocity = velocity, to = to, i = i, j = j)
  class(velocity) <- "Velocity"
  velocity
}


check_velocity_i <- function(i, to) {
  if (is.null(i)) return(invisible())
  if (is.null(to)) erify::throw("Can't set `i` when `to` is unspecified.")
  erify::check_n(i)
}


check_velocity_j <- function(j, i) {
  if (is.null(j)) return(invisible())
  if (is.null(i)) erify::throw("Can't set `j` when `i` is unspecified.")
  erify::check_n(j)
}


#' @export
print.Velocity <- function(x, ...) {
  velocity <- x$velocity
  to <- x$to
  i <- x$i
  j <- x$j

  cat("Velocity", velocity, "\n")
  if (!is.null(to)) cat("\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to

  if (!is.na(j)) {
    s_ij <- sprintf("(%s, %s)", i, j)
    cat("* to be applied to position", s_ij, "of Line", s_to)
  } else if (!is.na(i)) {
    cat("* to be applied to position", i, "of Line", s_to)
  } else if (!is.null(to)) {
    cat("* to be applied to Line", s_to)
  }
}
