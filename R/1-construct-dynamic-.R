#' @export
Dynamic <- function(marking, to, i, velocity = NULL, above = NULL) {
  # validation
  erify::check_string(marking)
  check_to(to)
  erify::check_n(i)

  if (!is.null(velocity) && !is.na(velocity)) {
    erify::check_interval(velocity, c(0L, 127L))
  }

  if (!is.null(above)) erify::check_bool(above)

  # normalization
  i <- as.integer(i)
  velocity <- normalize_dynamic_velocity(velocity, marking)

  # construction
  dynamic <- list(
    to = to,
    i = i,
    marking = marking,
    velocity = velocity,
    above = above
  )
  class(dynamic) <- "Dynamic"
  dynamic
}


normalize_dynamic_velocity <- function(velocity, marking) {
  if (!is.null(velocity)) return(as.integer(velocity))

  markings <- dynamics$marking

  if (marking %in% markings) {
    (dynamics$velocity)[which(markings == marking)]
  } else {
    NA_integer_
  }
}


#' @export
print.Dynamic <- function(x, ...) {
  velocity <- x$velocity
  above <- x$above

  cat("Dynamic", sprintf('"%s"', x$marking), "\n\n")
  if (!is.na(velocity)) cat("* of velocity", velocity, "\n")
  print_to_ij(x$to, x$i)

  if (!is.null(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}
