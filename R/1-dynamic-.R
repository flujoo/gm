#' @export
Dynamic <- function(marking = NULL,
                    velocity = NULL,
                    to = NULL,
                    i = NULL,
                    j = NULL,
                    scope = NULL) {
  # validation
  check_dynamic_marking(marking)
  if (!is.null(velocity)) erify::check_interval(velocity, c(0L, 127L))
  check_dynamic_marking_velocity(marking, velocity)
  if (!is.null(to)) check_to(to)
  check_dynamic_i(i, to)
  check_dynamic_j(j, i)
  check_dynamic_scope(scope, to, i, j)

  # normalization
  if (is.null(marking)) marking <- NA_character_
  velocity <- normalize_dynamic_velocity(velocity, marking)
  i <- if (!is.null(i)) as.integer(i) else NA_integer_
  j <- if (!is.null(j)) as.integer(j) else NA_integer_
  scope <- normalize_dynamic_scope(scope, to, i, j)

  # construction
  dynamic <- list(
    marking = marking,
    velocity = velocity,
    to = to,
    i = i,
    j = j,
    scope = scope
  )
  class(dynamic) <- "Dynamic"
  dynamic
}


#' @export
print.Dynamic <- function(x, ...) {
  marking <- x$marking
  velocity <- x$velocity
  to <- x$to
  i <- x$i
  j <- x$j
  scope <- x$scope

  if (is.na(marking)) marking <- ""
  cat("Dynamic", marking, "\n\n")
  cat("* of velocity", velocity, "\n")

  if (!is.null(to)) {
    cat(
      "* to be added to Line",
      if (is.character(to)) paste0('"', to, '"') else to,
      "\n"
    )
  }

  if (!is.na(j)) {
    cat("* to be added at position", paste0("(", i, ", ", j, ")"), "\n")
  } else if (!is.na(i)) {
    cat("* to be added at position", i, "\n")
  }

  if (scope == "note") {
    cat("* to be applied only to the note at the position", "\n")
  } else if (scope == "chord") {
    cat("* to be applied to the note(s) at position", i, "\n")
  } else if (scope %in% c("voice", "staff", "part")) {
    cat("* to be applied to the", scope, "containing the Line", "\n")
  } else if (scope == "score") {
    cat("* to be applied to the whole score", "\n")
  }
}
