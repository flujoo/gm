#' @export
Dynamic <- function(symbol = NULL,
                    velocity = NULL,
                    to = NULL,
                    i = NULL,
                    j = NULL,
                    scope = NULL) {
  # validation
  check_dynamic_symbol(symbol)
  if (!is.null(velocity)) erify::check_interval(velocity, c(0L, 127L))
  check_dynamic_symbol_velocity(symbol, velocity)
  if (!is.null(to)) check_to(to)
  check_dynamic_i(i, to)
  check_dynamic_j(j, i)
  check_dynamic_scope(scope, to, i, j)

  # normalization
  velocity <- normalize_dynamic_velocity(velocity, symbol)
  scope <- normalize_dynamic_scope(scope, to, i, j)

  # construction
  dynamic <- list(
    symbol = symbol,
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
  symbol <- x$symbol
  velocity <- x$velocity
  to <- x$to
  i <- x$i
  j <- x$j
  scope <- x$scope

  cat("Dynamic", symbol, "\n\n")
  cat("* of velocity", velocity, "\n")

  if (!is.null(to)) {
    cat(
      "* to be added to Line",
      if (is.character(to)) paste0('"', to, '"') else to,
      "\n"
    )
  }

  if (!is.null(j)) {
    cat("* to be added at position", paste0("(", i, ", ", j, ")"), "\n")
  } else if (!is.null(i)) {
    cat("* to be added at position", i, "\n")
  }

  if (is.null(scope)) {

  } else if (scope == "note") {
    cat("* to be applied only to the note at the position", "\n")
  } else if (scope == "chord") {
    cat("* to be applied to the note(s) at position", i, "\n")
  } else if (scope %in% c("voice", "staff", "part")) {
    cat("* to be applied to the", scope, "containing the Line", "\n")
  } else if (scope == "score") {
    cat("* to be applied to the whole score", "\n")
  }
}
