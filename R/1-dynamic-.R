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
