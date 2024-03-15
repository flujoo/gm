Breath <- function(i, to = NULL, symbol = NULL) {
  # Validation
  erify::check_n(i)
  check_to(to)

  symbols <- c("comma", "tick", "upbow", "salzedo")
  if (!is.null(symbol)) erify::check_content(symbol, symbols)

  # Normalization
  i <- as.integer(i)
  if (is.null(symbol)) symbol <- NA_character_

  # Construction
  structure(
    list(to = to, i = i, symbol = symbol),
    class = "Breath"
  )
}


#' @export
print.Breath <- function(x, ...) {
  symbol <- x[["symbol"]]

  cat("Breath Mark", "\n\n")
  if (!is.na(symbol)) cat(sprintf('* of symbol "%s"', symbol), "\n")
  print_to_i_j(x$to, x$i)
}
