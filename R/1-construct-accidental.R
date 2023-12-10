#' @export
Accidental <- function(name, i, j = NULL, to = NULL, bracket = NULL) {
  # Validation
  erify::check_content(name, accidentals)
  erify::check_n(i)
  check_to(to)
  if (!is.null(j)) erify::check_n(j)
  if (!is.null(bracket)) erify::check_bool(bracket)

  # Normalization
  i <- as.integer(i)
  j <- if (is.null(j)) NA_integer_ else as.integer(j)
  if (is.null(bracket)) bracket <- NA

  # Construction
  structure(
    list(to = to, i = i, j = j, name = name, bracket = bracket),
    class = "Accidental"
  )
}


accidentals <- c(
  # Not supported:
  "sharp-sharp", "natural-sharp", "natural-flat",
  "sharp-1", "sharp-2", "sharp-3", "sharp-5",
  "flat-1", "flat-2", "flat-3", "flat-4",

  # Only in Finale:
  "triple-sharp", "triple-flat",

  "sharp", "natural", "flat",
  "double-sharp", "flat-flat",
  "quarter-flat", "quarter-sharp",
  "three-quarters-flat", "three-quarters-sharp",
  "sharp-down", "sharp-up",
  "natural-down", "natural-up",
  "flat-down", "flat-up",
  "double-sharp-down", "double-sharp-up",
  "flat-flat-down", "flat-flat-up",
  "arrow-down", "arrow-up",
  "slash-quarter-sharp", "slash-sharp",
  "slash-flat", "double-slash-flat",
  "sori", "koron"
)


#' @export
print.Accidental <- function(x, ...) {
  cat("Accidental", "\n\n")
  cat("*", x$name, "\n")
  if (isTRUE(x$bracket)) cat("* enclosed in brackets", "\n")
  print_to_i_j(x$to, x$i, x$j)
}
