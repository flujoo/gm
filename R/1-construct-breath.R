#' Create `Breath` Object
#'
#' Create a `Breath` object to represent a breath mark.
#'
#' @param i A single positive integer, which represents the position
#' of the breath mark in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the breath mark.
#'
#' @param symbol Optional. A single character which can be `"comma"`,
#' `"tick"`, `"upbow"`, and `"salzedo"`. It represents the symbol
#' used for the breath mark. The default symbol is `"comma"`. See
#' [the MusicXML specification](`r to_url("data-types/breath-mark-value/")`).
#'
#' @returns A list of class `Breath`.
#'
#' @seealso [gm::+.Music()] for adding a breath mark to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a breath mark
#' breath <- Breath(1)
#' breath
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + breath
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
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
  print_to_i_j(x[["to"]], x[["i"]])
}
