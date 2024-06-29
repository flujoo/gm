#' Create `Hairpin` Object
#'
#' Create a `Hairpin` object to represent a crescendo or diminuendo symbol.
#'
#' @param symbol A single character, which can be `"<"` or `">"`. They
#' represent crescendo and diminuendo respectively.
#'
#' @param i,j A single positive integer. They indicate the start
#' and end position of the `Hairpin` object in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the `Hairpin` object.
#'
#' @param above Optional. A single logical, which indicates whether the
#' `Hairpin` object should appear above or below the staff.
#'
#' @returns A list of class `Hairpin`.
#'
#' @seealso [gm::+.Music()] for adding a `Hairpin` to
#' a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a crescendo
#' crescendo <- Hairpin("<", 1, 3)
#' crescendo
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4", "E4")) + crescendo
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Hairpin <- function(symbol, i, j, to = NULL, above = NULL) {
  # Validation
  erify::check_content(symbol, c("<", ">"))
  erify::check_n(i)
  erify::check_n(j)
  check_to(to)
  if (!is.null(above)) erify::check_bool(above)

  # Normalization
  i <- as.integer(i)
  j <- as.integer(j)

  # Construction
  structure(
    list(
      to = to,
      i = i,
      j = j,
      symbol = symbol,
      above = above
    ),

    class = "Hairpin"
  )
}


#' @export
print.Hairpin <- function(x, ...) {
  above <- x$above

  cat(switch(x$symbol, "<" = "Crescendo", ">" = "Diminuendo"), "\n\n")

  if (!is.null(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }

  print_to_i_j(x$to, x$i, x$j, line = TRUE)
}
