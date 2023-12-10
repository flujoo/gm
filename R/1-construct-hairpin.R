#' @export
Hairpin <- function(symbol, i, j, to = NULL, text = NULL, above = NULL) {
  # validation
  erify::check_content(symbol, c("<", ">"))
  check_to(to)
  erify::check_n(i)
  erify::check_n(j)
  if (!is.null(text)) erify::check_string(text)
  if (!is.null(above)) erify::check_bool(above)

  # normalization
  i <- as.integer(i)
  j <- as.integer(j)
  if (is.null(text)) text <- NA_character_

  # construction
  hairpin <- list(
    to = to,
    i = i,
    j = j,
    symbol = symbol,
    text = text,
    above = above
  )
  class(hairpin) <- "Hairpin"
  hairpin
}


#' @export
print.Hairpin <- function(x, ...) {
  text <- x$text
  above <- x$above

  cat(switch(x$symbol, "<" = "Crescendo", ">" = "Diminuendo"), "\n\n")
  if (!is.na(text)) cat(sprintf('* as text "%s"', text), "\n")

  if (!is.null(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }

  print_to_i_j(x$to, x$i, x$j, line = TRUE)
}
