#' @export
Lyric <- function(text, i, to = NULL, special = NULL, layer = NULL) {
  # Validation
  erify::check_type(text, "character")
  erify::check_n(i)
  check_to(to)
  if (!is.null(special)) erify::check_content(special, c("-", "_"))
  if (!is.null(layer)) erify::check_n(layer)

  # Normalization
  text <- normalize_lyric_text(text)
  i <- as.integer(i)
  if (is.null(special)) special <- NA_character_
  if (!is.null(layer)) layer <- as.integer(layer)
  j <- normalize_lyric_j(text)

  # Construction
  structure(
    list(
      to = to,
      layer = layer,
      i = i,
      j = j,
      text = text,
      special = special
    ),

    class = "Lyric"
  )
}


normalize_lyric_text <- function(text) {
  l <- length(text)

  if (l == 0) return(" ")
  text[is.na(text)] <- " "
  if (l == 1 && text == "") return(" ")

  text
}


normalize_lyric_j <- function(text) {
  l <- length(text)
  if (l == 1) NA_integer_ else 1:l
}


#' @export
print.Lyric <- function(x, ...) {
  special <- x$special
  layer <- x$layer

  cat("Lyric", sprintf('"%s"', paste(x$text, collapse = "‿")), "\n\n")

  if (!is.na(special)) {
    s_special <- switch(special,
      "-" = "to be connected with the next syllable",
      "_" = "as a melisma"
    )

    cat("*", s_special, "\n")
  }

  print_to_i_j(x$to, x$i)
  if (!is.null(layer)) cat("* to be added to layer", layer, "\n")
}
