#' @export
Fermata <- function(i, to = NULL, type = NULL, above = NULL) {
  # Validation
  erify::check_n(i)
  check_to(to)
  if (!is.null(type)) erify::check_content(type, unlist(fermatas))
  if (!is.null(above)) erify::check_bool(above)

  # Normalization
  i <- as.integer(i)
  if (is.null(type)) type <- NA_character_

  # Construction
  structure(
    list(to = to, i = i, type = type, above = above),
    class = "Fermata"
  )
}


fermatas <- rbind(
  data.frame(musescore = NA_character_, musicxml = "normal"),

  c("short"        , "angled"       ),
  c("long"         , "square"       ),
  c("very short"   , "double-angled"),
  c("very long"    , "double-square"),
  c("long (Henze)" , "double-dot"   ),
  c("short (Henze)", "half-curve"   )
)


#' @export
print.Fermata <- function(x, ...) {
  type <- x$type
  above <- x$above

  cat("Fermata", "\n\n")
  print_to_i_j(x$to, x$i)
  if (!is.na(type)) cat(sprintf('* of type "%s"', type), "\n")

  if (!is.null(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}
