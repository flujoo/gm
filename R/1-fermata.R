#' @export
Fermata <- function(to, i, type = NULL, above = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(type)) erify::check_content(type, unlist(fermatas))
  if (!is.null(above)) erify::check_bool(above)

  # normalization
  i <- as.integer(i)
  if (is.null(type)) type <- NA_character_

  # construction
  fermata <- list(to = to, i = i, type = type, above = above)
  class(fermata) <- "Fermata"
  fermata
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
  to <- x$to
  i <- x$i
  type <- x$type
  above <- x$above

  cat("Fermata", "\n\n")
  print_to_ij(to, i)
  if (!is.na(type)) cat(sprintf('* of type "%s"', type), "\n")

  if (!is.null(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}
