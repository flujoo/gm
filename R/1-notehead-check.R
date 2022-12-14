check_notehead_color <- function(color) {
  if (is.null(color)) return(invisible())
  erify::check_string(color)

  re_hex <- "(\\d|[a-f]|[A-F])"

  re <- paste0(
    "^", "#",
    re_hex, "{6}",
    "(", re_hex, re_hex, ")?",
    "$"
  )

  if (grepl(re, color)) return(invisible())
  general <- "`color` must be represented as hexadecimal RGB or ARGB values."
  specifics <- sprintf('`color` is "%s".', color)
  erify::throw(general, specifics)
}
