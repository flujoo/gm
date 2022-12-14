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

  general <- paste(
    "`color` must be represented in",
    "the hexadecimal RGB or ARGB format."
  )

  specifics <- sprintf('`color` is "%s".', color)
  erify::throw(general, specifics)
}
