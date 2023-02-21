#' @export
Notehead <- function(i,
                     j = NULL,
                     to = NULL,
                     shape = NULL,
                     color = NULL,
                     filled = NULL,
                     bracketed = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  if (!is.null(shape)) erify::check_content(shape, noteheads)
  check_color(color)
  if (!is.null(filled)) erify::check_bool(filled)
  if (!is.null(bracketed)) erify::check_bool(bracketed)

  # normalization
  i <- as.integer(i)
  j <- if (is.null(j)) NA_integer_ else as.integer(j)
  if (is.null(shape)) shape <- NA_character_
  color <- if (is.null(color)) NA_character_ else toupper(color)
  if (is.null(filled)) filled <- NA
  if (is.null(bracketed)) bracketed <- NA

  # construction
  notehead <- list(
    to = to,
    i = i,
    j = j,
    shape = shape,
    color = color,
    filled = filled,
    bracketed = bracketed
  )
  class(notehead) <- "Notehead"
  notehead
}


noteheads <- c(
  "normal", "diamond",
  "x", "cross", "circle-x",
  "triangle", "inverted triangle",
  "slash", "slashed", "back slashed",
  "do", "re", "mi", "fa", "so", "la", "ti"
)


check_color <- function(color) {
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


#' @export
print.Notehead <- function(x, ...) {
  shape <- x$shape
  color <- x$color

  cat("Notehead", "\n\n")
  if (!is.na(shape)) cat(sprintf('* of shape "%s"', shape), "\n")
  if (!is.na(color)) cat(sprintf('* of color "%s"', color), "\n")
  if (isTRUE(x$filled)) cat("* filled", "\n")
  if (isTRUE(x$bracketed)) cat("* bracketed", "\n")
  print_to_ij(x$to, x$i, x$j)
}
