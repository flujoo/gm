#' @export
Notehead <- function(to,
                     i,
                     j = NULL,
                     shape = NULL,
                     color = NULL,
                     filled = NULL,
                     bracketed = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  check_notehead_shape(shape)
  check_notehead_color(color)
  if (!is.null(filled)) erify::check_bool(filled)
  if (!is.null(bracketed)) erify::check_bool(bracketed)

  # normalization
  i <- as.integer(i)
  j <- if (is.null(j)) NA_integer_ else as.integer(j)
  if (is.null(shape)) shape <- NA_character_
  if (is.null(color)) color <- NA_character_
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
