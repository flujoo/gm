#' @export
Fermata <- function(to, i, shape = NULL, above = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  check_fermata_shape(shape)
  if (!is.null(above)) erify::check_bool(above)

  # normalization
  i <- as.integer(i)
  if (is.null(shape)) shape <- NA_character_
  if (is.null(above)) above <- NA

  # construction
  fermata <- list(to = to, i = i, shape = shape, above = above)
  class(fermata) <- "Fermata"
  fermata
}


check_fermata_shape <- function(shape) {
  if (is.null(shape)) return(invisible())

  valid <- c(
    "normal", "angled", "square", "double-angled",
    "double-square", "double-dot", "half-curve"
  )

  erify::check_content(shape, valid)
}


#' @export
print.Fermata <- function(x, ...) {
  to <- x$to
  i <- x$i
  shape <- x$shape
  above <- x$above

  cat("Fermata", "\n\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")
  cat("* to be added at position", i, "\n")

  if (!is.na(shape)) cat(sprintf('* of shape "%s"', shape), "\n")

  if (!is.na(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}
