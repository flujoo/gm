#' @keywords internal
#' @export
to_MusicXML.Notehead <- function(x, ...) {
  attributes <- list()

  color <- x[["color"]]
  if (!is.na(color)) attributes[["color"]] <- color

  filled <- x[["filled"]]
  if (!is.na(filled)) attributes[["filled"]] <- if (filled) "yes" else "no"

  bracket <- x[["bracket"]]
  if (!is.na(bracket)) attributes$parentheses <- if (bracket) "yes" else "no"

  MusicXML("notehead", x[["shape"]], attributes)
}
