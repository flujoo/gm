#' @keywords internal
#' @export
to_MusicXML.Hairpin <- function(x, type, ...) {
  symbol <- switch(x[["symbol"]], "<" = "crescendo", ">" = "diminuendo")
  if (type == "start") type <- symbol
  placement <- if (x[["above"]]) "above" else "below"

  musicxml <- MusicXML("wedge", NULL, list(type = type, number = x$number))
  musicxml <- MusicXML("direction-type", musicxml)
  MusicXML("direction", musicxml,  list(placement = placement))
}
