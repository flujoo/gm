#' @keywords internal
#' @export
to_MusicXML.Fermata <- function(x, ...) {
  MusicXML(
    "fermata",
    fermatas[["musicxml"]][which(fermatas == x[["shape"]], TRUE)[1]],
    list(type = if (x[["above"]]) "upright" else "inverted")
  )
}
