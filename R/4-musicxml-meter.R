#' @keywords internal
#' @export
to_MusicXML.Meter <- function(x, ...) {
  contents <- list(
    MusicXML("beats", x[["number"]]),
    MusicXML("beat-type", x[["unit"]])
  )

  attributes <- if (x[["invisible"]]) list(`print-object` = "no") else NULL
  MusicXML("time", contents, attributes)
}


