#' @keywords internal
#' @export
to_MusicXML.Key <- function(x, ...) {
  MusicXML(
    "key",
    MusicXML("fifths", x[["key"]]),
    list(number = x[["staff"]])
  )
}
