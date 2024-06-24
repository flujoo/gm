#' @keywords internal
#' @export
to_MusicXML.Clef <- function(x, ...) {
  contents <- list(
    MusicXML("sign", x[["sign"]]),
    MusicXML("line", x[["clef_line"]])
  )

  octave <- x[["octave"]]

  if (!is.na(octave)) {
    contents <- c(contents, list(MusicXML("clef-octave-change", octave)))
  }

  attributes <- list(number = x[["staff"]])
  MusicXML("clef", contents, attributes)
}
